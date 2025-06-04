#TWEETS SENTIMENT ANALYSIS WITH VADER

#Contributor: Min Kim

#Description: Created a table containing politicians' usernames, tweets, date of the tweets
#             and the sentiment (score and direction) of the tweets. Them merged the table with the CA State
#             and CA Assembly data frames to see each politicians' average sentiment score.


rm(list=ls())

install.packages(c("vader", "tidyverse", "scales", "data.table"))
library(vader)
library(tidyverse)
library(scales)
library(data.table)

# Load the tweet data
california_politicians_tweets_FIXED <- read_csv("california_politicians_tweets_FIXED.csv")
View(california_politicians_tweets_FIXED)

# Isolate the content column
Content <- tibble(california_politicians_tweets_FIXED$content)

# Apply get_vader() to each sentence to extract the sentiment data
# get_vader() will return both word-level and overall sentiment analysis
vader_results <- Content %>% 
  mutate(vader_output = map(california_politicians_tweets_FIXED$content, ~ get_vader(.x)))

# Extract relevant components: word scores, compound, pos, neu, neg, and but count
vader_results <- vader_results %>%
  mutate(
    word_scores = map(vader_output, ~ .x[names(.x) != "compound" & 
                                           names(.x) != "pos" & 
                                           names(.x) != "neu" & 
                                           names(.x) != "neg" & 
                                           names(.x) != "but_count"]),  # Extract word-level scores
    compound = map_dbl(vader_output, ~ as.numeric(.x["compound"])),
    pos = map_dbl(vader_output, ~ as.numeric(.x["pos"])),
    neu = map_dbl(vader_output, ~ as.numeric(.x["neu"])),
    neg = map_dbl(vader_output, ~ as.numeric(.x["neg"])),
    but_count = map_dbl(vader_output, ~ as.numeric(.x["but_count"])),
    sentiment = case_when(
      compound > 0.05 ~ "positive",
      compound < -0.05 ~ "negative",
      TRUE ~ "neutral") 
) 


# Rename the first column to content
names(vader_results)[1] <- "content"  

# create a row number column to keep track of the original order
vader_results$row_number <- seq_len(nrow(vader_results))


#SENATE TWEET SENTIMENT ANALYSIS  
#join the vader results with the original tweets data
tweets <- unique(left_join(california_politicians_tweets_FIXED, vader_results, by = "content")) 

# Create a summary table with the average sentiment score and direction for each politician
tweets_summary <- tapply(tweets$compound, tweets$username, mean, na.rm = TRUE)

# Create a data frame with the tweets and their sentiment
tweets_table <- as.data.frame(tweets_summary)

# Create a new table by extracting the mean score of sentiment direction from tweets_final 
# if u noticed, the username "column" isn't really a column, so I have to make a new column with same values
tweets_data <- tweets_table %>%
  mutate(username = rownames(tweets_table))

# Rename the columns before merging tables
tweets_final <- tweets_data %>% rename(sentiment_avg = tweets_summary, twitter = username)

# MERGED TABLE WITH SENATE
CA_State_Final_Updated <- read_csv("CA_State/CA_State_Combined/CA_State_Final_Updated.csv")
View(CA_State_Final_Updated)

# Get rid of the @ in the CA_state_Final_Updated data frame
CA_State_Final_Updated$twitter <- gsub("@", "", CA_State_Final_Updated$twitter)

# Merge the tweets summary with the CA_State_Final_Updated data frame
Merged_Senate <- left_join(CA_State_Final_Updated, tweets_final, by = "twitter")

Merged_Senate_updated <- Merged_Senate |>
  mutate(sentiment = case_when(
  sentiment_avg > 0.05 ~ "positive",
  sentiment_avg < -0.05 ~ "negative",
  TRUE ~ "neutral"))


#fwrite(Merged_Senate, file = "VADER_Senate_avg_sentiment.csv")


#-----------------------------------------------#


install.packages(c("tidytext", "wordcloud", "dplyr", "ggplot2"))
#Visualizing the data
library(tidytext)
library(wordcloud)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Histogram of compound scores
histogram_vader <- Merged_Senate_updated |>
  ggplot(aes(x = sentiment_avg, fill = sentiment)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  labs(title = "VADER Compound Sentiment Scores",
       x = "Compound Score",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red", "neutral" = "gray"))
print(histogram_vader)

party_vader <- Merged_Senate_updated |>
  ggplot(aes(x = sentiment_avg, fill = sentiment)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  labs(title = "VADER Compound Sentiment Scores",
       x = "Compound Score",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red", "neutral" = "gray")) + 
  facet_wrap(~ party, scales = "free_y") # Facet by party for better comparison
print(party_vader)


#TAKE OUT SOME OF THE WORDS: HTTPS, T.CO, NUMBERS

##---Word Frequency Analysis---##
#Add in the politicians' parties to the tweet data table
CA_State_Final_Updated <- read_csv("CA_State/CA_State_Combined/CA_State_Final_Updated.csv")

tweets_renamed <- tweets %>% 
  rename(twitter = username)

# Merge the tweets with the CA_State_Final_Updated to get party information
tweets_last <- right_join(tweets_renamed, CA_State_Final_Updated, by = "twitter") 

tweet_data_words <- tweets_last %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) |>
  filter(
    !str_detect(word, "^[0-9]"),
    !word %in% c("https", "t.co")
    ) # Remove words that start with a number

#Plot 10 most common words
tweet_data_words_barplot <- tweet_data_words %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Common Words in Tweets",
       x = "Word",
       y = "Count") +
  theme_minimal() 
print(tweet_data_words_barplot)


#table of most common words by party
tweet_data_words_party <- tweets_last %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE, .by = party)|>
  slice_max(n, n = 11, by = .by) %>% 
  filter(
    !str_detect(word, "^[0-9]"),
    !word %in% c("https", "t.co")) # Remove words


#Creating a word cloud for the most common words
install.packages("reshape2")
library(reshape2)


word_cloud <- tweet_data_words %>%
  with(wordcloud(word, n, max.words = 100))

#Word Cloud Comparison
word_cloud_comparison <- tweet_data_words_party %>%
  acast(word ~ .by, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


# Step 1: Prepare party-specific word counts
tweet_data_words_party <- tweets_last %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, by = "word") %>%
  count(party, word, sort = TRUE) %>%
  group_by(party) %>%
  slice_max(n, n = 100) %>%  # use top 100 per party
  filter(
    !str_detect(word, "^[0-9]"),
    !word %in% c("https", "t.co")) %>% # Remove words
  ungroup()

# Step 2: Reshape for comparison.cloud (matrix format: word ~ party)
word_matrix <- tweet_data_words_party %>%
  acast(word ~ party, value.var = "n", fill = 0)

# Step 3: Create comparison cloud
comparison.cloud(
  word_matrix,
  max.words = 100,
  colors = c("blue3", "firebrick3"),
  title.size = 1.5,
  scale = c(3, 0.5),
  random.order = FALSE
)

#Cleaning up word deck
install.packages(c("textstem", "SnowballC", "reshape2", "textdata", "tidytext"))
library(tidyverse)
library(textstem)
library(SnowballC)
library(reshape2)
library(textdata)
library(tidytext)



head(get_sentiments("bing"))

bing_words <- tweets_last %>%
  unnest_tokens(word, content) %>%
  mutate(word = lemmatize_words(word)) |>
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment.y, sort = TRUE)

# 2. Reshape to word ~ sentiment matrix
bing_matrix <- bing_words %>%
  acast(word ~ sentiment.y, value.var = "n", fill = 0)

# 3. Plot comparison cloud: Positive vs Negative
comparison.cloud(
  bing_matrix,
  max.words = 100,
  colors = c("firebrick3", "steelblue3"),  # red = negative, blue = positive
  title.size = 1.5,
  scale = c(3, 0.5),
  random.order = FALSE
)

#Can we do this for republicans vs democrats? 
# 1. Prepare party-specific sentiment counts
bing_party_words <- tweets_last %>%
  unnest_tokens(word, content) %>%
  mutate(word = lemmatize_words(word)) |>
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(party, word, sentiment.y, sort = TRUE) %>%
  group_by(party, sentiment.y) %>%
  slice_max(n, n = 100) %>%  # use top 100 per party and sentiment
  ungroup()

#commparison word cloud
# 2. Reshape for comparison.cloud (matrix format: word ~ party + sentiment)
bing_party_matrix <- bing_party_words %>%
  acast(word ~ party + sentiment.y, value.var = "n", fill = 0)

# 3. Create comparison cloud
comparison.cloud(
  bing_party_matrix,
  max.words = 100,
  colors = c("firebrick3", "steelblue3", "darkorange", "darkgreen"),  # red = negative, blue = positive
  title.size = 1.5,
  scale = c(3, 0.5),
  random.order = FALSE
)

#How to calculate word correlation 

# Load the lexicon
lexicon <- lexicon::hash_sentiment_jockers_rinker
# Calculate word correlations
word_correlation <- tweets_last %>%
  unnest_tokens(word, content) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(correlation = cor(positive, negative)) %>%
  arrange(desc(correlation))

tweet_tokens <- tweets_last %>%
  mutate(year = year(date)) %>%                     # Extract year from date
  unnest_tokens(word, content) %>%                # Tokenize
  anti_join(stop_words, by = "word") %>%            # Remove stop words
  filter(str_detect(word, "[a-z]"))                 # Keep only valid words

# --- Step 2: Calculate TF-IDF by Party-Year --- #
tfidf_party_year <- tweet_tokens %>%
  count(party, year, word, sort = TRUE) %>%
  mutate(doc_id = interaction(party, year)) %>%
  bind_tf_idf(word, doc_id, n) %>%
  group_by(party, year) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup()

ggplot(tfidf_party_year, aes(tf_idf, reorder_within(word, tf_idf, interaction(party, year)), fill = party)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ party + year, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Most Distinctive Words by Party Over Time (TF-IDF)",
    x = "TF-IDF Score", y = NULL
  ) +
  theme_minimal()

















