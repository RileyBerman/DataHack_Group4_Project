install.packages("vader")
library(vader)
library(dplyr)
library(readr)

#Not available for this version of R
#library(knitrr)
#library(kabelExtra)

#Not available for this version of R
#for tidyvader (quicker runtime)
install.packages("tidyvader")
#devtools::install_github("chris31415926535/tidyvader")

#-----------------------------------------------#
#Load dataset: create a speech_id variable for each press release
press_data <- read_csv("press_releases.csv") %>%
  mutate(speech_id = seq_along(full_text))

#Separating into sentences
press_data_wider <- press_data |> separate_rows(full_text, sep = "\n") |>
  #Getting rid of extra space
  mutate(full_text = str_squish(full_text)) |>
  filter(full_text != "") |>
  mutate(full_text = iconv(full_text, from = "UTF-8", to = "ASCII", sub = "")) |>
  mutate(full_text = str_replace_all(full_text, "-", ""))

# Run VADER on full_text
vader_scores <- vader_df(press_data_wider$full_text)

#Getting sentiment scores for each line
sentiment_vader <- press_data_wider %>%
  mutate(compound = vader_scores$compound,
         pos = vader_scores$pos,
         neg = vader_scores$neg,
         neu = vader_scores$neu) 

#Recombining the sentiment scores with the original data
sentiment_vader_updated <- sentiment_vader |>
  summarise(
    compound = mean(compound, na.rm = TRUE), 
    pos = mean(pos, na.rm = TRUE),
    neg = mean(neg, na.rm = TRUE),
    neu = mean(neu, na.rm = TRUE),
    .by = c(legislator_name, speech_id, title, date, party)) |>
  mutate(sentiment = case_when(
          compound > 0.05 ~ "positive",
         compound < -0.05 ~ "negative",
         TRUE ~ "neutral")) 

#-----------------------------------------------#
#Visualizing the data
library(tidytext)
library(wordcloud)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Histogram of compound scores
histogram_vader <- sentiment_vader_updated |>
  ggplot(aes(x = compound, fill = sentiment)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  labs(title = "VADER Compound Sentiment Scores",
       x = "Compound Score",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red", "neutral" = "gray"))

party_vader <- sentiment_vader_updated |>
  ggplot(aes(x = compound, fill = sentiment)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.7) +
  labs(title = "VADER Compound Sentiment Scores",
       x = "Compound Score",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red", "neutral" = "gray")) + 
  facet_wrap(~ party, scales = "free_y") # Facet by party for better comparison


#Plot compound over time
avg_sentiment_by_date_vader <- sentiment_vader_updated %>%
  ggplot(aes(x = date, y = compound, color = party)) + 
  geom_line(size = 1.2, alpha = 0.8) 

compound_by_senator <- sentiment_vader_updated %>%
  summarise(avg_compound = mean(compound, na.rm = TRUE), 
            .by = c(legislator_name, date)) %>%
  ggplot(aes(x = date, y = avg_compound, color = legislator_name)) + 
  geom_line()


#-----------------------------------------------#
##---Word Frequency Analysis---##

press_data_words <-press_data %>%
  unnest_tokens(word, full_text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) |>
  filter(word != str_detect(word, "^[0-9]")) # Remove words that start with a number

#Plot 10 most common words
press_data_words_barplot <- press_data_words %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Common Words in Press Releases",
       x = "Word",
       y = "Count") +
  theme_minimal() 

press_data_words_party <-press_data %>%
  unnest_tokens(word, full_text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE, .by = party) |>
  slice_max(n, n = 10, by = .by)

#Creating a word cloud for the most common words
library(reshape2)

word_cloud <- press_data_words %>%
  with(wordcloud(word, n, max.words = 100))

#Word Cloud Comparison
word_cloud_comparison <- press_data_words_party %>%
  acast(word ~ .by, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
  
# Step 1: Prepare party-specific word counts
press_data_words_party <- press_data %>%
  unnest_tokens(word, full_text) %>%
  anti_join(stop_words, by = "word") %>%
  count(party, word, sort = TRUE) %>%
  group_by(party) %>%
  slice_max(n, n = 100) %>%  # use top 100 per party
  ungroup()

# Step 2: Reshape for comparison.cloud (matrix format: word ~ party)
word_matrix <- press_data_words_party %>%
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

library(tidyverse)
library(textstem)
library(SnowballC)
library(reshape2)
library(textdata)
library(tidytext)

bing_words <-  press_data %>%
  unnest_tokens(word, full_text) %>%
  mutate(word = lemmatize_words(word)) |>
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE)

# 2. Reshape to word ~ sentiment matrix
bing_matrix <- bing_words %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)

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
bing_party_words <- press_data %>%
  unnest_tokens(word, full_text) %>%
  mutate(word = lemmatize_words(word)) |>
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(party, word, sentiment, sort = TRUE) %>%
  group_by(party, sentiment) %>%
  slice_max(n, n = 100) %>%  # use top 100 per party and sentiment
  ungroup()

#commparison word cloud
# 2. Reshape for comparison.cloud (matrix format: word ~ party + sentiment)
bing_party_matrix <- bing_party_words %>%
  acast(word ~ party + sentiment, value.var = "n", fill = 0)

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
word_correlation <- press_data %>%
  unnest_tokens(word, full_text) %>%
  inner_join(lexicon, by = c("word" = "x")) %>%
  count(word, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(correlation = cor(positive, negative)) %>%
  arrange(desc(correlation))

press_tokens <- press_data %>%
  mutate(year = year(date)) %>%                     # Extract year from date
  unnest_tokens(word, full_text) %>%                # Tokenize
  anti_join(stop_words, by = "word") %>%            # Remove stop words
  filter(str_detect(word, "[a-z]"))                 # Keep only valid words

# --- Step 2: Calculate TF-IDF by Party-Year --- #
tfidf_party_year <- press_tokens %>%
  count(party, year, word, sort = TRUE) %>%
  bind_tf_idf(word, interaction(party, year), n) %>%
  group_by(party, year) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup()

# --- Step 3: Plot TF-IDF Word Differences Over Time --- #
# Make sure tidytext::reorder_within() and scale_y_reordered() are available
ggplot(tfidf_party_year, aes(tf_idf, reorder_within(word, tf_idf, interaction(party, year)), fill = party)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ party + year, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Most Distinctive Words by Party Over Time (TF-IDF)",
    x = "TF-IDF Score", y = NULL
  ) +
  theme_minimal()










