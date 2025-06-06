install.packages("vader")
install.packages("tidyr")
install.packages("stringr")
install.packages("purrr")
library(vader)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

#Not available for this version of R
#library(knitrr)
#library(kabelExtra)

#Not available for this version of R
#for tidyvader (quicker runtime)
#install.packages("tidyvader")
#devtools::install_github("chris31415926535/tidyvader")

#-----------------------------------------------#
# Load dataset: create a speech_id variable for each press release
press_data <- read_csv("press_releases.csv") %>%
  mutate(speech_id = seq_along(full_text))
# Separating into sentences
press_data_wider <- press_data |> separate_rows(full_text, sep = "\n") |>
  #Getting rid of extra space
  mutate(full_text = str_squish(full_text)) |>
  filter(full_text != "") |>
  mutate(full_text = iconv(full_text, from = "UTF-8", to = "ASCII", sub = "")) |>
  mutate(full_text = str_replace_all(full_text, "-", ""))

# Run VADER on full_text using reticulate
library(reticulate)
vader <- import("vaderSentiment.vaderSentiment")
analyzer <- vader$SentimentIntensityAnalyzer()

# Sentiment score of the full_text
vader_scores <- press_data_wider |>
  mutate(result = map(full_text, ~ analyzer$polarity_scores(.x))) |>
  mutate(
    compound = map_dbl(result, "compound"),
    pos = map_dbl(result, "pos"),
    neu = map_dbl(result, "neu"),
    neg = map_dbl(result, "neg"))

# Recombining the sentiment scores with the original data
# DATA WITH ZEROS INCLUDED
sentiment_vader_updated <- vader_scores |>
  summarise(
    compound2 = mean(compound, na.rm = TRUE),
    pos = mean(pos, na.rm = TRUE),
    neg = mean(neg, na.rm = TRUE),
    neu = mean(neu, na.rm = TRUE),
    .by = c(legislator_name, speech_id, title, date, party)) |>
  mutate(sentiment = case_when(
          compound > 0.05 ~ "positive",
         compound < -0.05 ~ "negative",
         TRUE ~ "neutral")) 
# DATA WITHOUT ZEROS
sentiment_vader_updated2 <- vader_scores |>
  filter(compound != 0) |>
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

#------Testing the code (disregard this)
test <- sentiment_vader[c(1, 2, 3, 4, 5, 6), ]
test_update <- test |>
  filter(compound != 0) |>
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

test_update2 <- test |>
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
  


#------Histograms

install.packages(c("tidytext", "wordcloud", "tidyverse", "ggplot2"))
library(tidytext)
library(wordcloud)
library(tidyverse)
library(ggplot2)

# Histogram of compound scores and their frequency. more positive than negative
histogram_vader <- sentiment_vader_updated |>
  ggplot(aes(x = compound, fill = sentiment)) +
  geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.5)) +
  scale_fill_manual(values = c("positive" = "#2ca02c", "negative" = "#d62728", "neutral" = "#7f7f7f")) +
  labs(title = "VADER Compound Sentiment Scores",
       x = "Compound Score",
       y = "Frequency",
       fill = "Sentiment") +
  theme_minimal() +
  theme(text = element_text(size = 14))
print(histogram_vader)

# two histogram representing each party's compound score & frequency
party_vader <- sentiment_vader_updated |>
  ggplot(aes(x = compound, fill = sentiment)) +
  geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.5)) +
  scale_fill_manual(values = c(
    positive = "#2ca02c",
    negative = "#d62728",
    neutral = "#7f7f7f"
  )) +
  facet_wrap(~ party, scales = "free_y") +
  labs(
    title = "VADER Compound Sentiment Scores by Party",
    x = "Compound Score",
    y = "Frequency",
    fill = "Sentiment"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(size = 14))
print(party_vader)



# Plot compound over time. This part doesn't really matter
avg_sentiment_by_date_vader <- sentiment_vader_updated %>%
  ggplot(aes(x = date, y = compound, color = party)) + 
  geom_line(size = 1.2, alpha = 0.8) 
print(avg_sentiment_by_date_vader)

compound_by_senator <- sentiment_vader_updated %>%
  summarise(avg_compound = mean(compound, na.rm = TRUE), 
            .by = c(legislator_name, date)) %>%
  ggplot(aes(x = date, y = avg_compound, color = legislator_name)) + 
  geom_line()
print(compound_by_senator)


#------Word Frequency Analysis

# Get the most common words in the press releases and the count of their usage
press_data_words <- press_data %>%
  unnest_tokens(word, full_text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  filter(!str_detect(word, "^[0-9]"),
         !str_detect(word, "[[:punct:]]"),
         nchar(word) > 2)  

# Plot 10 most common words
press_data_words_barplot <- press_data_words %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Common Words in Press Releases",
       x = "Word",
       y = "Count") +
  theme_minimal() 
print(press_data_words_barplot)



#-------Word Cloud Comparison
press_data_words_party <- press_data %>%
  unnest_tokens(word, full_text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, party, sort = TRUE) %>%
  filter(
    !str_detect(word, "^[0-9]"),
    !str_detect(word, "[[:punct:]]"),
    nchar(word) > 2
  ) %>%
  group_by(party) %>%
  slice_max(n, n = 100) %>%
  ungroup()

#FIRST METHOD TO CREATE WORDCLOUD
# Word cloud for the most common words
library(reshape2)
word_cloud <- press_data_words %>%
  with(wordcloud(word, n, max.words = 100))

# Create a comparison cloud for party-specific words 
word_cloud_comparison <- press_data_words_party %>%
  reshape2::acast(word ~ party, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("blue3", "firebrick3"),
    max.words = 100,
    title.size = 1.5,
    random.order = FALSE,
    scale = c(2.6, 0.6)
  )

# SECOND METHOD TO CREATE A COMPARISON CLOUD
# Reshape for comparison.cloud (matrix format: word ~ party)
word_matrix <- press_data_words_party %>%
  acast(word ~ party, value.var = "n", fill = 0)

# Create comparison cloud based on parties
comparison.cloud(
  word_matrix,
  max.words = 80,
  colors = c("blue3", "firebrick3"),
  title.size = 1.5,
  scale = c(3, 0.5),
  random.order = FALSE
)

#-----Wordclouds using BING sentiment lexicon

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

#Reshape to word ~ sentiment matrix
bing_matrix <- bing_words %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)

# Party-specific sentiment counts
bing_party_words <- press_data %>%
  unnest_tokens(word, full_text) %>%
  mutate(word = lemmatize_words(word)) |>
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(party, word, sentiment, sort = TRUE) %>%
  group_by(party, sentiment) %>%
  slice_max(n, n = 100) %>%  # use top 100 per party and sentiment
  ungroup()

# Comparison word cloud
# 2Reshape for comparison.cloud (matrix format: word ~ party + sentiment)
bing_party_matrix <- bing_party_words %>%
  acast(word ~ party + sentiment, value.var = "n", fill = 0)

# Create comparison cloud splitting by party and sentiment
comparison.cloud(
  bing_party_matrix,
  max.words = 100,
  colors = c("firebrick3", "steelblue3", "darkorange", "darkgreen"),  # red = negative, blue = positive
  title.size = 1,
  scale = c(2.5, 0.6),
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


# Calculate correlation between positive and negative counts across all words
correlation_value <- cor(word_counts$positive, word_counts$negative)
print(correlation_value)

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










