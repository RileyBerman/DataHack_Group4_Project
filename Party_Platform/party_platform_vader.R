rm(list = ls())

install.packages(c("reticulate", "dplyr","vader","tidyr","stringr","tidyverse",
                   "tidytext", "reshape2","wordcloud"))
library(tidytext)
library(vader)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tidyverse)
library(wordcloud)


# Load dataset: create a speech_id variable for each press release
platform_data <- read_csv("Party_Platform/Party_Platform_Combined/Party_Platform_Final.csv") %>% 
  mutate(speech_id = seq_along(text))
# Separating into sentences
platform_data_wider <- platform_data |> separate_rows(text, sep = "\n") |>
  #Getting rid of extra space
  mutate(text = str_squish(text)) |>
  filter(text != "") |>
  mutate(text = iconv(text, from = "UTF-8", to = "ASCII", sub = "")) |>
  mutate(text = str_replace_all(text, "-", ""))

# Run VADER on full_text using reticulate
library(reticulate)
vader <- import("vaderSentiment.vaderSentiment")
analyzer <- vader$SentimentIntensityAnalyzer()

# Sentiment score of the full_text
vader_scores <- platform_data_wider |>
  mutate(result = map(text, ~ analyzer$polarity_scores(.x))) |>
  mutate(
    compound = map_dbl(result, "compound"),
    pos = map_dbl(result, "pos"),
    neu = map_dbl(result, "neu"),
    neg = map_dbl(result, "neg"))

#words used + frequency
plat_data_words <- platform_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  filter(!str_detect(word, "^[0-9]"),
         !str_detect(word, "[[:punct:]]"),
         nchar(word) > 2)  




#-------Word Cloud Comparison
# Words to take out
words_to_remove <- c("democrats", "democratic", "republican", "republicans")
pattern <- paste0("^(", paste(words_to_remove, collapse = "|"), ")$")

# Common words used by each party
plat_data_words_party <- platform_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, pattern)) %>%  # Remove specified words
  count(word, party, sort = TRUE) %>%
  filter(
    !str_detect(word, "^[0-9]"),
    !str_detect(word, "[[:punct:]]"),
    nchar(word) > 2
  ) %>%
  group_by(party) %>%
  slice_max(n, n = 100) %>%
  ungroup()

#Create a new table with just the democrat and republican parties
plat_dem_rep <- plat_data_words_party %>%
  filter(party %in% c("Democratic", "Republican"))

#FIRST METHOD TO CREATE WORDCLOUD
# Word cloud for the most common words
library(reshape2)
word_cloud <- plat_data_words %>%
  with(wordcloud(word, n, max.words = 100))

# Create a comparison cloud for party-specific words 
word_cloud_comparison <- plat_dem_rep %>%
  reshape2::acast(word ~ party, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("blue3", "firebrick3"),
    max.words = 100,
    title.size = 1.5,
    random.order = FALSE,
    scale = c(2, 0.2)
  )

# create a scatter plot of the sentiment scores grouped by party
library(ggplot2)

#creating a new vader_scores with only dem & rep
vader_scores_rd <- vader_scores %>%
  filter(party %in% c("Democratic", "Republican")) %>%
  select(speech_id, party, compound)


ggplot(vader_scores_rd, aes(x = compound, y = party, color = party)) +
  geom_jitter(width = 0.1, height = 0.2, alpha = 0.5) +
  labs(title = "VADER Sentiment Scores by Party",
       x = "Compound Sentiment Score",
       y = "Party") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c(
      "Democratic" = "royalblue",
      "Republican" = "firebrick3",
      "Republican (see 2016)**" = "firebrick3")) +
  scale_x_continuous(limits = c(0.6, 1))




#-----Wordclouds using BING sentiment lexicon

library(reshape2)
library(textstem)
library(SnowballC)
library(textdata)

# Party-specific sentiment counts
bing_party_words <- plat_dem_rep %>%
  unnest_tokens(word, text) %>%
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
  max.words = 120,
  colors = c("firebrick3", "steelblue3", "darkorange", "darkgreen"),  # red = negative, blue = positive
  title.size = 1,
  scale = c(2.5, 0.6),
  random.order = FALSE
)







