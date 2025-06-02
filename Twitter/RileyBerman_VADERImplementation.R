#Contributor: Riley Berman 
#Description: Applying VADER sentiment analysis to Senate Tweet Data in Tweet folder (in json format)
#             Output: Senate_Tweets_SentimentAnalysis.csv in Senate_Tweets_SentimentAnalysis folder
#Methodology: Functions in vader package

rm(list=ls())

library(tidylog)
library(tidyverse)
library(jsonlite)
library(janitor)
library(reticulate)
library(data.table)

#Loading in json files
Senate_FirstBatch <- fromJSON("Tweets/Senate_FirstBatch.json") |> flatten() |> as_tibble()

Senate_SecondBatch <- fromJSON("Tweets/Senate_SecondBatch.json") |> flatten() |> as_tibble()

#Grab the columns we want and combine the two batches (dropped 138 variables)
Senate_Tweets <- Senate_FirstBatch |> select(author.userName, author.name, fullText, retweetCount, replyCount, 
                                             likeCount, quoteCount, createdAt, twitterUrl, isReply, inReplyToUsername, 
                                             author.profilePicture, author.coverPicture, author.followers, author.createdAt, 
                                             author.statusesCount, entities.hashtags) |>
  bind_rows(Senate_SecondBatch |> select(author.userName, author.name, fullText, retweetCount, replyCount, 
                                         likeCount, quoteCount, createdAt, twitterUrl, isReply, inReplyToUsername, 
                                         author.profilePicture, author.coverPicture, author.followers, author.createdAt, 
                                         author.statusesCount, entities.hashtags)) |> clean_names() |>
  #Clean up entities_hashtags column 
  mutate(all_hashtags = sapply(entities_hashtags, function(x){
         if (is.null(x) || nrow(x) == 0) return(NA_character_)
         paste(x$text, collapse = ", ")})) |>
  select(-entities_hashtags) 

#Let's use the Python VADER 
vader <- import("vaderSentiment.vaderSentiment")
analyzer <- vader$SentimentIntensityAnalyzer()

#Testing... 
#Some made-up tweets
analyzer$polarity_scores("@SenatorBerman It is a REALLY great day to be alive!")$compound
analyzer$polarity_scores("@SenatorBerman It is a slightly bad day to be alive.")$compound

#Found some positive and negative tweets
test <- Senate_Tweets[c(1:2, 1238, 1819), ] |> select(author_user_name, full_text)

#Everything looks good except for the last tweet
test_results <- test |>
  mutate(result = map(full_text, ~ analyzer$polarity_scores(.x))) |>
  mutate(compound = map_dbl(result, "compound"),
         pos = map_dbl(result, "pos"),
         neu = map_dbl(result, "neu"),
         neg = map_dbl(result, "neg")) |>
  select(-result)

print(test_results)

#Interesting, for this tweet (which is arguably pretty negative), it is scored fairly positively!
#Note: this is because VADER is flagging most of this sentence as neutral and is not picking up on the "pretended not to hate us" part
#Note: I don't really know what we can do about this (maybe add words to the lexicon?)
analyzer$polarity_scores("Trump pretended not to hate us before election. Now the truth comes out. The GOP is a nightmare for #LGBT people.")

Senate_Tweets_SentimentAnalysis <- Senate_Tweets |>
  mutate(result = map(full_text, ~ analyzer$polarity_scores(.x))) |>
  mutate(compound = map_dbl(result, "compound"),
         pos = map_dbl(result, "pos"),
         neu = map_dbl(result, "neu"),
         neg = map_dbl(result, "neg")) |>
  select(-result) |>
  mutate(sentiment = case_when(
    compound >= 0.05 ~ "positive",
    compound <= -0.05 ~ "negative",
    TRUE ~ "neutral"))

#Create folder to store finalized .csv file
dir.create("Tweets_SentimentAnalysis", showWarnings = FALSE)

write_csv(Senate_Tweets_SentimentAnalysis, "Tweets_SentimentAnalysis/Senate_Tweets_SentimentAnalysis.csv")
