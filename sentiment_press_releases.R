#Description: Created a table containing politicians' usernames, tweets, date of the tweets
#             and the sentiment (score and direction) of the tweets. 

#Methodolgy: The sentiment is calculated using the analyzeSentiment function from the SentimentAnalysis package.
#             The sentiment score is then converted to a direction (positive, negative, neutral) using the convertToDirection function.


rm(list = ls())
setwd("~/Desktop/DataHack")


# Install and load the necessary packages
#install.packages("SentimentAnalysis")
#install.packages("SnowballC")
#install.packages("tidyverse")
library(SentimentAnalysis)
library(SnowballC)
library(tidyverse)
library(readr)

# Load the data
press_releases <- read_csv("all_11_press_releases.csv")
View(press_releases)

# isolate the content column
full_text <- press_releases$full_text

# sentiment analysis using analyzeSentiment function
sentiment <- analyzeSentiment(full_text)
sentiment$SentimentQDAP
# Convert the sentiment scores to a direction (positive, negative, neutral) & turn it into a tibble
sentiment_direction <- tibble(convertToDirection(sentiment$SentimentQDAP))

# Create a data frame with the tweets and their sentiment
press_release_df <- data.frame(full_text = full_text, sentiment = sentiment$SentimentQDAP, sentiment_direction = sentiment_direction)

# Create a new column in the original data frame with the sentiment direction
press_releases <- unique(left_join(press_releases, press_release_df, by = "full_text")) 

#Rename the last column to Sentiment Direction
colnames(press_releases)[ncol(press_releases )] <- "Sentiment Direction"

write.csv(press_releases, file = "Press_Release_Sentiment_Analysis.csv")
write_csv(press_releases, "/Users/sasha/Desktop/Press_Release_Sentiment_Analysis.csv")
