#TWEETS SENTIMENT ANALYSIS WITH VADER

#Contributor: Min Kim

#Description: Created a table containing politicians' usernames, tweets, date of the tweets
#             and the sentiment (score and direction) of the tweets. Them merged the table with the CA State
#             and CA Assembly data frames to see each politicians' average sentiment score.


rm(list=ls())

#install.packages(c("vader", "tidyverse", "scales", "data.table"))
library(vader)
library(tidyverse)
library(scales)
library(data.table)

# Load the tweet data
california_politicians_tweets_FIXED <- read_csv("C:/Users/mkim1/Downloads/california_politicians_tweets_FIXED.csv")
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
    but_count = map_dbl(vader_output, ~ as.numeric(.x["but_count"]))
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

fwrite(Merged_Senate, file = "VADER_Senate_avg_sentiment.csv")

# Some plots for comparison
#make a point plot of the average sentiment of each senator based on their party
ggplot(Merged_Senate, aes(x = reorder(twitter, sentiment_avg), y = sentiment_avg, color = party)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(title = "Average Sentiment of California Senators on Twitter",
       x = "Senator",
       y = "Average Sentiment Score") +
  theme_minimal() +
  coord_flip()  # Flip for better readability

#make a box plot based on the party's average sentiment
ggplot(Merged_Senate, aes(x = party, y = sentiment_avg, fill = party)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  labs(title = "Sentiment Scores by Party",
       x = "Party",
       y = "Sentiment Score") +
  theme_minimal() +
  theme(legend.position = "none")
