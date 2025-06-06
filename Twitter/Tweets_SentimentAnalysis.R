#Contributor: Riley Berman 
#Description: Preforms analysis of Senate Tweet Sentiment Analysis data (in Tweets_SentimentAnalysis)
#Methodology: tidyverse and ggplotting 

rm(list = ls())

library(tidyverse)
library(janitor)
library(ggplot2)
library(hrbrthemes)
library(tidylog)
library(lubridate)
library(fuzzyjoin)
library(fixest)
library(ggrepel)

Tweets_SentimentAnalysis <- read_csv("Tweets_SentimentAnalysis/Senate_Tweets_SentimentAnalysis.csv")

CA_State_Final_Updated <- read_csv("CA_State_Final_Updated.csv")

Tweets_SentimentAnalysis <- Tweets_SentimentAnalysis |>
  left_join(CA_State_Final_Updated |> mutate(twitter = str_replace_all(twitter, "@", "")), 
                                             by = c("author_user_name" = "twitter"))

#How many of the tweets for each senator did we scrape? 
tweet_summary <- Tweets_SentimentAnalysis |> summarize(tweets = n(), 
                                      proportion = tweets/first(author_statuses_count),
                                      .by = author_user_name)

average_tweets <- tweet_summary$proportion |> mean()

#Convert to date column 
Tweets_SentimentAnalysis <- Tweets_SentimentAnalysis |>
  mutate(created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"), 
         author_created_at = as.POSIXct(author_created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC")) |>
  mutate(date_created = as.Date(created_at), 
         author_date_created = as.Date(author_created_at)) |>
  mutate(popularity = retweet_count + reply_count + like_count + quote_count, 
         year = year(date_created)) |>
  #Very low number of tweets for the years prior, so filtering them out
  filter(year >= "2013") |>
  #Only has 2 tweets
  filter(author_user_name != "SenMeganDahle")

#By year? 
#How many of the tweets for each senator did we scrape? 
year_summary <- Tweets_SentimentAnalysis |> 
  summarize(tweets = n(), .by = year) |>
  arrange(year)

#By year, party? 
year_party_summary <- Tweets_SentimentAnalysis |> 
  summarize(tweets = n(), .by = c(year, party)) |>
  arrange(year)

year_author_summary <- Tweets_SentimentAnalysis |> 
  summarize(tweets = n(), .by = c(year, author_user_name)) |>
  arrange(year)

year_barplot <- year_summary |>
  ggplot(aes(x = year, y = tweets)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Tweets by Year", x = "Year", y = "Number of Tweets") +
  theme_ipsum()

#Tweeting frequency by month? 
month_summary <- Tweets_SentimentAnalysis |>
  mutate(month = lubridate::month(date_created, label = TRUE)) |>
  summarize(tweets = n(), .by = month) |>
  arrange(month)

#Note: December is heavily counted because I scraped the latest tweets from that year
#Note: there will be some bias as a result 
month_barplot <- month_summary |>
  ggplot(aes(x = month, y = tweets)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Tweets by Month", x = "Month", y = "Number of Tweets") +
  theme_ipsum()

#Graph for Presentation 
#Compound scores by year? 
compound_plot_final <- Tweets_SentimentAnalysis |>
  summarize(compound = mean(compound, na.rm = TRUE), 
            pos = mean(pos, na.rm = TRUE),
            neu = mean(neu, na.rm = TRUE),
            neg = mean(neg, na.rm = TRUE),
            .by = c(year, party)) |>
  arrange(year) |>
  ggplot(aes(x = year)) + 
  geom_line(aes(y = compound, color = "Compound Score"), linewidth = 1) +
  geom_line(aes(y = pos, color = "Positive Share"), linewidth = 1) +
  geom_line(aes(y = neu, color = "Neutral Share"), linewidth = 1) +
  geom_line(aes(y = neg, color = "Negative Share"), linewidth = 1) + 
  scale_color_manual(values = c("Compound Score" = "#08519c",
                                "Positive Share" = "#2ca25f",
                                "Neutral Share"  = "#bcbddc", 
                                "Negative Share" = "#de2d26")) +
  scale_x_continuous(breaks = seq(min(Tweets_SentimentAnalysis$year), 
                                         max(Tweets_SentimentAnalysis$year), by = 4)) +
  labs(title = "Sentiment Analysis of Tweets by Party", 
       subtitle = "Are the parties becoming more negative?",
       x = "Year", 
       y = "Yearly Average", 
       caption = "Data: Twitter") +
  theme_ipsum(base_family = "Helvetica", base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.margin = margin(t = -5),
        legend.box.margin = margin(b = -5),
        legend.title = element_blank(), 
        strip.text = element_text(size = 13)) + 
  facet_wrap(~party)

#List of Senators by Compound Value?
most_negative <- Tweets_SentimentAnalysis |>
  summarize(compound = mean(compound, na.rm = TRUE), 
            party = first(party), 
            .by = c(author_user_name)) 

most_negative_barplot <- most_negative |>
  slice_min(compound, n = 15) |>
  ggplot(aes(x = reorder(author_user_name, compound), y = compound, fill = author_user_name)) + 
  geom_col(fill = "steelblue") +
  labs(title = "Most Negative Senators", x = "Senator", y = "Compound Score") + 
  theme_ipsum() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Graphing Senators' sentiment by year
senator_sentiment_year <- Tweets_SentimentAnalysis |>
  summarize(compound = mean(compound, na.rm = TRUE), 
            pos = mean(pos, na.rm = TRUE),
            neu = mean(neu, na.rm = TRUE),
            neg = mean(neg, na.rm = TRUE),
            .by = c(author_user_name, year)) |>
  ggplot(aes(x = year, y = compound, color = author_user_name)) +
  geom_line(alpha = 0.5) +
  labs(title = "Senators' Sentiment by Year", x = "Year", y = "Compound Score") +
  theme_ipsum() + 
  theme(legend.position = "none")
  
#Graph for Presentation 
#Compound scores by year for just the 8 most negative senators? 
#Note: Now we see compound scores decreasing!
#Note: Pattern continues for top ~12 most negative senators? 
#Note: -0.1723715 decrease in compound score
compound_plot_negative_table <- Tweets_SentimentAnalysis |>
  filter(author_user_name %in% c(most_negative |>
                                   slice_min(compound, n = 8) |> pull(author_user_name))) |>
  summarize(compound = mean(compound, na.rm = TRUE), 
            pos = mean(pos, na.rm = TRUE),
            neu = mean(neu, na.rm = TRUE),
            neg = mean(neg, na.rm = TRUE),
            .by = year) |>
  arrange(year) |>
  mutate(difference_compound = last(compound) - first(compound)) |>
  distinct(difference_compound) |>
  pull(difference_compound)

#Not a party issue: 6 Democrats, 4 Republicans
party_percentage <- Tweets_SentimentAnalysis |>
  filter(author_user_name %in% c(most_negative |>
                                   slice_min(compound, n = 8) |> pull(author_user_name, party))) |>
  distinct(author_user_name, party) |>
  summarize(total = n(), 
            democrats = sum(party == "Democrat", na.rm = TRUE)/total,
            republicans = sum(party == "Republican", na.rm = TRUE)/total) 

compound_plot_negative_final <- Tweets_SentimentAnalysis |>
  filter(author_user_name %in% c(most_negative |>
           slice_min(compound, n = 8) |> pull(author_user_name, party))) |>
  summarize(compound = mean(compound, na.rm = TRUE), 
            pos = mean(pos, na.rm = TRUE),
            neu = mean(neu, na.rm = TRUE),
            neg = mean(neg, na.rm = TRUE),
            .by = year) |>
  arrange(year) |>
  ggplot(aes(x = year)) +
  scale_x_continuous(breaks = seq(min(Tweets_SentimentAnalysis$year), 
                                  max(Tweets_SentimentAnalysis$year), by = 4)) +
  geom_line(aes(y = compound, color = "Compound Score"), linewidth = 1) +
  geom_line(aes(y = pos, color = "Positive Share"), linewidth = 1) +
  geom_line(aes(y = neu, color = "Neutral Share"), linewidth = 1) +
  geom_line(aes(y = neg, color = "Negative Share"), linewidth = 1) + 
  scale_color_manual(values = c("Compound Score" = "#08519c", 
                                "Positive Share" = "#2ca25f",
                                "Neutral Share"  = "#bcbddc", 
                                "Negative Share" = "#de2d26")) + 
  labs(title = "Sentiment Analysis of Tweets of Top 8 Most Negative Senators", 
       subtitle = "Are certain Senators becoming more negative?",
       x = "Year",
       y = "Yearly Average", 
       caption = "Senators were ranked by their average compound score\n Data: Twitter") +
  theme_ipsum(base_family = "Helvetica", base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.margin = margin(t = -5),
        legend.box.margin = margin(b = -5),
        legend.title = element_blank())
  
#Sentiment Count by Year
#Note: Looks like an increase in both positive and negative tweets over the years
#Note: Recent decrease in neutral tweets 
sentiment_count <- Tweets_SentimentAnalysis |>
  summarize(positive = sum(sentiment == "positive", na.rm = TRUE),
            negative = sum(sentiment == "negative", na.rm = TRUE),
            neutral = sum(sentiment == "neutral", na.rm = TRUE),
            .by = year) |>
  pivot_longer(cols = c(positive, negative, neutral), 
               names_to = "sentiment", 
               values_to = "count") |>
  ggplot(aes(x = year, y = count, color = sentiment)) +
  geom_line() + 
  labs(title = "Sentiment Count by Year", x = "Year", y = "Count") +
  theme_ipsum()

#Graph for Presentation
#Percentages
#Note: Positive tweets have always been the majority 
#Note: Looks like an increase in both positive and negative tweets over the years
#Note: Historical decrease in neutral tweets 
sentiment_percentages_final <- Tweets_SentimentAnalysis |> 
  summarize(positive = sum(sentiment == "positive", na.rm = TRUE),
            negative = sum(sentiment == "negative", na.rm = TRUE),
            neutral = sum(sentiment == "neutral", na.rm = TRUE),
            .by = year) |>
  mutate(total = positive + negative + neutral,
         positive_pct = positive/total * 100,
         negative_pct = negative/total * 100,
         neutral_pct = neutral/total * 100) |>
  select(year, positive_pct, negative_pct, neutral_pct) |>
  pivot_longer(cols = c(positive_pct, negative_pct, neutral_pct), 
               names_to = "sentiment", 
               values_to = "percentage") |>
  mutate(sentiment = recode(sentiment,
                            positive_pct = "Positive Tweet",
                            neutral_pct  = "Neutral Tweet",
                            negative_pct = "Negative Tweet")) |>
  ggplot(aes(x = year, y = percentage, color = sentiment)) +
  scale_x_continuous(breaks = seq(min(Tweets_SentimentAnalysis$year), 
                                  max(Tweets_SentimentAnalysis$year), by = 4)) + 
  geom_line() + 
  scale_color_manual(values = c("Positive Tweet" = "#2ca25f",  
                                "Neutral Tweet"  = "#bcbddc", 
                                "Negative Tweet" = "#de2d26")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Sentiment Distribution of Tweets", x = "Year", y = "Proportion of Tweets", 
       subtitle = "Recent increases in shares of negative and positive tweets ", 
       caption = "Data: Twitter") +
  theme_ipsum(base_family = "Helvetica", base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.margin = margin(t = -5),
        legend.box.margin = margin(b = -5),
        legend.title = element_blank())

#Are more negative tweets popular? 
#Let's first look at one Senator
Senator_Hurtado <- Tweets_SentimentAnalysis |>
  filter(author_user_name == "Senator_Hurtado") |>
  ggplot(aes(x = popularity, y = compound)) +
  geom_point(aes(color = sentiment), alpha = 0.5) + 
  labs(title = "Senator Hurtado's Tweets: Popularity vs. Sentiment", 
       x = "Popularity (Retweets + Replies + Likes + Quotes)", 
       y = "Compound Sentiment Score") +
  theme_ipsum()

#Graph for presentation
#How about for all Senators?
#Note: Doesn't look like much of a trend of negative tweets being more popular
Tweet_Statistics <- Tweets_SentimentAnalysis |>
  mutate(avg_popularity = mean(popularity, na.rm = TRUE), 
         median_popularity = median(popularity, na.rm = TRUE),
         avg_compound = mean(compound, na.rm = TRUE), 
         avg_pos = mean(pos, na.rm = TRUE),
         avg_neu = mean(neu, na.rm = TRUE),
         avg_neg = mean(neg, na.rm = TRUE), 
         .by = c(author_user_name, year)) 

all_senators_popularity_final <- Tweet_Statistics |>
  mutate(sentiment = recode(sentiment,
                            "positive" = "Positive Tweet",
                            "neutral"  = "Neutral Tweet",
                            "negative" = "Negative Tweet")) |>
  #only chopping off 2 outliers
  filter(popularity < 5000) |>
  ggplot(aes(x = popularity-median_popularity, y = compound, color = sentiment)) +
  geom_jitter(alpha = 0.5) + 
  labs(title = " Median-Centered Popularity vs. Sentiment of Tweets", 
       subtitle = "Negativity doesn't pay", 
       x = "Adjusted Popularity", 
       y = "Compound Score", 
       caption = "Each dot represents a Senator's tweet\n Popularity = Retweets + Replies + Likes + Quotes\n Adjusted Popularity = Popularity - Median for each year and Senator\n Note: 2 outliers removed\n Data: Twitter") + 
  theme_ipsum(base_family = "Helvetica", base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.margin = margin(t = -5),
        legend.box.margin = margin(b = -5),
        legend.title = element_blank())

#Run regression of popularity on compound score fixing for year and author_user_name
regression <- feols(popularity ~ compound | year + author_user_name, data = Tweets_SentimentAnalysis)
summary(regression)

#What about for the most negative senators? 
#Note: Same thing
most_negative_popularity <- Tweets_SentimentAnalysis |>
  filter(author_user_name %in% c(most_negative |> 
           slice_min(compound, n = 10) |> pull(author_user_name))) |>
  ggplot(aes(x = popularity, y = compound, color = sentiment)) +
  geom_point(alpha = 0.5) + 
  labs(title = "Most Negative Senators' Tweets: Popularity vs. Sentiment", 
       x = "Popularity (Retweets + Replies + Likes + Quotes)", 
       y = "Compound Sentiment Score") +
  theme_ipsum()

#What about looking at followers count? Maybe less followed senators use negativity to gain popularity? 
#Note: Not too much, looks like Senators with less followers generally have a greater compound score
followers_compound_plot <- Tweets_SentimentAnalysis |>
  summarize(compound = mean(compound, na.rm = TRUE),
            .by = author_followers)  |>
  ggplot(aes(x = author_followers, y = compound)) +
  geom_point(alpha = 0.5) + 
  labs(title = "Followers Count vs. Average Compound Score", 
       x = "Followers Count", 
       y = "Average Compound Score") +
  theme_ipsum()

#Run a simple regression of followers count on compound score
#Note: Not really significant
regression2 <- feols(author_followers ~ compound | year + author_user_name, data = Tweets_SentimentAnalysis)
summary(regression2)

#Add simple regression to ggplot
regression2_plot <- followers_compound_plot + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Followers Count vs. Average Compound Score with Regression Line", 
       x = "Followers Count", 
       y = "Average Compound Score")

#Read in the Senate People Data and Bipartisan Data
CA_State_Final_Updated <- read_csv("CA_State_Final_Updated.csv")

bills_statistics_final <- read_csv("bills_statistics_final.csv")

#Join datasets
#Some Senators need renaming for matching 
Senate_Bipartisan <- CA_State_Final_Updated |>
  mutate(new_name = case_when(members == "Thomas Umberg" ~ "Tom Umberg", 
                              members == "Brian W. Jones" ~ "Brian Jones", 
                              members == "Maria Elena Durazo" ~ "Maria Durazo", 
                              members == "Eloise Gómez Reyes" ~ "Eloise Reyes", 
                              members == "Sasha Renée Pérez" ~ "Sasha Renee Perez",
                              TRUE ~ members)) |>
  stringdist_left_join(bills_statistics_final, by = c("new_name" = "people_name_sponsors"), max_dist = 1) |>
  select(-c(new_name, people_name_sponsors))

Senate_Bipartisan <- Senate_Bipartisan |> 
  #Some members just have the year as the date they assumed office (assign December 1 of that year)
  mutate(date_assumed_office = ifelse(str_detect(date_assumed_office, "^\\d{4}$"), paste("December 1,", date_assumed_office), date_assumed_office), 
         date_assumed_office = as.Date(date_assumed_office, format = "%B %d, %Y"), 
         time_in_office = Sys.Date() - date_assumed_office)

#Are negative politicians more/less likely to be bipartisan?
Tweets_Bipartisan <- Tweets_SentimentAnalysis |> 
  summarize(avg_compound = mean(compound, na.rm = TRUE), 
            avg_pos = mean(pos, na.rm = TRUE),
            avg_neu = mean(neu, na.rm = TRUE),
            avg_neg = mean(neg, na.rm = TRUE),
            sentiment = case_when(avg_compound > 0.05 ~ "positive", 
                              avg_compound < -0.05 ~ "negative", 
                              TRUE ~ "neutral"),
            .by = author_user_name) |>
  right_join(Senate_Bipartisan |> mutate(twitter = str_replace_all(twitter, "@", "")), by = c("author_user_name" = "twitter")) 

#Graphing
bipartisan3_plot <- Tweets_Bipartisan |>
    filter(!is.na(avg_compound)) |>
    ggplot(aes(x = avg_compound)) +
    labs(title = "Bipartisanship3 vs. Average Compound Score", 
         x = "Average Compound Score", 
         y = "Bipartisanship") +
    geom_point(aes(y = weighted_bipartisan_rate3, color = time_in_office), alpha = 0.5) + 
    theme_ipsum()

#time_spent_in_office versus compound score? 
#Note: Doesn't look like much 
time_compound_plot <- Tweets_Bipartisan |>
  filter(!is.na(avg_compound)) |>
  ggplot(aes(x = time_in_office, y = avg_compound)) +
  labs(title = "Time in Office vs. Average Compound Score", 
       x = "Time in Office (Days)", 
       y = "Average Compound Score") +
  geom_point() + 
  theme_ipsum()

#What about party polarization? 
Tweets_Polarization <- Tweets_SentimentAnalysis |> 
  mutate(avg_popularity = mean(popularity, na.rm = TRUE), 
         median_popularity = median(popularity, na.rm = TRUE),
         avg_compound = mean(compound, na.rm = TRUE), 
         avg_pos = mean(pos, na.rm = TRUE),
         avg_neu = mean(neu, na.rm = TRUE),
         avg_neg = mean(neg, na.rm = TRUE), 
         .by = c(author_user_name, year)) |>
  filter(str_detect(str_to_lower(full_text), 
                    "\\b(democrat|democrats|dems|liberal|libs|leftist|leftwing|progressive|
                    biden|kamala|pelosi|schumer|woke|republican|republic|republicans|gop|conservative|
                    conservatives|maga|far-right|far-left|rightwing|trump|mccarthy|desantis|mtg|boebert|greene)\\b")) |>
  filter(sentiment == "negative") 

#Example
example <- Tweets_Polarization[1, ]

#Are more politically polarizing tweets more popular?
#Let's first look at one politician 
Senator_McNerney <- Tweets_Polarization |>
  mutate(high_popularity = ifelse(popularity - median_popularity > 0, TRUE, FALSE)) |> 
  filter(author_user_name == "SenMcNerney") |>
  ggplot(aes(x = date_created, y = compound, color = high_popularity)) +
  geom_point(alpha = 0.5) + 
  labs(title = "Senator McNerney Polarizing Tweets: Median-Centered Popularity vs. Sentiment", 
       x = "Median-Centered Popularity (Retweets + Replies + Likes + Quotes - Average Popularity by Year)", 
       y = "Average Compound Score") +
  theme_ipsum()

#What percent of polarizing tweets get more than average popularity? 
polarization_popularity_summary <- Tweets_Polarization |>
  mutate(high_popularity = ifelse(popularity - avg_popularity > 0, TRUE, FALSE), 
         high_popularity2 = ifelse(popularity - median_popularity > 0, TRUE, FALSE)) |>
  summarize(total_tweets = n(), 
            high_popularity_count = sum(high_popularity, na.rm = TRUE), 
            high_popularity2_count = sum(high_popularity2, na.rm = TRUE), 
            high_popularity_pct = high_popularity_count/total_tweets * 100, 
            high_popularity2_pct = high_popularity2_count/total_tweets * 100,
            .by = author_user_name)

#Now all 
polarization_popularity1_all <- Tweets_Polarization |>
  mutate(high_popularity = ifelse(popularity - avg_popularity > 0, TRUE, FALSE)) |>
  ggplot(aes(x = date_created, y = compound, color = high_popularity)) +
  geom_point(alpha = 0.5) + 
  labs(title = "Polarizing Tweets: Demeaned Popularity vs. Sentiment", 
       x = "Date Created", 
       y = "Compound Sentiment Score") + 
  theme_ipsum() + 
  facet_wrap(~ author_user_name, scales = "free")

#Graph for presentation
#Interesting: Using median gives us very different results!
polarization_popularity2_all_final <- Tweets_Polarization |>
  mutate(high_popularity2 = ifelse(popularity - median_popularity > 0, "Above Median Popularity", "Below Median Popularity")) |>
  ggplot(aes(x = date_created, y = compound, color = high_popularity2)) +
  geom_jitter(alpha = 0.5, size = 2) + 
  labs(title = "Politically Polarizing Tweets: Year vs. Sentiment", 
    subtitle = "Political polarization pays",
    x = "Year", 
    y = "Compound Score", 
    caption = "Each dot represents a Senator's politically polarizing tweet\n Popularity = Retweets + Replies + Likes + Quotes\n Median popularity calculated for each year and Senator\n Data: Twitter") + 
  scale_color_manual(values = c("Above Median Popularity" = "#de2d26", "Below Median Popularity" = "gray50")) +
  theme_ipsum(base_family = "Helvetica", base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.margin = margin(t = -5),
        legend.box.margin = margin(b = -5),
        legend.title = element_blank())

#Does this change by party?
polarization_popularity2_all_party <- polarization_popularity2_all_final + 
  facet_wrap(~party)

polarization_popularity2_plot <- Tweets_Polarization |>
  mutate(high_popularity2 = ifelse(popularity - median_popularity > 0, TRUE, FALSE)) |>
  ggplot(aes(x = date_created, y = compound, color = high_popularity2)) +
  geom_point(alpha = 0.5) + 
  labs(title = "Polarizing Tweets: Median-Centered Popularity vs. Sentiment", 
       x = "Date Created", 
       y = "Compound Sentiment Score") + 
  theme_ipsum() + 
  facet_wrap(~ author_user_name, scales = "free")

#How are tweet popularity distributed? 
#This is because so many tweets get no popularity 
#So apparently tweet popularity is heavily right-skewed, so mean > median 
popuarlity_distribution <- Tweets_SentimentAnalysis |>
  #Not removing many outliers (37)
  filter(popularity < 1000) |>
  ggplot(aes(x = popularity)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Tweet Popularity", x = "Popularity (Retweets + Replies + Likes + Quotes)", y = "Count") +
  theme_ipsum() 

#Graph for Presentation
#party polarization by year graph 
party_polarization_final <- Tweets_Polarization |>
  summarize(avg_compound = mean(compound, na.rm = TRUE), 
            avg_pos = mean(pos, na.rm = TRUE),
            avg_neu = mean(neu, na.rm = TRUE),
            avg_neg = mean(neg, na.rm = TRUE), 
            .by = c(year, party)) |>
  ggplot(aes(x = year, y = avg_compound)) +
  scale_x_continuous(breaks = seq(min(Tweets_Polarization$year), 
                                  max(Tweets_Polarization$year), by = 4)) +
  geom_line(color = "#2E8B57", size = 1) + 
  labs(title = "Politically Polarizing Tweets: Year vs. Sentiment",
       subtitle = "Are the parties becoming more polarized?",
       x = "Year",
       y = "Yearly Average Compound Score",
       caption = "Only includes politically polarizing tweets\n Republican data starts at 2019\n Data: Twitter") +
  theme_ipsum(base_family = "Helvetica", base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.minor = element_blank()) + 
  facet_wrap(~party, scales = "free_y") 

#Does politically polarizing language result in less bipartisanship? 
#Get the politicians who aren't in the polarizing dataset for comparison
Tweets_SentimentAnalysis_Merged <- Tweets_SentimentAnalysis |>
  mutate(new_name = case_when(members == "Thomas Umberg" ~ "Tom Umberg", 
                              members == "Brian W. Jones" ~ "Brian Jones", 
                              members == "Maria Elena Durazo" ~ "Maria Durazo", 
                              members == "Eloise Gómez Reyes" ~ "Eloise Reyes", 
                              members == "Sasha Renée Pérez" ~ "Sasha Renee Perez",
                              TRUE ~ members)) |>
  stringdist_left_join(bills_statistics_final, by = c("new_name" = "people_name_sponsors"), max_dist = 1) 

polarizing_list <- Tweets_Polarization |> distinct(author_user_name) 

Tweets_SentimentAnalysis_Polarizing <- Tweets_SentimentAnalysis_Merged |> 
  mutate(ind = ifelse(author_user_name %in% polarizing_list$author_user_name, 
                     "Politically Polarizing", 
                     "Not Politically Polarizing")) |>
  summarize(avg_compound = mean(compound, na.rm = TRUE), 
            bipartisan_rate1 = first(bipartisan_rate1),
            bipartisan_rate2 = first(bipartisan_rate2),
            weighted_bipartisan_rate2 = first(weighted_bipartisan_rate2),
            bipartisan_rate3 = first(bipartisan_rate3),
            weighted_bipartisan_rate3 = first(weighted_bipartisan_rate3),
            ind = first(ind),
            .by = author_user_name) |>
  pivot_longer(
    cols = c(bipartisan_rate1, weighted_bipartisan_rate2, weighted_bipartisan_rate3),
    names_to = "metric",
    values_to = "bipartisan_rate") |>
  mutate(metric = recode(metric, 
                         bipartisan_rate1 = "bipartisanship1",
                         weighted_bipartisan_rate2 = "Weighted bipartisanship2",
                         weighted_bipartisan_rate3 = "Weighted bipartisanship3"))

#Graph for Presentation
tweets_polarization_bipartisan_final <- Tweets_SentimentAnalysis_Polarizing |>
  ggplot(aes(x = avg_compound, y = bipartisan_rate, color = ind)) +
  geom_jitter(alpha = 0.6) +
  scale_color_discrete(name = "Type of Politician") + 
  labs(title = "Political Polarization vs. Bipartisanship",
       subtitle = "Are polarizing politicians less bipartisan?",
       x = "Average Compound Score",
       y = "Bipartisan Rate",
       legend = "Type of Politician",
       caption = "\n Each dot represents a unique Senator's Twitter handle\n Bipartisan rate is calculated as bipartisanship/bills sponsored\n Data: Twitter, LegiScan") +
  #can't use theme_ipsum()
    theme_ipsum(base_family = "Helvetica", base_size = 13) +
    theme(plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(size = 13),
          axis.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.justification = "left",
          legend.margin = margin(t = -5),
          legend.box.margin = margin(b = -5)) + 
  facet_wrap(~metric, scales = "free_y") 
            
#Save all final plots 
dir.create("Graphs", showWarnings = FALSE) 

for (plot_name in ls(pattern = "_final$")){
  plot_obj <- get(plot_name)
  if (inherits(plot_obj, "ggplot")){
    if (plot_name == "tweets_polarization_bipartisan_final"){
      ggsave(filename = paste0("Graphs/", plot_name, ".png"), 
             plot = get(plot_name), 
             width = 9,
             height = 5,
             dpi = 300, 
             bg = "white")}
    else{
    ggsave(filename = paste0("Graphs/", plot_name, ".png"), 
           plot = get(plot_name), 
           width = 8,
           height = 5,
           dpi = 300, 
           bg = "white")}}}
