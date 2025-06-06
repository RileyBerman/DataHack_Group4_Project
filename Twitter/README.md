---
editor_options: 
  markdown: 
    wrap: 72
---

**Objective:** Performs sentiment analysis via `VADER` on scraped tweets of California State Senators.  
. Save output as a .csv file. Use `Twitter.Rproj` to run files. 

**Files:**

-   `Tweets`: Contains the raw tweet data in .json and .csv format (we use the .json format). We used a Twitter scraper service on 
    [Appify](https://apify.com/) to obtain (some of) the tweets of the current California State Senators. The tweets are 
    split into two different files. 

-   `RileyBerman_VADERTest.R`: Testing the VADER sentiment analysis package (R port and Python model). 

-   `RileyBerman_VADERImplementation.R`: Applying VADER sentiment analysis to the tweets in `Tweets`. 
    Outputs results as .csv file to `Tweets_SentimentAnalysis` folder. 
    
-   `Tweets_SentimentAnalysis`: Folder containing `Senate_Tweets_SentimentAnalysis.csv`, a .csv file 
    containing the sentiment analysis results of the combined tweets.
    
-   `RileyBerman_Tweets_SentimentAnalysis.R`: Performs analysis of `Senate_Tweets_SentimentAnalysis.csv`, importing
    `bills_statistics_final.csv`(from `LegiScan` project) and `CA_State_Final_Updated.csv` (from `CA_Senate` project). 
    Outputs presentation graphs to `Graphs` folder. These two .csv files have been copied over from their
    respective projects. 

-   `Graphs:` Folder containing presentation graphs from `RileyBerman_Tweets_SentimentAnalysis.R`. 

-   `VADER_Backup`: Some sentiment analysis code that was used on a preliminary tweet dataset. 
    Not used in the final analysis. 
    


