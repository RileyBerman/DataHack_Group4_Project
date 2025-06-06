#Contributor: Riley Berman 
#Description: Testing vader sentiment analysis package, as well as original VADER package
#Methodology: Functions in vader, reticulate package to access Python VADER package

rm(list = ls())

library(tidyverse)
library(kableExtra)
library(janitor)
library(webshot2)

#VADER (Valence Aware Dictionary and sEntiment Reasoner) was originally developed in Python.
#This is an R port of the original Python package.
#Source: https://cran.r-project.org/web/packages/vader/vader.pdf
library(vader)

#More information: 
#Source: https://hex.tech/templates/sentiment-analysis/vader-sentiment-analysis/
#Source: https://blog.marketingdatascience.ai/sentiment-analysis-of-online-reviews-with-different-lexicons-using-r-bc726649c8ef
#Source: https://blog.marketingdatascience.ai/basic-sentiment-analysis-using-r-with-vader-4eecb738566f
#Source: https://dgarcia-eu.github.io/SocialDataScience/Tutorials/3_Affect/035_UnsupervisedToolsR/UnsupervisedToolsR.html

#get_vader() function over a text
#Result: 
#word_scores: string that contains ordered list with the matched scores for each of the words in the text. 
#compound: final valence compound of VADER for the whole text after applying modifiers and aggregation rules, (-1, 1) = (very negative, very positive). 
#pos, neg, and neu: the parts of the compound for positive, negative, and neutral content. 
#These take into account modifiers and are combined when calculating the compound score 
#but_count: an additional count of “but” since it can complicate the calculation of sentiment
get_vader("Great day to be alive")

#Does VADER work with emojis?
#Need UTF-8 encoding (R treats the emoji 😊 the same as the Unicode code point)
#Result: Emoji's are scored as neutral (likely limitation of the R version of VADER)
get_vader("Great day to be alive \U0001F60A")

#Note: "love with a positive score, "hate" with a negative score, and but_count = 1
get_vader("I love today and I hate tomorrow")

get_vader("No, I am your father")

#Note: when @mentions are included, if the word is neutral it really won't make a difference in the compound score (but, neu will increase)
get_vader("@SenatorBerman I love today and I hate tomorrow")

#What about the use of "but"? 
#Result: compound is more negative, emphasis on the "but" part of sentence
get_vader("I love today, but I hate tomorrow")

#Are exclamation points considered? 
#Result: compound more negative
get_vader("I love today, but I hate tomorrow!")

#Modifiers
#Result: compound more negative
#hate has more negative word_score
res <- get_vader("I love today, but I really hate tomorrow!")
res["compound"]

#or weak modifiers 
#Result: compound more positive
get_vader("I love today, but I slightly hate tomorrow")

#What about CAPS? 
#Result: compound more negative 
get_vader("I love today, but I HATE TOMORROW!")

#What about negators? 
#Result: compound negative
get_vader("I do not love today")

#Result: compound positive
get_vader("Today is not bad")

#Try to trick VADER
#Result: identified "love" as negative and "hate" as positive with modifiers. 
get_vader("I do not love today, but I do not hate it either")

#vader_df() to apply VADER to a dataframe
test <- tibble(
  text = c(
    "Great day to be alive",
    "Great day to be alive 😊",
    "I love today and I hate tomorrow",
    "I love today, but I hate tomorrow",
    "I love today, but I hate tomorrow!",
    "I love today, but I really hate tomorrow!",
    "I love today, but I slightly hate tomorrow",
    "I love today, but I HATE TOMORROW!", 
    "I do not love today",
    "Today is not bad",
    "I do not love today, but I do not hate it either"))

result1 <- vader_df(test$text)

print(result1)

#Note: Due to the modification rules, VADER not as fast as other dictionary-based methods 
#and running it can take from minutes to hours depending on your computer. 
#However, vader_df function makes it much faster than a loop. 

#We can actually import the original Python VADER package and use it in R
library(reticulate)

vader <- import("vaderSentiment.vaderSentiment")
analyzer <- vader$SentimentIntensityAnalyzer()

#Example
analyzer$polarity_scores("Great day to be alive")

#For presentation
test <- tibble(
  text = c(
    "I love today and I hate tomorrow",
    "I love today 😊 and I hate tomorrow",
    "I love today, but I hate tomorrow!",
    "I love today, but I really HATE tomorrow!",
    "I do not love today",
    "Today is not bad"))

#Does it match up to the R version?
result2 <- test |>
  mutate(result = map(text, ~ analyzer$polarity_scores(.x))) |>
  mutate(compound = map_dbl(result, "compound"),
         pos = map_dbl(result, "pos"),
         neu = map_dbl(result, "neu"),
         neg = map_dbl(result, "neg")) |>
 select(-result)

#For Presentation
#Exported to Graphs folder
result2_final <- result2 |>
  rename(Text = text,
         Compound = compound,
         Positive = pos,
         Neutral = neu,
         Negative = neg) |>
    kbl(booktabs = TRUE) |>
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover", "condensed"),
                html_font = "Helvetica",
                font_size = 13) |>
  row_spec(0, bold = TRUE, background = "#e6f0ff") |>  
  add_header_above(c(" " = 5)) |>  
  footnote(general = "Sentiment Analysis: VADER", general_title = "", 
           footnote_as_chunk = TRUE, 
           escape = FALSE)

#Save styled table as HTML
save_kable(result2_final, file = "Graphs/VADER_sample_temp.html")

#Convert to PNG using webshot
webshot("Graphs/VADER_sample_temp.html", file = "Graphs/VADER_sample.png", zoom = 3, vwidth = 1200, delay = 0.2)

#Check how the results match up
#Note: looks like the Python version picks up the emoji's!
comparison <- result1 |>   
  rename_with(~ paste0(., "_R"), .cols = -text) |>
  select(text, compound_R, pos_R, neu_R, neg_R) |>
  left_join(result2 |> rename_with(~ paste0(., "_Python"), .cols = -text), by = "text") |>
  #changing column order
  select(text, compound_R, compound_Python, pos_R, pos_Python, neu_R, neu_Python, 
         neg_R, neg_Python)

#Let's look at the original VADER documentation
#Source: https://github.com/cjhutto/vaderSentiment
#Paper: VADER: A Parsimonious Rule-based Model for Sentiment Analysis of Social Media Text
#       (by C.J. Hutto and Eric Gilbert)
#       Eighth International Conference on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June 2014.

#Citation: Hutto, C.J. & Gilbert, E.E. (2014). VADER: A Parsimonious Rule-based Model for Sentiment Analysis of Social Media Text. 
#          Eighth International Conference on Weblogs and Social Media (ICWSM-14). Ann Arbor, MI, June 2014.

#Compound score is computed by summing valence scores of each word in the lexicon, adjusted according to the rules, 
#and then normalized to be between -1 (most extreme negative) and +1 (most extreme positive). 
#Most useful metric if you want a single unidimensional measure of sentiment for a given sentence.

#positive sentiment: compound score >= 0.05
#neutral sentiment: (compound score > -0.05) & (compound score < 0.05)
#negative sentiment: compound score <= -0.05

#pos, neg, and neu scores should add up to 1. 
#Note: these proportions represent the "raw categorization" of each lexical item (e.g., words, emoticons/emojis, or initialisms) 
#into positve, negative, or neutral classes; they do not account for the VADER rule-based enhancements such as word-order sensitivity for 
#sentiment-laden multi-word phrases, degree modifiers, word-shape amplifiers, and others. 

#What is does: Lexicon and rule-based sentiment analysis tool that is specifically
#attuned to sentiments expressed in social media, and works well on texts from other
#domains. 
#Note: Don't need to remove stopwords or preform stemming/lemmatization -- VADER handles tokenization.
#Note: Twitter tweets actually used in testing
#Note: Neutral sentiment does not contribute to overall sentiment score since by definition it’s score = 0.
#Advantages: VADER accounts for intensity of emotions: applies rules to words in a sentence and also 
#uses intensifiers ("really"), capitalization ("HAPPY"), punctuation ("!"), the word "but" (giving weight to what follows), contractions ("wasn't"), 
#sentiment-laden slang words ("friggin"), emoticons (":)"), sentiment-laden initialisms and acronyms ("lol")
#Limitations: VADER was developed in 2014, a bit outdated.
#Source: https://blog.marketingdatascience.ai/basic-sentiment-analysis-using-r-with-vader-4eecb738566f

#You can also use the package tidyvader which implements VADER and is 
#apparently faster than the R package.
#Still no support for emojis. 
#Source: https://github.com/chris31415926535/tidyvader
#Still no support for emojis

#Comparing lexicons
#Source: https://blog.marketingdatascience.ai/sentiment-analysis-of-online-reviews-with-different-lexicons-using-r-bc726649c8ef
#Bing: Classifies words into binary categories (positive/negative). 
#AFINN: Provides sentiment intensity score, (-5, 5) = (very negative, very positive). 
#NRC: Categorizes words based on eight emotions (e.g., anger, fear, anticipation) along with binary positive/negative sentiment.
#VADER: Designed for analyzing sentiment in social media and other short-text environments by assigned score in (-1, 1) =  (negative, positive) 
#with -.05 to .05 considered neutral.
#Conclusion (for online restaurant reviews): 
#Overall, AFINN, Bing, and NRC lexicons provided relatively stable and comparable results, making them reliable for basic sentiment analysis of single words. 
#VADER, while powerful for more complex sentiment tasks, seems to overestimate extremes when only single words are analyzed. 
#Its wider range and higher peaks suggest it might perform better when analyzing longer text or when using n-grams, where context plays a more significant role.
