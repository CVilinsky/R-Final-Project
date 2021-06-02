

library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(here)
library(dplyr)
library(stringr)


# load the libraries that we will use during the assignment

library(ggplot2)
library(tidyr)
library(dplyr)
library(robotstxt)
library(rvest)
library(glue)
library(tidyverse) 
library(lubridate)
library(gridExtra)
library(scales)
library(grid)

# first we load the data, using the read functions

civiqs_poll_data$summed <- civiqs_poll_data$dem+civiqs_poll_data$rep
colnames(civiqs_poll_data)[1] <- "Date"
civiqs_poll_data$Date <- as.Date(civiqs_poll_data$Date,format="%m/%d/%Y")

#load csv of filtered wiki
df_wiki_filtered <- read.csv("C:/Users/vilin/Desktop/University/Year 2/Advanced Programing/R_FinalProj_new_TRY/df_wiki_numeric.csv")
df_wiki_filtered <- df_wiki_filtered[,2:ncol(df_wiki_filtered)]

#load the summarised stats
summarised_stats <- read.csv("C:/Users/vilin/Desktop/University/Year 2/Advanced Programing/R_FinalProj_new_TRY/summarised_stats.csv")
summarised_stats <- summarised_stats[,2:ncol(summarised_stats)]

filtered_summ <- summmarised_stats[12:52,]

#load the trump data
trump_tweet_data <- readRDS("trump.rds")%>%
  rownames_to_column(var = "speech_id")
civiqs_poll_data <- read.csv("poll_with_sum.csv")
civiqs_poll_data <- civiqs_poll_data[,2:ncol(civiqs_poll_data)]

df_wiki_filtered$Date <-as.Date(df_wiki_filtered$Date,format="%d-%b-%y")

colnames(trump_tweet_data)[2] <- "Date"

tweet_and_wiki <- merge(df_wiki_filtered_temp,trump_tweet_data,by = "Date")
tweet_and_wiki_by_date <- tweet_and_wiki %>%
  group_by(Date)%>%
  summarise(number_of_tweets =n())



# the number of tweets in every date 
tweet_and_summmarised_stats <- merge(summmarised_stats,trump_tweet_data,by = "Date")

tweet_and_summmarised_stats_by_date <- tweet_and_summmarised_stats %>%
  group_by(Date)%>%
  summarise(number_of_tweets =n())
tweet_and_summmarised_stats_by_date <-merge(tweet_and_summmarised_stats_by_date,summmarised_stats, by="Date")
  

df_governors <- read.csv("us-governors.csv")
df_governors <- df_governors %>% select(state_name,state_code,party)

get_party <- function(x){
  temp_vec <- grepl(x,df_governors$state_code)
  row_num <- min(which(temp_vec==T))
  df_governors$party[row_num]
} #given a state code, receive the party it's connected to

# separate into sentences
trump_tweet_data_sentences <- trump_tweet_data %>%
  unnest_tokens(sentence, text, token = "sentences")

# check the balance between retweet and nonretweet
ggplot(trump_tweet_data_sentences, aes(x = isRetweet)) +
  geom_bar()



help ("initial_split")
# create train and test 

set.seed(1234)
trump_tweet_data_split <- initial_split(trump_tweet_data_sentences,strata =isRetweet )
trump_tweet_data_train <- training(trump_tweet_data_split)
trump_tweet_data_test <- testing(trump_tweet_data_split)

# specify model ----------------------------------------------------------------

lasso_mod <- logistic_reg(penalty = 0.005, mixture = 1) %>%
  set_engine("glmnet")



# build recipe -----------------------------------------------------------------

covid_rec <- recipe(isRetweet ~ sentence, data = trump_tweet_data_train) %>%
  # tokenize into words
  step_tokenize(sentence, token = "words") %>%
  # filter out stop words
  step_stopwords(sentence) %>%
  # all the 1-grams followed by all the 2-grams followed by all the 3-grams
  step_ngram(sentence, num_tokens = 3, min_num_tokens = 1) %>%
  # keep the 500 most frequent words to avoid creating too many variables 
  step_tokenfilter(sentence, max_tokens = 500) %>%
  # calculate tf-idf
  step_tfidf(sentence)


# build workflow ---------------------------------------------------------------

covid_wflow <- workflow() %>%
  add_model(lasso_mod) %>%
  add_recipe(covid_rec)

