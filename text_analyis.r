

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

civiqs_poll_data <- read.csv("civiqs_poll.csv")
colnames(civiqs_poll_data)[1] <- "Date"
civiqs_poll_data$Date <-as.Date(civiqs_poll_data$Date,format="%m/%d/%y")

trump_tweet_data <- readRDS("trump.rds") %>%
  rownames_to_column(var = "speech_id")
colnames(trump_tweet_data)[2] <- "Date"
trump_tweet_data$Date<-substr(trump_tweet_data$Date,1,10)
trump_tweet_data$Date<- as.Date(trump_tweet_data$Date)

df_wiki_filtered <- read.csv("df_wiki_numeric.csv")
summmarised_stats <- read.csv("summarised_stats.csv") 


trump_tweet_data$length_text <-str_count(trump_tweet_data$text)
# create table of tweet and civiqs_poll


civiqs_poll_and_tweets <-
  merge(trump_tweet_data,civiqs_poll_data,by = "Date")

#civiqs_poll_data["text"] <- " "

for (i in 1:nrow(civiqs_poll_data)){
  
  temp_long_tweet <- " "
  temp_date <- civiqs_poll_data$Date[i]
  temp_vec <- grepl(temp_date, trump_tweet_data$Date)
  start_index <- (which(temp_vec==TRUE))
  j=start_index
  if (length(j)==0)
    break
  while(trump_tweet_data$Date[j]==temp_date)
  {
    temp_long_tweet <- paste(temp_long_tweet,
      trump_tweet_data$text[j])
    j <- j+1
  }
  civiqs_poll_data$text[i] <-temp_long_tweet 
}
 

civiqs_poll_data$length_text <-str_count(civiqs_poll_data$text)

civiqs_poll_data$sum_rep_dem <- civiqs_poll_data$dem+civiqs_poll_data$rep


 

# add number of tweet for the wiki
tweet_and_wiki <- merge(df_wiki_filtered_temp,trump_tweet_data,by = "Date")
tweet_and_wiki_by_date <- tweet_and_wiki %>%
  group_by(Date)%>%
  summarise(number_of_tweets =n())


# switch col and row in the wiki
df_wiki_filtered_temp_change_row_col <- as.data.frame(t(df_wiki_filtered_temp))


# the number of tweets in every date in the summarise 
tweet_and_summmarised_stats <- merge(summmarised_stats,trump_tweet_data,by = "Date")
tweet_and_summmarised_stats_by_date <- tweet_and_summmarised_stats %>%
  group_by(Date)%>%
  summarise(number_of_tweets =n())




df_governors <- read.csv("us-governors.csv")
df_governors <- df_governors %>% select(state_name,state_code,party)


civiqs_poll_data_sentences <- civiqs_poll_data %>% 
  select(Date,rep,dem,sum_rep_dem,text) %>%
  unnest_tokens(sentence, text, token = "sentences")
  

# separate into sentences
trump_tweet_data_sentences <- trump_tweet_data %>%
  unnest_tokens(sentence, text, token = "sentences")

# check the balance between retweet and nonretweet
ggplot(trump_tweet_data_sentences, aes(x = isRetweet)) +
  geom_bar()



help ("initial_split")
# create train and test 

set.seed(1234)
civiqs_poll_data_split <- initial_split(civiqs_poll_data_sentences,strata =isRetweet )
civiqs_poll_data_train <- training(civiqs_poll_data_split)
civiqs_poll_data_test <- testing(civiqs_poll_data_split)

# specify model ----------------------------------------------------------------

lasso_mod <- logistic_reg(penalty = 0.005, mixture = 1) %>%
  set_engine("glmnet")



# build recipe -----------------------------------------------------------------

covid_rec <- recipe(sum_rep_dem ~ sentence, data = civiqs_poll_data_train) %>%
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

