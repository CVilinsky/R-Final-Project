

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
trump_tweet_data <- readRDS("trump.rds")

paths_allowed("https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/United_States_medical_cases")

page <- read_html("https://en.wikipedia.org/wiki/Template:COVID-19_pandemic_data/United_States_medical_cases")

table_wiki <- page %>%
  html_node("table.wikitable") %>%
  html_table(fill = TRUE)


df_wiki <- as.data.frame((table_wiki))

#save the DF so we won't load it again and again

write.csv(df_wiki,"C:/Users/vilin/Desktop/University/Year 2/Advanced Programing/Final/df_wiki.csv")

#ONLY DO THIS!!!!!!!!!!!! DON'T USE THE ABOVE!!!!
df_wiki <- read.csv("df_wiki.csv")
df_wiki <- df_wiki[,2:ncol(df_wiki)]
for (i in 1:ncol(df_wiki)){names(df_wiki)[i] <- df_wiki[1,i]}
df_wiki <- df_wiki[2:nrow(df_wiki),]

total_df_wiki <- df_wiki[486,2:ncol(df_wiki)] #create a small table with the totals in it.
df_wiki_advance <- df_wiki[-c(485:488),] #remove the last 4 rows which don't help us

df_wiki_filtered <-df_wiki_advance[!(df_wiki_advance$Date=="Date"),] #remove rows from the middle of the table

trump_tweet_data_1 <- gsub('.{9}$','',trump_tweet_data$date)
trump_tweet_data$date <- as.Date(trump_tweet_data_1, format = "%Y-%m-%d")

original_twitts <- trump_tweet_data%>% filter(isRetweet=="FALSE")
original_twitts <- original_twitts[order(original_twitts$date),] 
#change the titles to fit our work
names(df_wiki_filtered)[58] <- "Confirmed_daily"
names(df_wiki_filtered)[59] <- "Confirmed_total"
names(df_wiki_filtered)[60] <- "Death_daily"
names(df_wiki_filtered)[61] <- "Death_total"
names(df_wiki_filtered)[62] <- "Recovered_daily"
names(df_wiki_filtered)[63] <- "Recovered_total"
names(df_wiki_filtered)[64] <- "Active"

summmarised_stats <- df_wiki_filtered%>% select(tail(names(.),8))
#total_summarised_stats <- df_wiki[486:487,]
#summmarised_stats <- summmarised_stats[-1,]
summmarised_stats$Date <- dmy(summmarised_stats$Date)
#fill the empty cells with '0'
summmarised_stats[summmarised_stats==''] <- 0

df_wiki_filtered <- df_wiki_filtered[1:(length(df_wiki_filtered)-8)]

#function to replace the commas
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

#filter to get the dates we want to work on
filtered_summ <- summmarised_stats[12:52,]
#iterate the columns, and remove the commas for the values and turn the columns to numeric
for (i in 2:ncol(filtered_summ)){filtered_summ[,i] <- replaceCommas(filtered_summ[,i])
filtered_summ[,i] <- as.numeric(filtered_summ[,i])}

civiqs_poll_data$summed <- civiqs_poll_data$dem+civiqs_poll_data$rep
colnames(civiqs_poll_data)[1] <- "Date"
civiqs_poll_data$Date <- as.Date(civiqs_poll_data$Date,format="%m/%d/%Y")

#plots active vs voters

plot_active_cases_dates <- ggplot(filtered_summ,mapping = aes(x=Date,y=Active))+labs(title='Active Case',subtitle = 'In the begining of the pandamic')+geom_line()+scale_y_continuous(trans=log2_trans(), breaks = trans_breaks("log2", function(x) 2^x),
                                                                                                                                                                                      labels = trans_format("log2", math_format(2^.x)))
plot_polls_dates <- ggplot(civiqs_poll_data,aes(x=Date,y=summed))+geom_line()+labs(title='Concern Level',y='Level',x='Date')
grid.arrange(plot_active_cases_dates,plot_polls_dates,top=textGrob("The effect of active cases on the concern of the citizens",gp=gpar(fontsize=15,font=2)))

#iterate the columns, and remove the commas for the values and turn the columns to numeric
for (i in 2:ncol(summmarised_stats)){summmarised_stats[,i] <- replaceCommas(summmarised_stats[,i])
summmarised_stats[,i] <- as.numeric(summmarised_stats[,i])}


#ggplot(summmarised_stats%>% filter(Date>=as.Date('2020-10-01')&Date<=as.Date('2020-11-30')),aes(x=Date,y=Active))+geom_line()

df_wiki_filtered[df_wiki_filtered==''] <- 0


print(df_wiki_filtered[17,5])




#load the data
trump_tweet_data <- readRDS("trump.rds") %>%
  rownames_to_column(var = "speech_id")





new_date <- as.Date("26-Jan-20","%d-%b-%y")
print(new_date)

df_wiki_filtered_temp <- df_wiki_filtered
df_wiki_filtered_temp$Date
df_wiki_filtered_temp$Date <-as.Date(df_wiki_filtered_temp$Date,format="%d-%b-%y")
df_wiki_filtered_temp$Date
colnames(trump_tweet_data)[1] <- "Date"


tweet_and_wiki <- merge(df_wiki_filtered_temp,trump_tweet_data,by = "Date")
tweet_and_wiki_by_date <- tweet_and_wiki %>%
  group_by(Date)%>%
  summarise(number_of_tweets =n())

df_wiki_filtered_temp_change_row_col <- as.data.frame(t(df_wiki_filtered_temp))


# the number of tweets in every date 
tweet_and_summmarised_stats <- merge(summmarised_stats,trump_tweet_data,by = "Date")

tweet_and_summmarised_stats_by_date <- tweet_and_summmarised_stats %>%
  group_by(Date)%>%
  summarise(number_of_tweets =n())




df_governors <- read.csv("us-governors.csv")
df_governors <- df_governors %>% select(state_name,state_code,party)





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

