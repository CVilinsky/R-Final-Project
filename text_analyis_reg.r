
# load the libraries that we will use during the assignment

library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(here)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(robotstxt)
library(rvest)
library(glue)
library(lubridate)
library(gridExtra)
library(scales)
library(grid)
library(glmnet)

# first we load the data, using the read functions


civiqs_poll_data <- read.csv("civiqs_poll.csv")
colnames(civiqs_poll_data)[1] <- "Date"
civiqs_poll_data$Date <-as.Date(civiqs_poll_data$Date,format="%m/%d/%y")

trump_tweet_data <- readRDS("trump.rds") %>%
  rownames_to_column(var = "speech_id")
colnames(trump_tweet_data)[2] <- "Date"
trump_tweet_data$Date<-substr(trump_tweet_data$Date,1,10)
trump_tweet_data$Date<- as.Date(trump_tweet_data$Date)
speech_join_rep <- inner_join(trump_tweet_data,civiqs_poll_data ,by ="Date" )
speech_join_rep<-speech_join_rep %>% select(Date,speech_id,rep)
speech_join_rep$speech_id <- as.numeric(as.character(speech_join_rep$speech_id))
df_wiki_filtered <- read.csv("df_wiki_numeric.csv")
summmarised_stats <- read.csv("summarised_stats.csv") 



filtered_summ <- summmarised_stats[12:52,]


trump_tweet_data$length_text <-str_count(trump_tweet_data$text)
df_wiki_filtered$Date <-as.Date(df_wiki_filtered$Date,format="%d-%b-%y")


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




# civiqs_poll_data$rep == 0 ~ 'Unsure',
# add number of tweet for the wiki
#tweet_and_wiki <- merge(df_wiki_filtered_temp,trump_tweet_data,by = "Date")
#tweet_and_wiki_by_date <- tweet_and_wiki %>%
#  group_by(Date)%>%
#  summarise(number_of_tweets =n())


# switch col and row in the wiki
#df_wiki_filtered_temp_change_row_col <- as.data.frame(t(df_wiki_filtered_temp))



# the number of tweets in every date in the summarise 
tweet_and_summmarised_stats <- merge(summmarised_stats,trump_tweet_data,by = "Date")
#tweet_and_summmarised_stats_by_date <- tweet_and_summmarised_stats %>%
#  group_by(Date)%>%
#  summarise(number_of_tweets =n())




tweet_and_summmarised_stats_by_date <- tweet_and_summmarised_stats %>%
  group_by(Date)%>%
  summarise(number_of_tweets =n())
tweet_and_summmarised_stats_by_date <-merge(tweet_and_summmarised_stats_by_date,summmarised_stats, by="Date")



civiqs_poll_data_sentences <- civiqs_poll_data %>% 
  select(Date,rep,dem,text) %>%
  unnest_tokens(sentence, text, token = "sentences")


civiqs_poll_data_tweets <- civiqs_poll_data %>% 
  select(Date,rep,dem,text) %>%
  unnest_tokens(sentence, text, token = "tweets")

# separate into sentences
trump_tweet_data_sentences <- trump_tweet_data %>%
  unnest_tokens(sentence, text, token = "sentences")

# check the balance between retweet and nonretweet
ggplot(trump_tweet_data_sentences, aes(x = isRetweet)) +
  geom_bar()+
  labs(title = "the amount of tweet and retweet by trump")

ggplot(civiqs_poll_data,aes(x = rep_group)) +
  geom_bar()+
  labs(title = "Concernen level in the beging of the pandamic",
       subtitle = "See the proportion between the cocerne level in the Republican party supporters",
       y="Number of Days with the concern level", x="")



trump_tweet_data_word <-trump_tweet_data %>%
  rowwise()%>%
  mutate(sum_words = text %>% str_count("\\w+")%>% sum()) %>%
  ungroup() %>%
  unnest_tweets(word,text)
  

trump_tweet_data_retweet <- trump_tweet_data_word %>%
  filter(trump_tweet_data_word$isRetweet == TRUE)



ggplot(trump_tweet_data, aes(x = isRetweet)) +
  geom_bar()+
  labs(title = "the amount of tweet and retweet by trump")


ggplot(trump_tweet_data,aes(x=length_text,fill = isRetweet))+
  geom_density()+facet_grid(isRetweet~.,scales = "free")


ggplot(trump_tweet_data_word,aes(x=sum_words,fill = isRetweet))+
  geom_density()+facet_grid(isRetweet~.,scales = "free")

tweet_by_day <- trump_tweet_data %>% 
  group_by(Date)%>%
  summarise(number_of_tweets =n())
  
concerned_tweets_by_day <- merge(tweet_by_day, civiqs_poll_data ,by = "Date")
concerned_tweets_by_day <- concerned_tweets_by_day %>%
  select(Date,rep,number_of_tweets,length_text)





trump_top_500_ngrams <- trump_tweet_data %>% 
  unnest_tokens(n_grams, text, token = "ngrams", n = 3, n_min = 1) %>% 
  anti_join(stop_words, by = c("n_grams" = "word")) %>%    # drop rows with stop words
  group_by(n_grams) %>%    # group by bigram index
  summarise(n_grams = unique(n_grams), n = n(), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  rowid_to_column(var = "id") %>% 
  filter(id<=200) %>%
  
trump_top_500_ngrams<- trump_top_500_ngrams[-grep('rt',trump_top_500_ngrams$n_grams),]
trump_top_500_ngrams<- trump_top_500_ngrams[-grep('https',trump_top_500_ngrams$n_grams),]
trump_top_500_ngrams<- trump_top_500_ngrams[-grep('t.co',trump_top_500_ngrams$n_grams),]



trump_tweets_tf_idf_wide <- trump_tweet_data %>% 
  unnest_tokens(n_grams, text, token = "ngrams", n = 3, n_min = 1, drop = FALSE) %>% 
  anti_join(stop_words, by = c("n_grams" = "word")) %>% 
  group_by(speech_id, n_grams) %>% 
  summarise(n = n()) %>% 
  bind_tf_idf(n_grams,document = speech_id, n = n) %>% 
  filter(n_grams %in% trump_top_500_ngrams$n_grams) %>% 
  pivot_wider(id_cols = speech_id,
              names_from = n_grams,
              values_from = tf_idf,
              values_fill = 0)
trump_tweets_tf_idf_wide$speech_id <- as.numeric(as.character(trump_tweets_tf_idf_wide$speech_id))

trump_tweets_tf_idf_wide<-
  cbind(trump_tweets_tf_idf_wide, total = rowSums(trump_tweets_tf_idf_wide)-trump_tweets_tf_idf_wide$speech_id)




trump_tweet_sum_td_idf <- trump_tweets_tf_idf_wide %>% select(speech_id,total)
rep_tf_idf <- inner_join(trump_tweet_sum_td_idf ,speech_join_rep, by = "speech_id")


ggplot(rep_tf_idf, aes(x = total, y = rep,size = total)) +
    geom_point(aes (color = rep , alpha = 0.7)) +
    geom_smooth(method = "lm", formula = y ~ x)+
    labs(title = " the republican concern level vs sum of tf_idf tweet",
         x = "sum of tf_idf tweet",
         y = " republican concern level")+
  guides(alpha = FALSE ,smooth = FALSE)


cor(rep_tf_idf$total,rep_tf_idf$rep)



tweet_by_day <- inner_join(tweet_by_day ,civiqs_poll_data , by = "Date")
tweet_by_day <- tweet_by_day %>% select(-text)
tweet_by_day <- tweet_by_day[tweet_by_day$number_of_tweets <100,]


ggplot(tweet_by_day, aes(x = length_text, y = rep)) +
  geom_point() +
  stat_smooth()
cor(tweet_by_day$length_text,tweet_by_day$rep)





ggplot(tweet_by_day, aes(x = number_of_tweets, y = rep)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x)
  
cor(tweet_by_day$number_of_tweets,tweet_by_day$rep)






trump_tweets_tf_idf_long <- trump_tweet_data %>% 
  unnest_tokens(n_grams, text, token = "ngrams", n = 3, n_min = 1, drop = FALSE) %>% 
  anti_join(stop_words, by = c("n_grams" = "word")) %>% 
  group_by(speech_id, n_grams) %>% 
  summarise(n = n()) %>% 
  bind_tf_idf(n_grams,document = speech_id, n = n) %>% 
  filter(n_grams %in% trump_top_500_ngrams$n_grams) 
  


join_staff <- inner_join(trump_tweet_data , civiqs_poll_data , by = "Date")
join_staff <- join_staff %>% select(speech_id,rep)

'''
new_table <- inner_join(join_staff,trump_tweets_tf_idf_wide,by = "speech_id")
new_table <- new_table %>% select(-speech_id)
'''
#lm_trump <- lm(rep~. ,new_table)
#summary (lm_trump)


# create train and test 

set.seed(1234)
cov_split <- initial_split(tweet_by_day )
cov_train <- training(cov_split)
cov_test <- testing(cov_split)

# specify model ----------------------------------------------------------------


cov_mod <- linear_reg()%>%
  set_engine("lm") 



# build recipe -----------------------------------------------------------------

cov_rec <- recipe(rep ~ number_of_tweets,data=cov_train) 



# build workflow ---------------------------------------------------------------

cov_wflow <- workflow() %>%
  add_model(cov_mod) %>%
  add_recipe(cov_rec)


#cv ---------------------------------------------------------------------------

set.seed(1234)
cov_folds <- vfold_cv(cov_train, v = 3)
write_rds(cov_folds, "cov_folds.rds", compress = "bz2")

civiqs_poll_data_folds <- read_rds("cov_folds.rds")


# fit resamples ----------------------------------------------------------------

cov_fit_rs <- cov_wflow %>%
 fit_resamples(
   cov_folds,
   control = control_resamples(save_pred = TRUE)
 )

#write_rds(civiqs_poll_data_fit_rs, "cov_fit_rs.rds", compress = "xz")

#cov_fit_rs <- read_rds("cov_fit_rs.rds")




civiqs_poll_data_train_metrics <- collect_metrics(cov_fit_rs)
civiqs_poll_data_train_pred <- collect_predictions(cov_fit_rs)




civiqs_poll_data_train_pred %>%
  group_by(id) %>%
  roc_curve(truth = rep_group, .pred_concerned) %>%
  autoplot() +
  labs(
    title = "ROC curve for concerned & Not concerned level",
    subtitle = "Each resample fold is shown in a different color"
  )




# make predictions for test data -----------------------------------------------

civiqs_poll_data_fit <- civiqs_poll_wflow %>%
  fit(data = civiqs_poll_data_train)

civiqs_poll_data_test_pred <- predict(civiqs_poll_data_fit, new_data = civiqs_poll_data_test, type = "prob") %>%
  bind_cols(civiqs_poll_data_test %>% select(rep_group, Date, sentence))

civiqs_poll_data_test_pred %>%
  roc_curve(truth = factor(rep_group), .pred_concerned) %>%
  autoplot()

civiqs_poll_data_test_pred %>%
  roc_auc(truth = factor(rep_group), .pred_concerned)

Errors_in_the_model <- civiqs_poll_data_test_pred %>% 
  filter(rep_group == "concerned", `.pred_Not concerned` > 0.5)

