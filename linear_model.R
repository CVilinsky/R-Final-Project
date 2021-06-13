
# load the libraries that we will use during the assignment

library(tidymodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rvest)
library(lubridate)

# first we load the data, using the read functions


civiqs_poll_data <- read.csv("civiqs_poll.csv")
colnames(civiqs_poll_data)[1] <- "Date"
civiqs_poll_data$Date <-as.Date(civiqs_poll_data$Date,format="%m/%d/%y")

trump_tweet_data <- readRDS("trump.rds") %>%
  rownames_to_column(var = "speech_id")
colnames(trump_tweet_data)[2] <- "Date"
trump_tweet_data$Date<-substr(trump_tweet_data$Date,1,10)
trump_tweet_data$Date<- as.Date(trump_tweet_data$Date)
trump_tweet_data$length_text <-str_count(trump_tweet_data$text)

speech_join_rep <- inner_join(trump_tweet_data,civiqs_poll_data ,by ="Date" )
speech_join_rep<-speech_join_rep %>% select(Date,speech_id,rep)
speech_join_rep$speech_id <- as.numeric(as.character(speech_join_rep$speech_id))



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





# separate into sentences
trump_tweet_data_sentences <- trump_tweet_data %>%
  unnest_tokens(sentence, text, token = "sentences")

# check the balance between retweet and nonretweet
ggplot(trump_tweet_data_sentences, aes(x = isRetweet)) +
  geom_bar()+
  labs(title = "the amount of tweet and retweet by trump")




trump_tweet_data_word <-trump_tweet_data %>%
  rowwise()%>%
  mutate(sum_words = text %>% str_count("\\w+")%>% sum()) %>%
  ungroup() %>%
  unnest_tweets(word,text)






ggplot(trump_tweet_data,aes(x=length_text,fill = isRetweet))+
  geom_density()+facet_grid(isRetweet~.,scales = "free")


ggplot(trump_tweet_data_word,aes(x=sum_words,fill = isRetweet))+
  geom_density()+facet_grid(isRetweet~.,scales = "free")


trump_top_500_ngrams <- trump_tweet_data %>% 
  unnest_tokens(n_grams, text, token = "ngrams", n = 3, n_min = 1) %>% 
  anti_join(stop_words, by = c("n_grams" = "word")) %>%    # drop rows with stop words
  group_by(n_grams) %>%    # group by bigram index
  summarise(n_grams = unique(n_grams), n = n(), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  rowid_to_column(var = "id") %>% 
  filter(id<=200) %>%
print (10)
  
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



trump_tweets_tf_idf_long <- trump_tweet_data %>% 
  unnest_tokens(n_grams, text, token = "ngrams", n = 3, n_min = 1, drop = FALSE) %>% 
  anti_join(stop_words, by = c("n_grams" = "word")) %>% 
  group_by(speech_id, n_grams) %>% 
  summarise(n = n()) %>% 
  bind_tf_idf(n_grams,document = speech_id, n = n) %>% 
  filter(n_grams %in% trump_top_500_ngrams$n_grams) 


set.seed(1234)
cov_split <- initial_split(rep_tf_idf )
cov_train <- training(cov_split)
cov_test <- testing(cov_split)

# specify model ----------------------------------------------------------------


cov_mod <- linear_reg()%>%
  set_engine("lm") 



# build recipe -----------------------------------------------------------------

cov_rec <- recipe(rep ~ total,data=cov_train) 



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







# make predictions for test data -----------------------------------------------


