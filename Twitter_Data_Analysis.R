### LOADING MY PACKAGES
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(wordcloud)
library(tidyr)
library(reshape2)
library(wordcloud)
library(scales)
library(gt)

### CRYPTOCURRENCIES TWEETS
# start time "2020-02-19 02:58:09 UTC"
# stop time  "2020-02-21 04:50:06 UTC"
crypto_tweets = search_tweets(q='#cryptocurrencies', n=2000, include_rts = FALSE)
head(crypto_tweets, n = 5)
start_time = min(crypto_tweets$created_at)  
end_time = max(crypto_tweets$created_at)
crypto_tweets$source
crypto_tweets$screen_name
crypto_tweets$text
crypto_tweets$location

#### crypto users
crypto_users = search_users(q='#cryptocurrencies', n = 1000)
nrow(crypto_users)
length(unique(crypto_users))

table(is.na(crypto_users$location))

## Function to plot the users
plot_users = function(data){
  data %>%
    count(location, sort = TRUE) %>%
    mutate(location = reorder(location, n)) %>%
    mutate(prop = n/sum(n)) %>%
    top_n(20) %>%
    ggplot(aes(x = location, y = n)) +
    geom_text(aes(label = percent(prop)),  hjust = -1) +
    geom_col(fill='blue') +
    coord_flip() + 
    labs(x = "Users Location", y = "Count",
         title = "Users data (top 20 rows) on hashtag: Cryptocurrencies",
         subtitle = 'CRYPTOCURRENCIES')
  
}
plot_users(crypto_users)

### PICKING THE COLUMNS AM INTERESTED IN MY ANALYSIS
crypto_dataframe= data.frame(
  time_sent = crypto_tweets$created_at,
  screen_name = crypto_tweets$screen_name,
  tweet = crypto_tweets$text
)

head(crypto_dataframe, 4)
head(crypto_dataframe$tweet, 4)

table(is.na(crypto_dataframe$time_sent))
table(is.na(crypto_dataframe$screen_name))
table(is.na(crypto_dataframe$tweet))

min(crypto_dataframe$time_sent)
max(crypto_dataframe$time_sent)

#### CLEANING THE TWEETS
### PROCESSING THETWEETS
## removing links and stop words
crypto_dataframe$tweet = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", crypto_dataframe$tweet)
head(crypto_dataframe, 4)

crypto_clean_tweets = crypto_dataframe %>%
  select(tweet)  %>%
  unnest_tokens(word, tweet)

head(crypto_clean_tweets, 10)

### REMOVING STOP WORDS
crypto_clean_tweets = crypto_clean_tweets %>% 
  anti_join(stop_words) %>%
  filter(!word == "rt")


head(crypto_clean_tweets,10)
head(crypto_clean_tweets$word, 4)
length(crypto_clean_tweets$word)
nrow(crypto_clean_tweets)
# ploting word count for tweets
plot_tweets = function(tweets){
  tweets %>%
    count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n)) %>%
    mutate(prop = n/sum(n)) %>%
    top_n(30) %>%
    ggplot(aes(x = word, y = n)) +
    geom_text(aes(label = percent(prop)),  hjust = -1) +
    geom_col(fill='blue') +
    coord_flip() + 
    labs(x = "Crypto words", y = "Count",
         title = "Word Count",
         subtitle = 'CRYPTOCURRENCIES')
  
}
plot_tweets(crypto_clean_tweets)

#### PAIRED WORDS ANALYSIS
### WORDS THAT ARE BEING USED TOGETHER
crypto_tweets_paired = crypto_dataframe %>%
  select(tweet) %>%
  unnest_tokens(paired_words, tweet, token = "ngrams", n = 2) %>%
  count(paired_words, sort = TRUE)

crypto_tweets_paired

#### sentimental analysis
get_sentiments('bing')
sentiment_crypto_counts = crypto_clean_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
sentiment_crypto_counts

### TOTALS BY NUMBERS
table(sentiment_crypto_counts$sentiment)
prop.table(table(sentiment_crypto_counts$sentiment))*100

sentiment_crypto_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_text(aes(label = percent(prop)),  hjust = 0) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Opinions of people on cryptocurrencies",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


##### WORDCLOUD
crypto_clean_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by()


crypto_clean_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray", "blue"),
                   max.words = 100)


      