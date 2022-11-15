install.packages("rtweet")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("textdata")
install.packages("forestmangr")
library(rtweet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(textdata)
library(forestmangr)
library(ggplot2)
rm(list=ls())
#retrieving the tweets that use the hashtag
tweets <- search_tweets(q = "#kendricklamar",
n = 5000,
include_rts = FALSE,
`-filter` = "replies",
lang = "en")
#Getting tweets from the timeline of kendrick lamar and graphing them over the years
k_tweets <- get_timeline("@kendricklamar", n= 2000)
colnames(k_tweets)[colnames(k_tweets)=="screen_name"] <- "Twitter_Account"
#Graphing the frequency of tweets over years
ts_plot(group_by(k_tweets, Twitter_Account), "month") +
theme_minimal() +
theme(plot.title = element_text(face = "bold")) +
labs(
x = NULL, y = NULL,
title = "Frequency of Tweets from Kendrick Lamar Over the Past Decade"
)
#Graphing the frequency of tweets including #KendrickLamar over the past few days
ts_plot(tweets, by = "hours") +
theme_minimal() +
theme(plot.title = element_text(face = "bold")) +
labs(x = NULL, y = NULL,
title = "Frequency of tweets with a #KendrickLamar hashtag",
subtitle = paste0(format(min(tweets$created_at), "%d %B %Y"), " to ",
format(max(tweets$created_at),"%d %B %Y")),) +
theme_minimal()
#Plotting the most used hashtags along with #KendrickLamar
hash <- tweets %>%
unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
filter(str_detect(hashtag, "^#"),
hashtag != "#KendrickLamar",
hashtag != "#kendricklamar") %>%
count(hashtag, sort = TRUE) %>%
top_n(10)
hash <- hash %>%
rename(
number = n
)
dfh <- data.frame(hash)
dfh$hashtag <- factor(dfh$hashtag, levels = dfh$hashtag[order(dfh$number)])
ggplot(dfh, aes(x = number, y = hashtag)) +
geom_bar(stat = "identity")
#Plotting the cities most tweeted from
locations <- tweets %>%
filter(!is.na(place_full_name)) %>%
count(place_full_name, sort = TRUE) %>%
top_n(5)
locations <- locations %>%
rename(
number = n,
place = place_full_name
)
dfl <- data.frame(locations)
ggplot(dfl)
dfl$place <- factor(dfl$place, levels = dfl$place[order(dfl$number)])
ggplot(dfl, aes(x = number, y = place)) +
geom_bar(stat = "identity")
#Sentiment analysis of tweets containing the hashtag
tweets.Celeb <- tweets %>% select(screen_name, text)
tweets.Celeb$stripped_text <- gsub("http\\S+","",tweets.Celeb$text)
tweets.Celeb_stem <- tweets.Celeb %>%
select(stripped_text) %>%
unnest_tokens(word, stripped_text)
cleaned_tweets.Celeb <- tweets.Celeb_stem %>%
anti_join(stop_words)
bing_celeb = cleaned_tweets.Celeb %>%
inner_join(get_sentiments("bing")) %>%
count(word, sentiment, sort = TRUE) %>%
ungroup()
bing_celeb %>%
group_by(sentiment) %>%
top_n(10) %>%
ungroup() %>%
mutate(word = reorder(word, n)) %>%
ggplot(aes(word, n, fill = sentiment)) +
geom_col(show.legend = FALSE) +
facet_wrap(~sentiment, scales = "free_y") +
labs(title = "Tweets containing '#KendrickLamar' ",
y = "Contribution to Sentiment",
x = NULL) +
coord_flip() + theme_bw()