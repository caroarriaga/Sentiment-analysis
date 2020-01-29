library("dplyr")

#Select text to tokenize
text<-readLines("feed.txt")

#tibble creates data frame with colums: a row per line (int) and a string per line
text_df<- tibble(line= 1:30, text = text)

#tool to tokenize
install.packages("tidytext")
library(tidytext)

#tokenize = one word per element, keeps track of what row it was taken from
text_df<- text_df %>%
  unnest_tokens(word,text)

#remove stop words
text_df<- text_df %>%
  anti_join(stop_words)

#Look at most frequent words
text_df %>% count(word, sort = TRUE)

install.packages("tidyr")
library(tidyr)

#count word frequency
freq<- text_df %>% 
  count(word, sort = TRUE) %>%
  mutate(proportion = n / sum(n))

#plot word frequency
install.packages("scales")
library(scales)

install.packages("ggplot2")
library(ggplot2)

#if wanted to compared among other word sers
# ggplot(freq, aes(x = proportion, y = word, color = abs(word - proportion))) +
#        geom_abline(color = "gray40", lty = 2) +
#       geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      

#available lexicons "afinn", "bing" and "nrc" to get sentiment data
##afinn uses Yes/No, bing uses Positive/negative and nrc scale -5 to 5
#get_sentiments("bing")

#get strings
library("stringr")

feed_sent <- freq %>%
  inner_join(get_sentiments("bing"), by = "word")

feed_ex<- freq %>%
  filter(word %in% feed_association)

library(plyr)
feed_association<- c('loved', 'thankful', 'like', 'good', 'afterwards', 'complicated', 'exam', 'final', 'takes', 'lot')
feedAss_sent<- ldply(feed_association, data.frame)
names(feedAss_sent)[1]<-"word"
feedAss_sent<- feedAss_sent %>%
  inner_join(get_sentiments("bing")) %>%
  right_join(d, by = "word")

sum(feedAss_sent$sentiment == "positive")/length(feedAss_sent$sentiment)
