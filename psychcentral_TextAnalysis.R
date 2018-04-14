
library("tidytext")
library("dplyr")

library("data.table")
install.packages("utf8")
library("utf8")
data <- fread("psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

#print column names
data[0]

text<- data$q_content

text_df<-data_frame(row=data$row, text=text)

tidy_text<-  text_df %>% unnest_tokens(word, text)
head(tidy_text,10)

data(stop_words)
tidy_text<-tidy_text %>%
  anti_join(stop_words)

tidy_text %>% count(word,sort=TRUE)


library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


#STemming the q_content data
install.packages("SnowballC", repos = "https://cran.r-project.org")
library(SnowballC)
tidy_text <- data %>%
  unnest_tokens(word, q_content) %>%
  mutate(word = wordStem(word)) 

#Removing stop words
tidy_text<-tidy_text %>%
  anti_join(stop_words)

#tokens after stemming
tidy_text %>% count(word,sort=TRUE)


library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Creating word Cloud
library(wordcloud)

tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#Sentiment analysis + Word Cloud

install.packages("reshape2", repos = "https://cran.r-project.org")
library(reshape2)

tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)



#Repeating the text analysis steps on the Answers column

#Without stemming


text_df<-data_frame(row=data$row, text=data$answers)
tidy_text_ans<-  text_df %>% unnest_tokens(word,text)


data(stop_words)
tidy_text_ans<-tidy_text_ans %>%
  anti_join(stop_words)

tidy_text_ans %>% count(word,sort=TRUE)


tidy_text_ans %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


#with Stemming on Answers column
tidy_text_answers <- data %>%
  unnest_tokens(word1, answers) %>%
  mutate(word1 = wordStem(word1)) 

data(stop_words)
#Removing stop words
tidy_text_answers<-tidy_text_answers %>% 
  anti_join(stop_words,by = c("word1" = "word"))

#tokens after stemming
tidy_text_answers %>% count(word1,sort=TRUE)

tidy_text_answers %>%
  count(word1, sort = TRUE) %>%
  filter(n > 6000) %>%
  mutate(word1 = reorder(word1, n)) %>%
  ggplot(aes(word1, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Creating word Cloud

tidy_text_answers %>%
  count(word1) %>%
  with(wordcloud(word1, n, max.words = 200))

#Sentiment analysis + Word Cloud

tidy_text_answers %>%
  inner_join(get_sentiments("bing"), by = c("word1" = "word")) %>%
  count(word1, sentiment, sort = TRUE) %>%
  acast(word1 ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)






#Topic Modeling 
library(RTextTools)
library(tm)
library(topicmodels)
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.

lda_td <- tidy(lda)
head(lda_td,5)


top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#trying smaller no. of k
lda_3 <- LDA(dtm.new, k = 10) # k is the number of topics to be found.

lda_td_3 <- tidy(lda_3)
head(lda_td_3,5)


top_terms <- lda_td_3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



#Topic Modeling on Answers
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 14) # k is the number of topics to be found.

lda_td <- tidy(lda)
head(lda_td,5)


top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



