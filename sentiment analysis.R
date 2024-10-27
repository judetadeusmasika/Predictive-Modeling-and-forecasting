## loading the necessary packages
library(tidyverse)
library(tm)
library(gt)
##load the dataset
apple <-read.csv("Data1.csv")
apple |> 
  head(n=10) |> 
  gt()
##skim through th structure of the data
str(apple)
## we are only interested in the "text" column
##we load the text or vector data as a corpus
corpus <-iconv(apple$text)
corpus <-Corpus(VectorSource(corpus))
inspect(corpus[1:5])
## clean text
##In the cleaning process first, we need to convert all the text into lower case.
##Based on tm_map function can convert text into lower case.
corpus <-tm_map(corpus, tolower)
inspect(corpus[1:5])
##Cleaning the text data one of the major parts is removing special characters from the text. 
##This is done using the tm_map() function to replace all kinds of special characters.
corpus <-tm_map(corpus, removePunctuation)
inspect(corpus[1:7])
##in the text data, numbers commonly occur, we need to remove numbers from the text data
corpus <-tm_map(corpus, removeNumbers)
inspect(corpus[1:10])
##Stop words are the most commonly occurring words in a language and have very little
##value in terms of extracting useful information from the text. Need to remove all 
##stopwords from the text before the analysis.
##Stop words mean like “the, is, at, on”.  stopwords in the tm_map() function supports
##several languages like English, French, German, Italian, and Spanish.
corpus <-tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus[1:10])
##depends on your dataset, if links contain the dataset, remove the same
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
cleanset <- tm_map(corpus, content_transformer(removeURL))
inspect(cleanset[1:10])
##text stemming-reduces words to their root form
cleanset <-tm_map(cleanset, removeWords, c("aapl","apple"))
cleanset <-tm_map(cleanset, gsub,
pattern ="stocks",
replacement ='stock')
library(SnowballC)
cleanset <-tm_map(cleanset, stemDocument)
cleanset <-tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])
## term document matrix
##After cleansing the textual content data, the following step is 
##to matter the incidence of every word, to perceive famous or trending topics. 
##Using the function TermDocumentMatrix() from the textual content mining package, 
##you may construct a Document Matrix – a table containing the frequency of words.
tdm <-TermDocumentMatrix(cleanset)
tdm <-as.matrix(tdm)
tdm[1:10,1:20]
## bar plot
##Plotting the words using a bar chart is a good basic way to
##visualize this word’s frequent data. Simply you can create a bar chart for visualization.
w<-rowSums(tdm)
w <-subset(w, w>=25)
barplot(w,
labs = 2,
col= rainbow(50))
##word cloud
##A word cloud is one of the most popular ways to visualize and analyze text data. It’s an image composed
##of keywords found within a body of text, where the size of each word indicates its count in that body of text.
library(wordcloud)
w <-sort(rowSums(tdm), decreasing = T)
set.seed(222)
wordcloud(words = names(w),
freq = w,
max.words = 150,
random.order = F,
min.freq = 5,
colors = brewer.pal(8,"Dark2"),
scale = c(5,0.3),
rot.per = 0.7)
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)


## sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

tweets <-iconv(apple$text)
##obtain sentiment scores
s <-get_nrc_sentiment(tweets)
s |> 
  head(n=10) |> 
  gt()
##Its ranging from anger to trust, Negative and Positive.
## bar plot
barplot(colSums(s), 
labs=2,
col=rainbow(10),
ylab="Count",
main="Sentiment Scores Tweets")
##Sentiment scores more on negative followed by anticipation and positive, trust and fear.

