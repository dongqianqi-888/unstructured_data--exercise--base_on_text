library(tm) # for text mining
library(SnowballC) # for text stemming
library(wordcloud) # word-cloud generator
library(RColorBrewer) # color palettes
library(syuzhet) # for sentiment analysis
library(ggplot2) # for plotting graphs


# Load the data as a corpus
text<-readLines('ai_distillation_comments.txt')
text <-text[8:length(text)]
docs <- Corpus(VectorSource(text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "=")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, '<')
docs <- tm_map(docs, toSpace, '>')
docs <- tm_map(docs, toSpace, ':')
docs <- tm_map(docs, toSpace, 'https:')
docs <- tm_map(docs, toSpace, 'can')
docs <- tm_map(docs, toSpace, 'github.com')
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, removeWords, stopwords("english"))
# docs <- tm_map(docs, removeWords, c("s", "company","team"))
# docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, stemDocument)

# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
dtm_m <- as.matrix(dtm)
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE) # Sort by descending value of frequency
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
head(dtm_d, 5) # Display the top 5 most frequent words

## Generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))
library(wordcloud2)
wordcloud2(dtm_d)


## Sentiment scores
text <- sapply(docs, as.character)
text <- text[text != ""]
text <- text[!grepl("^\\s*$", text)]

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods have different scales
syuzhet_vector <- get_sentiment(text, method="syuzhet")
head(syuzhet_vector)
head(syuzhet_vector,10) # see the first 10 elements of the vector
summary(syuzhet_vector)
# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)
#afinn
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)
#nrc
nrc_vector <- get_sentiment(text, method="nrc")
head(nrc_vector)
summary(nrc_vector)
#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)))

## Find the most common positive and negative words
bing_dict <- get_sentiment_dictionary("bing")
words <- unlist(strsplit(tolower(text), "\\s+"))
matched <- words[words %in% bing_dict$word]
library(dplyr)
matched_df <- data.frame(word = matched) %>%
  inner_join(bing_dict, by = "word")

word_counts <- matched_df %>%
  group_by(value, word) %>%   # 用 value 而不是 sentiment
  summarise(freq = n(), .groups = "drop") %>%
  arrange(desc(freq))
word_counts %>% filter(value == "1")
word_counts %>% filter(value == "-1")

## Emotion classification
# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score : 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# and if the sentiment is positive or negative
d<-get_nrc_sentiment(text)
head (d,10) # head(d,10) - just to see top 10 lines
#Visualization
td<-data.frame(t(d)) #transpose
td_new <- data.frame(rowSums(td)) #The function rowSums computes column sums across rows for each level of a grouping variable.
names(td_new)[1] <- "count" #Transformation and cleaning
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot 1 - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment,ylab="count")+ggtitle("Survey sentiments")
#Plot 2 - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage")
