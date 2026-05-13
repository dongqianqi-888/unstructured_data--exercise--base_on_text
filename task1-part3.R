l1<-read.csv("lyrics/good_time.csv")
l2<-read.csv("lyrics/creeping_up_on_you.csv")
l3<-read.csv("lyrics/alive.csv")
l4<-read.csv("lyrics/time_machine.csv")
l5<-read.csv("lyrics/see_you_again.csv")

library(tm)
d1<-Corpus(VectorSource(l1))
d2<-Corpus(VectorSource(l2))
d3<-Corpus(VectorSource(l3))
d4<-Corpus(VectorSource(l4))
d5<-Corpus(VectorSource(l5))

clean<-function(docs){
  docs<-tm_map(docs,removePunctuation)
  docs<-tm_map(docs,content_transformer(tolower))
  docs<-tm_map(docs,removeNumbers)
  docs<-tm_map(docs,removeWords,stopwords("english")) #remove stop words
  docs<-tm_map(docs,stripWhitespace)
  return(docs)}

cd1<-clean(d1)
cd2<-clean(d2)
cd3<-clean(d3)
cd4<-clean(d4)
cd5<-clean(d5)

library(textstem)
scd1<-stem_strings(sapply(cd1, as.character))
scd2<-stem_strings(sapply(cd2, as.character))
scd3<-stem_strings(sapply(cd3, as.character))
scd4<-stem_strings(sapply(cd4, as.character))
scd5<-stem_strings(sapply(cd5, as.character))

wcloud<-function(docs){
  dtm<-DocumentTermMatrix(docs)
  freq<-colSums(as.matrix(dtm))
  print(paste0('A total of ',length(freq),' words were counted.'))
  print('The high frequency words include:')
  wf<-data.frame(names(freq),freq)
  names(wf)<-c("TERM","FREQ")
  wf<-wf[order(-wf$FREQ),]
  print(head(wf,20))
  library(wordcloud2)
  wordcloud2(wf,size=0.8)
}

wcloud(scd1)
wcloud(scd2)
wcloud(scd3)
wcloud(scd4)
wcloud(scd5)
