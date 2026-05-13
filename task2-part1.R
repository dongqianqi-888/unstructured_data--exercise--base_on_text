library(tm)
l_h<-VCorpus(DirSource('news/health'))
l_f<-VCorpus(DirSource('news/finance'))

clean<-function(docs){
  toSpace<-content_transformer(function(x,pattern){return(gsub(pattern," ",x))})
  docs<-tm_map(docs,removePunctuation)
  docs<-tm_map(docs,toSpace,"\\’")
  docs<-tm_map(docs,toSpace,"\\-")
  docs<-tm_map(docs,toSpace,"\\-")
  docs<-tm_map(docs,toSpace,"\\-")
  docs<-tm_map(docs,toSpace,"\\‘")
  docs<-tm_map(docs,toSpace,"\\—")
  docs<-tm_map(docs,content_transformer(tolower))
  docs<-tm_map(docs,toSpace,"new")
  docs<-tm_map(docs,toSpace,"fox")
  docs<-tm_map(docs,removeNumbers)
  docs<-tm_map(docs,removeWords,stopwords("english")) #remove stop words
  docs<-tm_map(docs,stripWhitespace)
  return(docs)}
l_h<-clean(l_h)
l_f<-clean(l_f)

library(textstem)
docsH<-stem_strings(sapply(l_h, as.character))
docsF<-stem_strings(sapply(l_f, as.character))

wcloud<-function(docs){
  dtm<-DocumentTermMatrix(docs)
  freq<-colSums(as.matrix(dtm))
  print(paste0('A total of ',length(freq),' words were counted.'))
  print('The high frequency words include:')
  wf<-data.frame(names(freq),freq)
  names(wf)<-c("TERM","FREQ")
  wf<-wf[order(-wf$FREQ),]
  print(head(wf,10))
  library(wordcloud2)
  wordcloud2(wf,size=0.8)
}
wcloud(docsH)
wcloud(docsF)


#-----------------------------------------------------------------------------
# LDA model for health
library(tidytext)
library(topicmodels)
library(tidyr)
library(ggplot2)
library(dplyr)

dtm_h <- DocumentTermMatrix(docsH)
h_lda<-LDA(dtm_h,k=2,control=list(seed=1234)) 
h_topics<-tidy(h_lda,matrix="beta") 

## Extract the per-topic-per-word-probabilities
h_top_terms<-h_topics%>%group_by(topic)%>%
  top_n(10,beta)%>%ungroup()%>%arrange (topic, -beta)

h_top_terms%>%mutate(term=reorder(term,beta))%>% 
  ggplot(aes(term,beta,fill=factor(topic)))+geom_col(show.legend=FALSE)+
  facet_wrap(~topic,scales="free")+coord_flip() 

## Compare two topics
beta_spread<-h_topics%>%mutate(topic=paste0("topic",topic))%>%
  spread(topic,beta)%>%filter (topic1>0.0033 | topic2 > 0.0034)%>% 
  mutate(log_ratio = log2(topic2/topic1))

beta_spread%>%mutate(term=reorder(term,log_ratio))%>% 
  ggplot(aes(term,log_ratio))+geom_col(show.legend=FALSE)+coord_flip()

## Extract the per-document-per-topic-probabilities
h_documents<-tidy(h_lda,matrix="gamma") 
h_documents%>%filter(gamma>=0.99)%>%group_by(topic)%>%tally()
h_documents%>%filter(gamma>=0.99)%>%group_by(topic)
h_documents%>%filter(gamma<=0.99 & gamma>0.5)%>%group_by(topic)
h_documents%>%filter(gamma>=0.99 & topic==2)
h_documents%>%filter(gamma<=0.99 & gamma>0.5 & topic==2)
tidy(dtm_h)%>%filter(document==5)%>%arrange(desc(count)) # 100% belong with topic1
tidy(dtm_h)%>%filter(document==8)%>%arrange(desc(count)) # 70% belong with topic1
tidy(dtm_h)%>%filter(document==2)%>%arrange(desc(count)) # 100% belong with topic2
tidy(dtm_h)%>%filter(document==30)%>%arrange(desc(count)) # 90% belong with topic2


# LDA model for financial issues
dtm_f <- DocumentTermMatrix(docsF)
f_lda<-LDA(dtm_f,k=2,control=list(seed=1234)) 
f_topics<-tidy(f_lda,matrix="beta") 

#Extract the per-topic-per-word-probabilities
f_top_terms <- f_topics %>% group_by(topic) %>% top_n(10,beta) %>% ungroup () %>% arrange (topic, -beta)
f_top_terms%>% mutate(term=reorder(term,beta))%>% 
  ggplot(aes(term,beta,fill=factor(topic)))+geom_col(show.legend=FALSE)+
  facet_wrap(~topic,scales="free")+coord_flip() 

## Compare two topics
beta_spread<-f_topics%>%mutate(topic=paste0("topic",topic))%>%
  spread(topic,beta)%>%filter(topic1>0.003| topic2 > 0.004)%>% 
  mutate(log_ratio = log2(topic2/topic1))

beta_spread%>%mutate(term=reorder(term,log_ratio))%>% 
  ggplot(aes(term,log_ratio))+geom_col(show.legend=FALSE)+coord_flip()

## Extract the per-document-per-topic-probabilities
f_documents<-tidy(f_lda,matrix="gamma") 
f_documents%>%filter(gamma>=0.99)%>%group_by(topic)%>%tally()
f_documents%>%filter(gamma>=0.99 & topic==1)%>%arrange(-gamma)
f_documents%>%filter(gamma>=0.99 & topic==2)%>%arrange(-gamma)
f_documents%>%filter(gamma<=0.99 & gamma>0.5 & topic==1)%>%arrange(-gamma)
f_documents%>%filter(gamma<=0.99 & gamma>0.5 & topic==2)%>%arrange(-gamma)
tidy(dtm_f)%>%filter(document==1)%>%arrange(desc(count)) #100% belong with topic1
tidy(dtm_f)%>%filter(document==6)%>%arrange(desc(count)) #100% belong with topic2
tidy(dtm_f)%>%filter(document==33)%>%arrange(desc(count)) #62.6% belong with topic1
tidy(dtm_f)%>%filter(document==22)%>%arrange(desc(count)) #78.5% belong with topic2
