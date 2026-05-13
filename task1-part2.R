library(rvest)
library(dplyr)

# The comparison of the best-selling men's and women's clothing on dangdang shopping website.
#--------------------------------------------------------------------------------------------------
# 1.extract the info of men's clothing
## name   --> the name of product
## price  --> the price of product
## detail --> the detail of the hot comment
## rate  --> the number of rate about this product

pages1<-paste0('https://category.dangdang.com/pg',1:3,'-cid10010336-srsort_sale_amt_desc.html')

info1<-function(page){  
  url<-read_html(page)  
  p<-html_nodes(url,'.price')  
  price<-html_text(p)
  n<-html_nodes(url,'.name')  
  name<-html_text(n)
  r<-html_nodes(url,'.star')
  rate<-html_text(r)
  d<-html_nodes(url,'.search_hot_word')
  detail<-html_text(d)
  name<-name[1:30]
  price<-price[1:30]
  detail<-detail[1:30]
  rate<-rate[1:30]
  data<-data.frame(
    name=name,
    price=price,
    detail=detail,
    rate=rate)}

sapply(pages1,info1)
re1<-bind_rows(lapply(pages1,info1))
re1$type<-'men'


#-----------------------------------------------------------------------------
# 2.extract the info of women's clothing
## name   --> the name of product
## price  --> the price of product
## detail --> the detail of the hot comment
## rate  --> the number of rate about this product

pages2<-paste0('https://category.dangdang.com/pg',1:3,'-cid4003844-srsort_sale_amt_desc.html')

info2<-function(page){  
  url<-read_html(page)  
  p<-html_nodes(url,'.price')  
  price<-html_text(p)
  n<-html_nodes(url,'.name')  
  name<-html_text(n)
  r<-html_nodes(url,'.star')
  rate<-html_text(r)
  d<-html_nodes(url,'.search_hot_word')
  detail<-html_text(d)
  name<-name[1:30]
  price<-price[1:30]
  detail<-detail[1:30]
  rate<-rate[1:30]
  data<-data.frame(
    name=name,
    price=price,
    detail=detail,
    rate=rate)}

sapply(pages2,info2)
re2<-bind_rows(lapply(pages2,info2))
re2$type<-'women'


#-------------------------------------------------------------
# 3.compare the info between men's clothing and women's clothing
## clean the data
df<-rbind(re1,re2)
sum(is.na(df))
str(df)
df$price <- gsub("[^0-9.]", "", df$price)  # 只留数字
df$price <- as.numeric(df$price)
df$rate <- gsub("[^0-9.]", "", df$rate)  # 只留数字
df$rate <- as.numeric(df$rate)
str(df)


## statistic about data
library(dplyr)
df %>%group_by(type) %>%
  summarise(
    number_of_product=n(),
    avg_price=mean(price),
    low_price=min(price),
    high_price=max(price),
    median_price=median(price),
    sd_price=sd(price),
    avg_name_length=mean(nchar(name)),
    avg_commends=mean(rate,na.rm=TRUE)
  ) %>% print()

df<-df %>%mutate(price_level = case_when(
    price < 100 ~ "low",
    price >=100 & price <=200 ~ "middle",
    price >200 ~ "high"
  ))

table(df$type,df$price_level)

library(ggplot2)
ggplot(df, aes(x=price_level, y=price, fill=type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("men" = "lightblue", "women" = "pink")) +
  ggtitle("Men vs Women Clothing Price")  
