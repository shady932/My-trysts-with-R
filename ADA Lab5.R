setwd('C:/Users/SASWATA/Desktop/Study ebooks/Sem 6/CSE4029/LAB')


bks<-read.csv('Books.csv')

head(bks)
str(bks)
names(bks)


#A>
bks$Content<-gsub('<br> ','',bks$Content)
head(bks,1)
str(bks)


#B>
bks %>%
  group_by(Book) %>%
  summarise() 

tdy_bks<- bks %>%
  unnest_tokens(word, Content)

head(tdy_bks)
unique(tdy_bks$Book)
str(tdy_bks)


#C>
nbks<- tdy_bks %>%
  anti_join(get_stopwords())

str(nbks$word)
head(nbks)

#Compare
dim(tdy_bks)
dim(nbks)



#D>
names(nbks)
head(nbks)
#Drop writters
nbks<-subset(nbks,select=-c(6))
names(nbks)
dim(nbks)

#E>
bing_s<-get_sentiments('bing')

bks_s<- nbks %>%
  left_join(bing_s) %>%
  count(word,sentiment,sort=TRUE) %>%
  mutate(sentiment = ifelse(is.na(sentiment), 'neutral', sentiment))



#F>
plyr::count(bks_s$sentiment)


#G>
head(bks_s,10)


#H>
no_neu<-bks_s[bks_s$sentiment!='neutral',]
dim(no_neu)
no_neu$n<-ifelse(no_neu$sentiment=='negative',yes=no_neu$n*(-1),no=no_neu$n)
head(no_neu)

ggplot(head(no_neu,10),aes(x=word,y=n))+
  geom_segment( aes(x=word,xend=word,y=0,yend=n,colour=sentiment), size=1.3, alpha=0.9)+
  theme_light()



#I>
no_neu<-bks_s[bks_s$sentiment!='neutral',]

nbks %>%
  inner_join(bing_s) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),max.words = 60)


#J>
names(bks_s)
nnbks<-left_join(bks_s,nbks,by='word')

head(nnbks,3)
