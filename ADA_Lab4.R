library(ggpubr)
library(tm)
library(lubridate)
library(dplyr)
library(plotly)
library(scales)

library(RSentiment)
#install.packages("RSentiment")
library(stringr)
library(broom)
library(tidyr)
#install.packages("tidytext")
library(tidytext)


#A)
library(janeaustenr)


#B)
austen_books_df=as.data.frame(austen_books(),stringAsFactors=F)


#C)
austen_books() %>% group_by(book) %>%
  summarise()


#D)
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

pc<-dplyr::filter(original_books,book=='Pride & Prejudice')
pc


#E)

tb_pc <- pc %>%
  unnest_tokens(word, text)
tb_pc
str(tb_pc)
dim(tb_pc)


#F)

tb_pc %>%
  group_by(book,chapter) %>%
  count(word, sort = TRUE) 



#G)

tidy_books<- original_books %>%
  unnest_tokens(word,text)

#tidy_books

bw<-tidy_books %>%
  group_by(book) %>%
  count(word, sort = TRUE)
bw

#H)

btw<-tidy_books %>%
  group_by(book) %>%
  summarise(word=n())
btw


#I)

i_ans<-dplyr::left_join(bw,btw,by=c('book'))
names(i_ans)<-c('book','word','n','total')
i_ans


#J)

plot<-i_ans %>%
  bind_tf_idf(word,book,n)
plot


##########

sense<-dplyr::filter(plot,book=='Sense & Sensibility')

sense<-sense%>%top_n(15)

sep<-ggplot(data=sense, aes(x=reorder(word,tf_idf), y=tf_idf)) +
  geom_bar(stat="identity",fill='blue') +
  labs(x="",y="",title="Sense & Sensibility") + coord_flip()

sep

#############
prejudice=dplyr::filter(plot,book=='Pride & Prejudice')
prejudice<-prejudice%>%top_n(15)

prp<-ggplot(data=prejudice, aes(x=reorder(word,tf_idf), y=tf_idf)) +
  geom_bar(stat="identity",fill='pink') +
  labs(x="",y="",title="Pride & Prejudice") + coord_flip()

prp

#############

Park<-dplyr::filter(plot,book=='Mansfield Park')
Park<-Park%>%top_n(15)

Pap<-ggplot(data=Park, aes(x=reorder(word,tf_idf), y=tf_idf)) +
  geom_bar(stat="identity",fill='yellow') +
  labs(x="",y="",title="Mansfield Park") + coord_flip()

Pap

####################
Emma<-dplyr::filter(plot,book=='Emma')
Emma<-Emma%>%top_n(15)

Emp<-ggplot(data=Emma, aes(x=reorder(word,tf_idf), y=tf_idf)) +
  geom_bar(stat="identity",fill='green') +
  labs(x="",y="",title="Emma") + coord_flip()

Emp

####################
Abbey<-dplyr::filter(plot,book=='Northanger Abbey')
Abbey<-Abbey%>%top_n(15)

Abp<-ggplot(data=Abbey, aes(x=reorder(word,tf_idf), y=tf_idf)) +
  geom_bar(stat="identity",fill='orange') +
  labs(x="",y="",title="Northanger Abbey") + coord_flip()

Abp

################
Persuasion<-dplyr::filter(plot,book=='Persuasion')
Persuasion<-Persuasion%>%top_n(15)

Pep<-ggplot(data=Persuasion, aes(x=reorder(word,tf_idf), y=tf_idf)) +
  geom_bar(stat="identity",fill='purple') +
  labs(x="",y="",title="Persuasion") + coord_flip()

Pep


figure <- ggarrange(sep, prp, Pap, Emp, Abp, Pep,ncol = 2, nrow = 3)
figure