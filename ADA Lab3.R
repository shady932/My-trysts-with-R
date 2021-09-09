#Question 1
#Part1

setwd('C:/Users/SASWATA/Desktop/Study ebooks/Sem 6/CSE4029/LAB')

dset<-read.table('lab3_data.csv',header=TRUE,sep=",",quote="\"'",dec=".",stringsAsFactors=FALSE)

head(dset)

str(dset)

#Dropping column c1
dset<-subset(dset,select=-c(1))


#C2
str(dset)
sapply(dset, function(x) sum(is.na(x)))


dset$c2[dset$c2== 'Zero'] <- 0
dset$c2[dset$c2== '4+'] <- 4
dset$c2 #check recoding

dset$c2<-as.integer(dset$c2)

#Binary encoding

dset<-cbind(dset[,1:match("c2",names(dset))-1],bin_en2(dset$c2,name="C2_"),
		dset[,(match("c2",names(dset))+1):dim(dset)[2]])


#C3
str(dset$c3)

count(dset,'c3')

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c3!="Unknown",]

unique(dset$c3) #Confirming distinct values

tm<-c()

for (v in dset$c3){
	tmp<-as.numeric(gsub("([0-9]+).*$", "\\1", unlist(strsplit(v,'-'))))
	tm<-append(tm,mean(tmp))
}
print(tm)

dset$c3<-tm

head(dset)

#C4
str(dset$c4) #Identifying datatype

unique(dset$c4) #Identifying distinct values

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c4!="Unknown",]

unique(dset$c4) #Confirming distinct values

count(dset,'c4')

i<-match("c4",names(dset))#Count 

#Using one_hot to encode c4 to dummy
dset<-one_hot(dset,i)

head(dset) #Confirming one hot encoding

#C5
str(dset$c5) #Identifying datatype

unique(dset$c5) #Identifying distinct values

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c5!="Unknown",]

unique(dset$c5) #Confirming distinct values

i<-match("c5",names(dset))

#Using one_hot to encode c5 to dummy
dset<-one_hot(dset,i)

head(dset)

#C6
str(dset$c6) #Identifying datatype

unique(dset$c6) #Identifying distinct values

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c6!="Unknown",]

unique(dset$c6) #Confirming distinct values

i<-match("c6",names(dset))

#Using one_hot to encode c5 to dummy
dset<-one_hot(dset,i)

head(dset)


#C7
str(dset$c7) #Identifying datatype

unique(dset$c7) #Identifying distinct values

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c7!="Unknown",]

unique(dset$c7) #Confirming distinct values

i<-match("c7",names(dset))

#Using one_hot to encode c5 to dummy
dset<-one_hot(dset,i)

head(dset)


#C8
str(dset$c8) #Identifying datatype

unique(dset$c8) #Identifying distinct values

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c8!="Unknown",]

unique(dset$c8) #Confirming distinct values

dset$c8<-as.numeric(factor(dset$c8,levels=unique(dset$c8),exclude=NULL))
dset$c8<-as.factor(dset$c8)
str(dset$c8)

#Requires binary
dset<-cbind(dset[,1:match("c8",names(dset))-1],bin_en2(dset$c8,name="C8_"),
		dset[,(match("c8",names(dset))+1):dim(dset)[2]])



head(dset)


#C9
str(dset$c9) #Identifying datatype

unique(dset$c9) #Identifying distinct values
#No Unknown/NA values
#Integer columns are faster than strings. 'Yes' == 1 and 'No' == 0

dset$c9<-(as.numeric(ifelse(dset$c9=='Yes',yes=1,no=0)))

str(dset$c9)
head(dset) #Confirming

#C10
str(dset$c10) #Identifying datatype

unique(dset$c10) #Identifying distinct values

count(dset,'c10')

i<-match("c10",names(dset))

#Using one_hot to encode c5 to dummy
dset<-one_hot(dset,i)

head(dset)



#C11
str(dset$c11) #Identifying datatype

unique(dset$c11) #Identifying distinct values

#Too many distinct values, so checking variabce of attribute
var(dset$c11)

#Since variance is sufficient, we dont drop column.
#but we need to drop 0 values

dset$c11[dset$c11==0]<-NA
unique(dset$c11) #confirming NA injection

#Drop rows with NAs
dim(dset)
dset<-dset[complete.cases(dset),]
dim(dset)
unique(dset$c11) #Confirm NA drop


#C12
str(dset$c12) #Identifying datatype

unique(dset$c12) #Identifying distinct values

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c12!="Unknown",]

unique(dset$c12) #Confirming distinct values

#Requires binary

dset<-cbind(dset[,1:match("c12",names(dset))-1],bin_en2(dset$c12,name="C12_"),
		dset[,(match("c12",names(dset))+1):dim(dset)[2]])


head(dset)


#C13
str(dset$c13) #Identifying datatype

unique(dset$c13) #Identifying distinct values
#Too many unique values. Checcking varience.
length(unique(dset$c13))

c13<-as.numeric(factor(dset$c13,levels=unique(dset$c13),exclude=NULL))
var(c13)
#Extreme high varience. Leads to overfitting. So we drop the column.

dset<-subset(dset,select=-c(match('c13',names(dset))))

head(dset) #Confirming drop


#C14

str(dset$c14) #Identifying datatype

unique(dset$c14) #Identifying distinct values
#Too many unique values. Checcking varience.
length(unique(dset$c14))

c14<-as.numeric(factor(dset$c14,levels=unique(dset$c14),exclude=NULL))

var(c14)

#Extreme high varience. Leads to overfitting. So we drop the column.

dset<-subset(dset,select=-c(match('c14',names(dset))))

head(dset) #Confirming drop


#C15
str(dset$c15) #Identidying datatype.

unique(dset$c15) #Identifying distinct values

length(unique(dset$c15)) #Checking number of unique values.

var(dset$c15) #Varience is in within bounds, so column is not dropped yet.

#Checking frequency of each unique value
count(dset,'c15')
#Large number of 0 are observed. Checking percentage:

count(dset,'c15')[1,2]/dim(dset)[1] #0 takes 61% of the row.



#C16

str(dset$c16) #Identidying datatype.

unique(dset$c16) #Identifying distinct values

dset$c16[is.na(dset$c16)==TRUE] #No NA values

length(unique(dset$c16)) #Checking number of unique values.

var(dset$c16) #Varience is in within bounds, so column is not dropped yet.

#Checking frequency of each unique value
count(dset,'c16')

#Numerical column with okayish varience. Kept as it is.


#C17

str(dset$c17) #Identidying datatype.

unique(dset$c17) #Identifying distinct values

dset$c16[is.na(dset$c17)==TRUE] #No NA values

length(unique(dset$c17)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c17')

var(dset$c17)
#Variance is normal. Column is left as it is.


#C18

str(dset$c18) #Identidying datatype.

unique(dset$c18) #Identifying distinct values

dset$c16[is.na(dset$c18)==TRUE] #No NA values

length(unique(dset$c18)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c18')

var(dset$c18)
#Varience is a bit too much but the column is kept.


#C19

str(dset$c19) #Identidying datatype.

unique(dset$c19) #Identifying distinct values

dset$c16[is.na(dset$c19)==TRUE] #No NA values

length(unique(dset$c19)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c19')

var(dset$c19)
#Varience is normal. The column is  also numerical, so it is kept.


#C20

str(dset$c20) #Identidying datatype.

unique(dset$c20) #Identifying distinct values

dset$c16[is.na(dset$c20)==TRUE] #No NA values

length(unique(dset$c20)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c20')

var(dset$c20)
#Varience is normal. The column is  also numerical, so it is kept.


#C21

str(dset$c21) #Identidying datatype.

unique(dset$c21) #Identifying distinct values

dset$c16[is.na(dset$c21)==TRUE] #No NA values

length(unique(dset$c21)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c21')

var(dset$c21)
#Varience is high but the column is  also numerical, so it is kept.


#C22

str(dset$c22) #Identidying datatype.

unique(dset$c22) #Identifying distinct values

dset$c16[is.na(dset$c22)==TRUE] #No NA values

length(unique(dset$c22)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c22')

var(dset$c22)
#Varience is normal. The column is  also numerical, so it is kept.


#C23

str(dset$c23) #Identidying datatype.

unique(dset$c23) #Identifying distinct values

dset$c16[is.na(dset$c23)==TRUE] #No NA values

length(unique(dset$c23)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c23')

var(dset$c23)
#Varience is normal. The column is  also numerical, so it is kept. Useful.


#C24

str(dset$c24) #Identidying datatype.

unique(dset$c24) #Identifying distinct values

dset$c16[is.na(dset$c24)==TRUE] #No NA values

length(unique(dset$c24)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c24')

var(dset$c24)
#Varience is low. The column is  also numerical, so it is kept. Useful.


#C25

str(dset$c25) #Identidying datatype.

unique(dset$c25) #Identifying distinct values

dset$c16[is.na(dset$c25)==TRUE] #No NA values

length(unique(dset$c25)) #Checking number of unique values.

#Checking frequency of each unique value
count(dset,'c25')

var(dset$c25)
#Varience is high. The column is numerical, so it is kept.


#C26 Class Lable

str(dset$c26) #Identifying datatype.

unique(dset$c26) #Identifying distinct values

#Recoding 1 as 1 and 2 as 0

dset$c26<-as.numeric(ifelse(dset$c26==1,yes=1,no=0)) 

unique(dset$c26) #Checking

head(dset)


#C27
str(dset$c27) #Identifying datatype

unique(dset$c27) #Identifying distinct values

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c27!="Unknown",]

unique(dset$c27) #Confirming distinct values

i<-match("c27",names(dset))

#Using one_hot to encode c5 to dummy
dset<-one_hot(dset,i)

head(dset)


#C28

str(dset$c28) #Identifying datatype.

unique(dset$c28) #Identify distinct values.

#Since 'Unknown' will not help categorise even if we create dummy, we just drop it.
dset<-dset[dset$c28!="Unknown",]

#Checking
unique(dset$c28)

#Binary is required.

dset<-cbind(dset[,1:match("c28",names(dset))-1],bin_en2(dset$c28,name="C28_"),
		dset[,(match("c28",names(dset))+1):dim(dset)[2]])


head(dset)


#C29

str(dset$c29) #Identifying datatype.

unique(dset$c29) #Identify distinct values.

#Lot of unique values, checking count.

length(unique(dset$c29))

#Checking frequency of each unique value
count(dset,'c29')

var(dset$c29)
#Varience is normal. The column is numerical, so it is kept.


#C30

str(dset$c30) #Identifying datatype.

unique(dset$c30) #Identify distinct values.

#Lot of unique values, checking count.

length(unique(dset$c30))

#Checking frequency of each unique value
count(dset,'c30')

var(dset$c30)
#Varience is normal. The column is numerical, so it is kept.


#C31

str(dset$c31) #Identifying datatype.

unique(dset$c31) #Identify distinct values.

#Lot of unique values, checking count.

length(unique(dset$c31))

#Checking frequency of each unique value
count(dset,'c31')

var(dset$c31)
#Varience is normal. The column is numerical, so it is kept.


#C32

str(dset$c32) #Identifying datatype.

unique(dset$c32) #Identify distinct values.

#Lot of unique values, checking count.

length(unique(dset$c32))
dim(dset)
#Almost all of the data is unique, so we drop this column.



names(dset)
#Now we will drop column c10_Yes as it is inversely corelated with c10_No
#and column c27_Female for the same reason.

dset<-subset(dset,select=-c(match(c('c10_Yes','c27_Female'),names(dset))))

#Confirming drop
names(dset)

#Normalizing required columns.

mima_n<-function(x){
	x<-(x- min(x)) /(max(x)-min(x))
	return (x)
}


lst<-c('c3','c11','c15','c16','c17','c18','c19',
		'c20','c21','c22','c23','c24','c25','c29','c30','c31','c32')


for (v in lst){

	dset[,match(v,names(dset))]<-mima_n(dset[,match(v,names(dset))])
	print(range(dset[,match(v,names(dset))]))
}


str(dset)

#Checking for dropable columns.

count(dset,'C2_1')
count(dset,'C8_1')
count(dset,'C12_1')
count(dset,'C28_1')

count(dset,'C2_3')
count(dset,'C8_4')
count(dset,'C12_4')
count(dset,'C28_4')

#Huge imbalance observed. Since this was generated due to encoding
#and is not a feature of the data itself, we drop this column.

dset<-subset(dset,select=-c(match('C2_1',names(dset))))
str(dset)

#Part2

#Splitting training and test data

set.seed(69)
sample = sample.split(dset,SplitRatio = 0.80) 
train1 =subset(dset,sample ==TRUE) 
test1=subset(dset, sample==FALSE)

tr1<-sample_n(train1[train1$c26 == 1 , ] , 570)
tr2<-sample_n(train1[train1$c26 == 0 , ] , 570)

train<-rbind(tr1,tr2)


#Applying logistic regression

formula<-c26~.

model<-glm(formula,data=train,family="binomial")

summary(model)

#Dropping column with NA

train<-subset(train,select=-c(match('c4_Divorced/Separated',names(train))))
test<-subset(test1,select=-c(match('c4_Divorced/Separated',names(test1))))

model<-glm(formula,data=train,family="binomial")

summary(model)

car::vif(model)


pred<-predict(model,test,type = "response")
print(pred)
summary(pred)

p<-ifelse(pred>0.5,yes=1,no=0)
confusionMatrix(as.factor(p),as.factor(test$c26))
