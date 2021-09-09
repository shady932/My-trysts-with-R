#Lab 7
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(MLmetrics)
setwd('C:/Users/SASWATA/Desktop/Study ebooks/Sem 6/CSE4029/LAB')



dset<-read.table('Sales_lab7.csv',header=TRUE,sep=",",quote="\"'",dec=".",stringsAsFactors=FALSE)



#Question 1

#adding t and t^2 columns
dset<- data.frame(t=seq.int(nrow(dset)),Month=dset$Month,Sales=dset$Sales.in.Dollars)
dset$t2<-'^'(dset$t,2)
buff<-dset

names(dset)
unique(dset$Month)


#creating wide dataset by one-hot encoding month
dset$Month<-substr(dset$Month,4,nchar(dset$Month)-3)
unique(dset$Month)

dM<-dummyVars(~Month,data=dset)
dset2<-data.frame(predict(dM,newdata=dset))

dset<-cbind(dset,dset2[,-c(12)])
dset<-dset[,-c(match("Month",names(dset)))]


#Split the dataset into train&validation set in 4:1 ratio

trows <- floor(0.8 * nrow(dset))

set.seed(77)
tidx <- sample(seq_len(nrow(dset)), size = trows)
train <- dset[tidx, ]
names(train)
validate <- dset[-tidx, ]
names(validate)


#define the models using glm
lm1<-glm(Sales~t,data=train)
lm2<-glm(log(Sales)~t,data=train)
lm3<-glm(Sales~t+t2,data=train)
lm4<-glm(Sales~.,data=train[,-c(1,3)])
lm5<-glm(Sales~t2,data=train)
lm6<-glm(log(Sales)~.,data=train[,-c(1,3)])


#Question2

valid_y<-validate[,"Sales"]
valid_x<-validate[,-c(2)]

min<-Inf
rmse<-c()

pred<-c()
pred_y1<-predict(lm1,valid_x[c(1)],type="response")
pred_y2<-predict(lm2,valid_x[c(1)],type="response")
pred_y3<-predict(lm3,valid_x[c(1,2)],type="response")
pred_y4<-predict(lm4,valid_x[-c(1,2)],type="response")
pred_y5<-predict(lm5,valid_x,type="response")
pred_y6<-predict(lm6,valid_x[-c(1,2)],type="response")

v1<-RMSE(pred_y1,valid_y)
v2<-RMSE(pred_y2,valid_y)
v3<-RMSE(pred_y3,valid_y)
v4<-RMSE(pred_y4,valid_y)
v5<-RMSE(pred_y5,valid_y)
v6<-RMSE(pred_y6,valid_y)

u1<-MAPE(pred_y1,valid_y)
u2<-MAPE(pred_y2,valid_y)
u3<-MAPE(pred_y3,valid_y)
u4<-MAPE(pred_y4,valid_y)
u5<-MAPE(pred_y5,valid_y)
u6<-MAPE(pred_y6,valid_y)

error<-data.frame(Model=c('lm1','lm2','lm3','lm4','lm5','lm6'),RMSE=c(v1,v2,v3,v4,v5,v6),MAPE=c(u1,u2,u3,u4,u5,u6))
print(error)

#Judging from the RMSE and MAPE errors both, model 3 is the best model.


#Question3

#Using the best model and checking if homoscedacity is maintained
lmf<-lm3
res <- resid(lmf)
plot(res, ylab="Residuals", xlab="Data Points", main="Sales Data")
abline(0, 0)

#ACF plot for ARIMA
acf(dset[,!(names(dset) %in% c("Sales"))])


#Question4

#Retrain using whole dataset

lmfr<-glm(Sales~t+t2,data=dset)


#Making new test set where year = 1999
buff$Month<-substr(buff$Month,7,nchar(buff$Month))
test99<-buff[buff[,"Month"]=="99",]
print(test99)

test99_x<-test99[,c(1,4)]
test99_y<-test99[,c(3)]

pred99_y<-predict(lmfr,test99_x)

result<-data.frame(Month=seq(1,12),Actual=test99_y,Predicted=pred99_y)
frmse<-RMSE(pred99_y,test99_y)
fmape<-MAPE(pred99_y,test99_y)
print(result)
print('The final RMSE is: ')
print(frmse)
print('The final MAPE is: ')
print(fmape)
