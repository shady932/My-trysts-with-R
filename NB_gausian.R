library('janitor')
library('plyr')
library('funModeling')
library('dplyr')
library(moments)
library(e1071)
library(caTools)
library(caret)
library(naivebayes)


#Gausian Naive bayes with brest cancer data

get.f1<-function(cm){
  cd<-confusionMatrix(cm)
  cdcm<-unlist(cd[2])
  TN<-cdcm[1]
  FN<-cdcm[2]
  FP<-cdcm[3]
  TP<-cdcm[4]
  
  precision =(TP)/(TP+FP)
  recall_score =(FP)/(FP+TN)
  f1_score=2*((precision*recall_score)/(precision+recall_score))
  return (as.numeric(f1_score))
}
get.ac<-function(cm){
  cd<-confusionMatrix(cm)
  cdcm<-unlist(cd[2])
  TN<-cdcm[1]
  FN<-cdcm[2]
  FP<-cdcm[3]
  TP<-cdcm[4]
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  False_positive_rate =(FP)/(FP+TN)
  False_negative_rate =(FN)/(FN+TP)
  return (as.numeric(accuracy_model))
}

setwd('C:/Users/SASWATA/Desktop/Study ebooks/Sem 6/CSE4029')
set.seed(69)

dset<-read.csv('data.csv')

str(dset)
dim(dset)

dset<-dset[,-ncol(dset)]
dset$ID <- seq.int(nrow(dset))
dsetX<-dset[,-c(1)]
str(dsetX)

######TEST AREA####
l<-c(1,1,1,1,1)

plyr::count(l)
str(dsetX)

###################

outliers<-function(df,feat){
  outliers.idx<-c()
  
  for (c in feat){
    q1<-as.numeric(summary(df[,c])[2])
    q3<-as.numeric(summary(df[,c])[5])
    
    IQR<-q3-q1
    
    out.range<-IQR*1.5
    outlier.list.column<-df[(df[c] < q1 - out.range)
                            | (df[c] > q3 + out.range),ncol(df)]
    outliers.idx<-append(outliers.idx,outlier.list.column)
    
  }
  freq<-plyr::count(outliers.idx)
  multiple.outliers<-c()
  
  for (i in 1:dim(freq)[1]){
    if(freq[i,2]>2){
      multiple.outliers<-append(multiple.outliers,freq[i,1])
    }
  }
  
  return (multiple.outliers)
  
}

#getting outliers
dsetX.outliers<-outliers(dsetX,names(dsetX[,-1]))


#removing outliers and confirming
dim(dsetX)
dsetX<-dsetX[-dsetX.outliers,]
dsetX<-dsetX[,-ncol(dsetX)]
dim(dsetX)



#Branch0 - no transform no drop no discritize

b0<-dsetX
split0 <- sample.split(b0, SplitRatio = 0.7)
b0.train <- subset(b0, split0 == "TRUE")
b0.test <- subset(b0, split0 == "FALSE")

b0.classifier<-naiveBayes(as.factor(diagnosis)~.,data=b0.train)

b0.pred<-predict(b0.classifier,newdata = b0.test)

cm0<-table(b0.test$diagnosis,b0.pred)
confusionMatrix(cm0)



#transforming to normal distribution
str(dsetX)
moments::skewness(dsetX[,-1])
skew.columns<-c('radius_se','texture_se','perimeter_se','area_se','smoothness_se','compactness_se','concavity_se',
                'concave.points_se','symmetry_se','fractal_dimension_se','concavity_mean','area_mean','area_worst','compactness_worst'
                ,'symmetry_worst','fractal_dimension_worst')

for (s in skew.columns){
  hist(dsetX[,s])
  dsetX[,s]<-log10(dsetX[,s])
  hist(dsetX[,s])
}
moments::skewness(dsetX[,-1])

equal.freq.n<-function(nb,n){
  nb<-equal_freq(nb,n)
  nb<-as.numeric(factor(nb,levels=unique(nb)))
  return (nb)
}




#Branch1- dont drop co related columns and discritize

b1<-dsetX


for (i in 2:(ncol(b1))){
  b1[,i]<-equal.freq.n(b1[,i],9)
}

str(b1)

split <- sample.split(b1, SplitRatio = 0.7)
b1.train <- subset(b1, split == "TRUE")
b1.test <- subset(b1, split == "FALSE")

b1.classifier<-naiveBayes(as.factor(diagnosis)~.,data=b1.train)

b1.pred<-predict(b1.classifier,newdata = b1.test)

cm<-table(b1.test$diagnosis,b1.pred)
confusionMatrix(cm)





#Branch2 - dont drop dont discritize

b2<-dsetX
split2 <- sample.split(b2, SplitRatio = 0.7)
b2.train <- subset(b2, split2 == "TRUE")
b2.test <- subset(b2, split2 == "FALSE")

b2.classifier<-naiveBayes(as.factor(diagnosis)~.,data=b2.train)

b2.pred<-predict(b2.classifier,newdata = b2.test)

cm2<-table(b2.test$diagnosis,b2.pred)
confusionMatrix(cm2)






#Branch3 - drop dont discritize

b3<-subset(dsetX,select=-c(match(c('radius_se','texture_se','perimeter_se','area_se','smoothness_se','compactness_se','concavity_se',
              'concave.points_se','symmetry_se','fractal_dimension_se'),names(dsetX))))

split3 <- sample.split(b3, SplitRatio = 0.7)
b3.train <- subset(b3, split3 == "TRUE")
b3.test <- subset(b3, split3 == "FALSE")

b3.classifier<-naiveBayes(as.factor(diagnosis)~.,data=b3.train)

b3.pred<-predict(b3.classifier,newdata = b3.test)

cm3<-table(b3.test$diagnosis,b3.pred)
confusionMatrix(cm3)







#Branch4 - drop discritize

b4<-subset(dsetX,select=-c(match(c('radius_se','texture_se','perimeter_se','area_se','smoothness_se','compactness_se','concavity_se',
                                   'concave.points_se','symmetry_se','fractal_dimension_se'),names(dsetX))))

for (i in 2:(ncol(b4))){
  b4[,i]<-equal.freq.n(b4[,i],9)
}

split4 <- sample.split(b4, SplitRatio = 0.7)
b4.train <- subset(b4, split4 == "TRUE")
b4.test <- subset(b4, split4 == "FALSE")

#plyr::count(b4.train$diagnosis)
#bs<-sample_n(b4.train[b4.train$diagnosis=="B",],106)
#b4.train<-rbind(b4.train[b4.train$diagnosis=="M",],bs)
#plyr::count(b4.train$diagnosis)

b4.classifier<-naiveBayes(as.factor(diagnosis)~.,data=b4.train)

b4.pred<-predict(b4.classifier,newdata = b4.test)

cm4<-table(b4.test$diagnosis,b4.pred)
confusionMatrix(cm4)




#Bernoulli NB

b4.oh<-b4

dm<-caret::dummyVars(~as.factor(radius_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(texture_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(perimeter_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(area_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(smoothness_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(compactness_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(concavity_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(concave.points_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(symmetry_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(fractal_dimension_mean),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(radius_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(texture_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(perimeter_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(area_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(smoothness_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(compactness_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(concavity_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(concave.points_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(symmetry_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

dm<-caret::dummyVars(~as.factor(fractal_dimension_worst),data=b4)
temp<-data.frame(predict(dm,newdata = b4))
b4.oh<-cbind(b4.oh[-c(2)],temp)

split5 <- sample.split(b4.oh, SplitRatio = 0.7)
b4.oh.train <- subset(b4.oh, split5 == "TRUE")
b4.oh.test <- subset(b4.oh, split5 == "FALSE")


b4.oh.matrix<-as.matrix(b4.oh.train[,-c(1)])

b4.classifier.bernoulli<-bernoulli_naive_bayes(b4.oh.matrix,b4.oh.train[,1],laplace=1)

b4.oh.pred<-predict(b4.classifier.bernoulli,newdata = as.matrix(b4.oh.test[,-c(1)]))

cm5<-table(b4.oh.test$diagnosis,b4.oh.pred)
confusionMatrix(cm5)

#29% improvement in f1 score over control
#Imbalanced dataset F1 score and balanced datasets accuracy

all.models<-list(cm0,cm,cm2,cm3,cm4,cm5)

for (g in all.models){
  print('Accuracy: ')
  print(get.ac(g))
  print('F1 score:')
  print(get.f1(g))
  #print(class(g))
}

#We get the highest accuracy with Bernoulli NB, but the model is overfit due to high dimentionality of one-hot encoded dataset.
#The best alternative is the discritized version of the dataset after dropping highly corelated columns and using Gaussian NB. 

