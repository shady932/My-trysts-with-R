library('binaryLogic')
library("plyr")
library("caTools")
library(caret)
library('car')
library('dplyr')


one_hot<-function(data,i){
	prnt<-data[,i]
	name<-names(data)[i]
	data<-data[,-c(i)]
	l<-dim(data)[2]
	nd<-data.frame(data[,i])
	for (v in unique(prnt)){
		tmp<-ifelse(prnt==v,yes=1,no=0)
		tmp<-as.numeric(tmp)
		#tmp<as.factor(tmp)
		d_t<-data.frame(tmp)
		names(d_t)[1]<-paste(name,"_",v,sep="")
		nd<-cbind(nd,d_t)
	}
	data<-cbind(data[,1:i-1],nd[,-c(1)],data[,i:l])
	return (data)
}



bin_en2 <- function(x, order = unique(x), name = "v_") {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x2 <- as.binary(x)
  maxlen <- max(sapply(x2, length))
  x2 <- lapply(x2, function(y) {
    l <- length(y)
    if (l < maxlen) {
      y <- c(rep(0, (maxlen - l)), y)
    }
    y
  })
  d <- as.data.frame(t(as.data.frame(x2)))
  rownames(d) <- NULL
  colnames(d) <- paste0(name, 1:maxlen)
  d
}
