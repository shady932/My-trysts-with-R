library(dplyr)
library(magrittr)


##read csv as matrix 


##!.
df = read.csv("Ratings.csv")
dfm <- as.matrix(df) 
dfm


dim(dfm)
set.seed(1024)

##2.
#user matrix
X<-matrix(runif(16),nrow=8,ncol=2)
X

##3.
#item matrix 
Y<- matrix(runif(14),nrow=7,ncol=2)
Y


K<-2

mat_fact <- function(dfm, X, Y, K, steps=10000, alpha=0.0002, beta=0.02){
  Y<-t(Y)
  for (step in 1:steps){
    for (i in 1:nrow(dfm)){
      for (j in 1:ncol(dfm)){
        if (dfm[i,j] > 0){
          eij <- dfm[i,j] - (X[i,] %*% Y[,j])
          
        }
        
        for (k in 1:K){
          X[i,k] <- X[i,k] + alpha * ((2 * eij * Y[k,j]) - (beta * X[i,k]))
          Y[k,j] <- Y[k,j] + alpha * ((2 * eij * X[i,k]) - (beta * Y[k,j]))
        }
      }
    }
    
    eR = (X %*% Y)
    
    e = 0
    
    for(i in 1:nrow(dfm)){
      for(j in 1:ncol(dfm)){
        if (dfm[i,j] > 0){
          e <- e + (dfm[i,j] - (X[i,] %*% Y[,j]))^2
          for(k in 1:K) {
            e = e + (beta/2) * (X[i,k]^2) + (Y[k,j]^2)
          } 
        }
      }
    }
    
    
    if (e < 0.001){
      break
    }
    
  }
  Y<-t(Y)
  newList <- list("X" = X, "Y" = Y)
  return(newList)
}


a <-mat_fact(dfm, X, Y, K)
X <- a[[1]]
Y <- a[[2]]
Y <- t(Y)
ans <- (X %*% Y)
ans
