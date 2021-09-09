library(tidyverse)
library(rgl)

#Question 1:

games<-read.csv("game.csv")
winPct<-games[,7]
rd<-games[,10]
era<-games[,14]

#Plotting the graphs vs. Rdiff and ERA
plot(winPct,rd,xlab='WinPct',ylab='RDiff')
plot(winPct,era,xlab='WinPct',ylab='ERA')


l<-lm(winPct ~ rd + era)
l

plot3d(x = rd,
       y = era,
       z = winPct,
       type = 's',
	 size=2,
       col = 'red',
       xlim = c(min(rd)-1,max(rd)+1),
       ylim = c(min(era)-1,max(era)+1),
       zlim = c(min(winPct)-1,max(winPct)+1),
       xlab = 'Rdiff',
       ylab = 'ERA',
       zlab = 'winPct')


#Part b

x<-cbind(data.frame(B0=rep(1,30)),rd,era)
xm<-data.matrix(x)
y<-data.matrix(winPct)

get_beta<-function(x,y){
	tx<-t(x)%*%x
	tx<-solve(tx)
	#Using solve because ^-1 gave wrong output
	xy<-t(x)%*%y
	beta<-tx%*%xy

	return (beta)
}

beta<-get_beta(xm,y)
#Compare with lm() output
beta
l


#Part c

#Using the beta values to get predicted winPct named 'nz'
nz<-beta[1,1]+beta[2,1]*rd+beta[3,1]*era

plot3d(x = rd,
       y = era,
       z = winPct,
       type = 's',
	 size=1,
       col = 'yellow',
       xlim = c(min(rd)-1,max(rd)+1),
       ylim = c(min(era)-1,max(era)+1),
       zlim = c(min(winPct)-1,max(winPct)+1),
       xlab = 'Rdiff',
       ylab = 'ERA',
       zlab = 'winPct')

lines3d(rd,era,nz,col='red',add=TRUE)


#Question 2


#compute next step of batch GD

rep_pro<-function(xi,al){

get_fx<-function(b0,b1,x){
	s0<-0
	s1<-0
	n<-length(mz)
	for (i in 1:n){
		s0<-s0-mz[i]+b0+b1*x[i]
		s1<-s1-(x[i]*(mz[i]-b0-b1*x[i]))
	}
	f<-c((s0*2)/n,(s1*2)/n)
	return (f)
}

#B_c stores current b0 and b1 values

B_c<-data.frame(B0=c(0),B1=c(0))
fx<-get_fx(B_c[1,1],B_c[1,2],xi)
#instead of declaring alphs, it will be passed as a param

for (i in 1:500){
	b0_up<-B_c[dim(B_c)[1],1]-al*fx[1]
	b1_up<-B_c[dim(B_c)[1],2]-al*fx[2]
	B_c<-rbind(B_c,c(b0_up,b1_up))
	fx<-get_fx(b0_up,b1_up,xi)
}

d<-data.frame(B0=c(B_c[dim(B_c)[1],1]),B1=c(B_c[dim(B_c)[1],2]),ErB0=c(fx[1]),ErB1=c(fx[2]))

return (d)
}

#With X=Rdiff
rep_pro(mx,0.001)

#With X=OPS
rep_pro(data.matrix(games[,11]),0.001)

#With X=HomeRun (homerun has been decimal normalized with j=2
rep_pro(data.matrix(games[,12])/100,0.001)

