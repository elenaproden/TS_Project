library(gdata)#read.xls
library(tseries)
library(forecast)
library(Ecdat)
library(fpp)
library(tictoc)
library(astsa)#spectrum
library(quantmod)   
library(vars)  
library(fxregime)  
library(moments)  
library(rugarch)  
library(FitAR)
library(tsDyn)#setar
library(stats)
library(latex2exp)

# setwd("E:/Universite de Gen?ve/Facult? d'?conomie/Mon Master en statistique/A1S1/The Statistical Analysis of Time Series/TS_Project")
set.seed(19920913)

# [5. ] -------------------------------------------------------------------

theta=2
X=rep(0,2000);
W=arima.sim(n=2000,list())
for (i in 2:2000){
  X[i]=W[i] + W[i-1]/theta
}
X=ts(X)
plot(ts(X))

# [6. ] -------------------------------------------------------------------

Y=stats::arima(X,order=c(0,0,1),method="CSS")
summary(Y)
theta_est = 1/Y$coef[1]

# [7. ] -------------------------------------------------------------------

autocov = function(k,theta,sigma){
  ans=rep(0,length(k))
  for (i in length(k)){
    if (k[i]==0){
      ans[i]=sigma*(1+1/theta^2)
    }
    else if (k[i]==1 || k[i]==-1){
      ans[i]=sigma*(1/theta)
    }
    else{
      ans[i]=0
    }
    return(ans)
  }
  
}

theta=2
plot(c(0.02,0.02),c(0,autocov(0,theta,1)),type="l",xlim=c(0,2),ylim=c(0,2),xaxt="n",lwd=3,
     main="Series X",xlab="Lag",ylab="ACF")
axis(1,at=seq(0,2,by=1))
lines(c(1.02,1.02),c(0,autocov(1,theta,1)),lwd=3)

theta=1.5
lines(c(0,0),c(0,autocov(0,theta,1)),col="red",lwd=3)
lines(c(1,1),c(0,autocov(1,theta,1)),col="red",lwd=3)


lines(c(0.04,0.04),c(0,autocov(0,theta_est,1)),col="blue",lwd=3)
lines(c(1.04,1.04),c(0,autocov(1,theta_est,1)),col="blue",lwd=3)

legend(1.71,2,legend=c(TeX('$\\theta = 1.5$'),TeX('$\\theta = 2$'),TeX('$\\hat{\\theta} \\approx 2.5 $')),fill=c("red","black","blue"))



# We can see that the estimated theta underestimates the real correlation.
