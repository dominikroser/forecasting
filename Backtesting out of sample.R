          # RECURSIVE SCHEME "EXAMPLE CODE" 
          # ------------------------------
# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
# getwd()
# ----------------------------------------------
library(fBasics)
library(forecast) 
# ----------------------------------------------
# READ THE DATA
datos<-read.csv("Datos CO2.csv",header=TRUE,sep=",",dec=".")
y<-datos[,4] 
# brent dated weekly spot prices

ts.plot(y) 

n<-length(y)

n.estimation<-612 # 
n.forecasting<-n-n.estimation # 120 observations
horizontes<-2 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)

s<-12

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[1:(n.estimation-Periods_ahead+i)]; # Recursive
    # aux.y<-y[i:(n.estimation-Periods_ahead+i)]; # Rolling
    # No logs
    fit<-arima(aux.y,order=c(0,0,0),seasonal=list(order=c(0,0,0),period=s))
    # With logs
    #fit<-arima(log(aux.y),order=c(0,0,0),seasonal=list(order=c(0,0,0),period=s))
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead]; # no logs
  # error<-real-exp(predicc[,Periods_ahead]); # with logs
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100; # in percentage
}
