#clearing R scripts and making notation used easier to read
rm(list=ls())
cat("\014") 
options("scipen"=99999, digits=3)

#setting working directory for the research
setwd("C:/Users/Senal Peiris/OneDrive/Desktop/Masters Studies/Capstone/Project")

#loading needed libraries for the analysis

library(dplyr)
library(urca)
library(vars)
library(dynlm)
library(tsDyn)
library(quantmod)
library(tseries)
library(urca)
library(forecast)
library(vtable)
library(vars)



#loading data and converting data to numerics where needed
data <- read.csv("Data.csv")
str(data)

data$Non_Farm_Emp <- as.numeric(data$Non_Farm_Emp)    
str(data)

#converting data into a time series for analysis of variables

dfts = ts(data, start = c(1988, 1), frequency = 4) #frequency of data is quarterly


#Reviewing the time series data and checking for the order of integration
source(file="intord.R")
#plot.ts(dfts)#plotting all variables

par(mfrow=c(3,1))
plot(dfts[,2], main="Loan Loss Reserve Ratio",type ="l", ylab="Ratio",xlab="Year", col="Red")
plot(dfts[,3], main="Net Interest Margin",type ="l", ylab="Rate",xlab="Year", col="Red")
plot(dfts[,4], main="Loans Net Charge-off Rate",type ="l", ylab="Rate",xlab="Year", col="Red")
plot(dfts[,5], main="NASDAQ Composite Index",type ="l", ylab="Index",xlab="Year", col="Red")
plot(dfts[,6], main="Consumer Confidence Index",type ="l", ylab="Index",xlab="Year", col="Red")
plot(dfts[,7], main="Non-Farm Emoployees",type ="l", ylab="No in Thousands",xlab="Year", col="Red")
plot(dfts[,8], main="Federal Funds Effective Rate",type ="l", ylab="Rate",xlab="Year", col="Red")


par(mar=c(2,2,2,2))
intord(data$Loan_loss_reserve_ratio)#I1 order of integration
intord(data$NIM)#I1 order of integration
intord(data$Loans_netchargeoff)#I1 order of integration
intord(data$Nasdaq)#I1 order of integration
intord(data$Consumer_confidence)#I1 order of integration
intord(data$Non_Farm_Emp)#I2 order of integration
intord(data$FED_Funds)#I1 order of integration

#summary table of variables
dta2 <- data[-c(4,7)]
st(dta2)

#######Setting up all the variables for the analysis

LLR <- dfts[,2]
NIM <- dfts[,3]
NASDAQ <- dfts[,5]
CC <- dfts[,6]
EMP<- dfts[,7]
FED <- dfts[,8]


##taking  first differences of the variables of interest
dLLR <- diff(LLR)
dNIM <- diff(NIM)
dNASDAQ <- diff(NASDAQ)
dCC <- diff(CC)
dEMP <- diff(EMP)
ddEMP <- diff(dEMP)
dFED <- diff(FED)


intord(dLLR)#I0 order of integration
intord(dNIM)#I0 order of integration
intord(dNASDAQ)#I0 order of integration
intord(dCC)#I0 order of integration
intord(dEMP)#I1 order of integration
intord(ddEMP)#I0 order of integration
intord(dFED)#I0 order of integration



########Checking cointegration for VECM#####################


#####checking engle-granger method
#c1 <- dynlm(FED~LLR+NIM+NASDAQ+CC) #Possible

c1 <- dynlm(CC~LLR+NIM+NASDAQ+FED) # Very GOOD

dy <- cbind(dCC,dLLR,dNIM,dNASDAQ,dFED) # variables I(0)
ly <- cbind(CC,LLR,NIM,NASDAQ,FED) # variables I(0)

###using varselect to determing the optimum number of lags to be used
VARselect(ly, lag.max=12, type="const")

summary(c1)
ecm <- c1$residuals
dev.off()
par(mar = c(2.5,2.5, 2.5, 2.5))
intord(ecm)
Acf(ecm)
Pacf(ecm)

#create a lag for ECM term,
ec <- embed(ecm,2)
ecm1 <- ec[,2]


# VECM with Engle-Granger
var5 <- VAR(dy, p=2, type="cons",exogen=ecm1,season=4)
summary(var5)


####next i will check all the variables to see if there is any serial correlation in the residuals
# BoX-Ljung Q Statistic for Consumer Confidence
resi = var5$varresult$dCC$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b

blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt

# BoX-Ljung Q Statistic for Loan Loss Reserve Ratio
resi = var5$varresult$dLLR$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b

blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt

#BoX-Ljung Q Statistic for Net Interest Margin
resi = var5$varresult$dNIM$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b

blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt

#BoX-Ljung Q Statistic for Nasdaq
resi = var5$varresult$dNASDAQ$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b

blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt

#BoX-Ljung Q Statistic for FED
resi = var5$varresult$dFED$residuals
b = Box.test(resi,lag = 20, type="Ljung-Box")
b

blt = rep(0,20)
for (i in 1:20){
  b = Box.test(resi,lag = i, type="Ljung-Box")
  blt[i]=b$p.value
}
blt


###########impulse response function#####################
irfsp3a <- irf(var5, impulse = "dNIM", response = c("dLLR"), ci=0.95)
plot(irfsp3a, main = "Impact of Net Interest Margin on Loan Loss Reserve Ratio")

irfsp3b <- irf(var5, impulse = "dNASDAQ", response = c("dLLR"), ci=0.95)
plot(irfsp3b,main = "Impact of NASDAQ on Loan Loss Reserve Ratio")

irfsp3c <- irf(var5, impulse = "dFED", response = c("dLLR"), ci=0.95)
plot(irfsp3c,main = "Impact of Federal Funds Rate on Loan Loss Reserve Ratio")

irfsp3d <- irf(var5, impulse = "dCC", response = c("dLLR"), ci=0.95)
plot(irfsp3d,main = "Impact of Consumer Confidence on Loan Loss Reserve Ratio")


# Variance Decompositions (contribution of each variable to predicting a variable)
par(mfrow=c(5,2))
vard <- fevd(var5, n.ahead=8)
vard
vard$dLLR

plot(vard,col=1:5)


#####Granger Causality

CC_Granger <- causality(var5,cause ="dCC")
CC_Granger

NIM_Granger <- causality(var5,cause ="dNIM")
NIM_Granger

NASDAQ_Granger <- causality(var5,cause ="dNASDAQ")
NASDAQ_Granger

FED_Granger <- causality(var5,cause ="dFED")
FED_Granger




