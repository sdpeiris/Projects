##clearing environment
rm(list=ls())
cat("\014") 

library("dplyr")
library("quantmod")
library("forecast")
library("dynlm")
library(tseries)
library(urca)
library(lubridate)
library(forecast)
library(dynlm)
library(zoo)


source(file="intord.R")

setwd("C:/Users/Senal Peiris/OneDrive/Desktop/Masters Studies/Spring Semester/8011- Time series econometrics/Homework")

#loading data
House_DB <- read.csv("Medianhouseprice_unadjusted.csv")
Fed_DB <-read.csv("FEDFUNDS_unadjusted.csv")
Unem_DB <-read.csv("Unemployment_unadjusted.csv")
GDP_DB <-read.csv("GDP_unadjusted.csv")


#checking the data
str(House_DB)
str(Fed_DB)
str(Unem_DB)
str(GDP_DB)

#creating a database for econometric analysis
#final dataset used will be "Data"
DB <- merge(GDP_DB,Unem_DB, by = 'DATE')
DB <- merge(DB,Fed_DB, by = 'DATE')
DB <- merge(DB,House_DB, by = 'DATE')


#renaming columns
DB <- DB %>%
rename(
  Int_rate = FEDFUNDS,
  House_price = MSPUS,
  Unemp = UNRATENSA,
  GDP = NA000334Q
  )

#taking logs of GDP and house price due to spread of data
DB$GDP <- log(DB$GDP)
DB$House_price <- log(DB$House_price)

#converting data to numeric format and date format
DB$Int_rate <- as.numeric(DB$Int_rate)
DB$DATE <- ymd(DB$DATE)
str(DB)

#Data_final <- na.omit(Data)
#Data_final$week <- week(ymd(Data_final$DATE))

attach(DB)

#########part a)################
#using intord function to check the order of integration
par(mar = c(2, 2, 2, 2))
intord(House_price) #I(1) order of integration
intord(Int_rate)#I(1) order of integration
intord(Unemp)#I(0) order of integration

#coverting the final data into a time series
dfts = ts(DB,frequency= 4, start=c(1963,01))

#plotting data to visually inspect the time series
plot.ts(dfts)

#par(mar = c(1, 1, 1, 1))
#plot.ts(dfts[,2],xlab = "Year")
#plot.ts(dfts[,3],xlab = "Year")
#plot.ts(dfts[,4],xlab = "Year")
#plot.ts(dfts[,5],xlab = "Year")

#taking the first differenced variables as time series has the same number of observations

dUNP <- diff(dfts[,3])
dIT <- diff(dfts[,4])
dHP <- diff(dfts[,5])

seas = seasonaldummy(dHP) # set dummy for the shortest time series


###############part b)#######################
#starting analysis with 3 years worth of lags
r1 <- dynlm(dHP~L(dHP,1:12)+L(dUNP,0:12)+L(dIT,0:12)+seas)
summary(r1)
AIC(r1)
BIC(r1)


#reduced lags from 3 years of lags to 9 lags to see impact on AIC/BIC and can see it reduces hence r1b is a better model
r1b <- dynlm(dHP~L(dHP,1:9)+L(dUNP,0:9)+L(dIT,0:9)+seas)
summary(r1b)
AIC(r1b)
BIC(r1b)

# reduced the number of lags to the highest lag which is significant to see if further lags can be dropped
r1c <- dynlm(dHP~L(dHP,1:9)+L(dUNP,0:5)+L(dIT,0:5)+seas)
summary(r1c)
AIC(r1c)
BIC(r1c)

#based on r1c model we use the start date as 1964-3 as we cannot improve further by changing number of lags

#r2 model will now be the base model with a time set on which we will
#try to remove further lags to see which are the most significant on house
r2 <- dynlm(dHP~L(dHP,1:9)+L(dUNP,0:5)+L(dIT,0:5)+seas, start = c(1965,3))
summary(r2)
AIC(r2)
BIC(r2)

r3 <- dynlm(dHP~L(dHP,1:9)+L(dUNP,0:5)+L(dIT,0:1)+seas, start = c(1965,3))
summary(r3)
AIC(r3)
BIC(r3)
# we can see that AIC/BIC improves when the lags of interest rates are removed
#as they are still insignificant we will completely remove the lags

r4 <- dynlm(dHP~L(dHP,1:5)+L(dUNP,0:5)+ seas, start = c(1965,3))
summary(r4)
AIC(r4)
BIC(r4)
#AIC/BIC further improved, will try to remove seasonal dummies to see if model improves based on annova test


r5 <- dynlm(dHP~L(dHP,1:5)+L(dUNP,0:5), start = c(1965,3))
summary(r5)
AIC(r5)
BIC(r5)

anova(r4, r5, test="F")
#We reject null hence there seasonality dummies are significant and should be kept in the model
#hence the best model for us is r4

#Check serial correlation
library(lmtest) #--- No serial correlation 
bgtest(r4, order = 1)
bgtest(r4, order = 2)
bgtest(r4, order = 3)
bgtest(r4, order = 4)
bgtest(r4, order = 5)
bgtest(r4, order = 6)


#granger causality
#best model is shown below
r4 <- dynlm(dHP~L(dHP,1:5)+L(dUNP,0:5)+ seas, start = c(1965,3))
summary(r4)
AIC(r4)
BIC(r4)

####testing is unemployment granger causes houseprices #######
r6 <- dynlm(dHP~L(dHP,1:5)+seas, start = c(1965,3))
summary(r6)
AIC(r6)
BIC(r6)

anova(r4, r6, test="F")
#since p value is significant it rejects the null
#hence unemployment granger causes house prices

####testing if house prices granger causes houseprices #######

r7 <- dynlm(dHP~L(dUNP,0:5)+ seas, start = c(1965,3))
summary(r7)
AIC(r7)
BIC(r7)
anova(r4, r7, test="F")

#since p value is significant it rejects the null
#hence houseprices granger causes house prices



