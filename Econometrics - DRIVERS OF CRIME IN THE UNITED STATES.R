#Clear Console
rm(list=ls())
cat("\014")

#Scientific Notation
options("scipen"=99999, digits=3)

setwd("C:/Users/shane/OneDrive/UC Work files/Econometrics/Final Project")

#install.packages("vtable")
#install.packages("lm.beta")


library("readxl")  
library(tidyverse)
library(stargazer)
library("car")
library(lm.beta)
library(data.table)
library(lmtest)
library(sandwich)
library("pscl")
library(mfx)
library(texreg)
library(dplyr)
library(ggplot2)
library(vtable)

prjdata <- read_excel("Project Dataset.xlsx")
data <- drop_na(prjdata)
str(data)


summary(data)

# For crime - do we remove outliers or use logs?
# for population - take logs
# For Per_capita_income - do we remove outliers or use logs?

# Tests to be done on data
# Plot scatter plots for each X against Y
# Test for Heteroskedasticity - WHite test/WLS/GLS
# Plot scatter plots of each X against its own residuals - Should show homoskedasticity


########### Regressions ##########################


# Convert Per Capita Income to Logs to avoid Heteroskedasticity
data$log_pcincome <- log(data$Per_capita_income)

str(data)
summary(data)


statstable <- dplyr::select(data, Crime_percentage, Poverty_percent, Educated_adults, Per_capita_income, Unemployment_rate, Capital_punishment, Male_percentage, Old_age_percentage, African_American_percentage, log_pcincome)
colnames(statstable) <- c("Crime Rate","Poverty %","% of Educated adults","Per capita income","Unemployment Rate","Capital Punishment","% of Males","% of Old people","% of African Americans", "Log Per capita Income")
Sumstats <- sumtable(statstable)
st(statstable, digits = 3, fixed.digits = TRUE)
Sumstats

attach(prjdata)
ols <- lm(Crime_percentage ~  Poverty_percent + Educated_adults + log_pcincome +  Unemployment_rate + Capital_punishment, data = data )
summary(ols)

Aols <- lm(Crime_percentage ~  Poverty_percent + Educated_adults + log_pcincome +  Unemployment_rate + Capital_punishment + Male_percentage + Old_age_percentage + African_American_percentage, data = data)
summary(Aols)

# Stargazer with the Basic and Advanced model
stargazer(ols,Aols,type="text",align=TRUE,
          keep.stat=c("n","rsq","f"), no.space=FALSE,df=FALSE, 
          column.labels =c("Base Model","Advanced Model"),
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)

# Testing for Heteroskedasticity
## (1) Graphical Examination?
data$residualSQ<-ols$residuals^2
data$fit<-ols$fitted.values # here we generate the fit variable from the OLS

plot(y=data$residualSQ, x=data$fit) 
abline(0, 0) 

data$residualSQ_V2<-ols$residuals^2
data$fit_V2<-ols$fitted.values # here we generate the fit variable from the OLS


##  Basic model -  Heteroskedasticity Graphical Examination
Hetero_Test1 <- ggplot(data, mapping = aes(x = data$fit, y =data$residualSQ)) +
        geom_point(size=3,colour = "#FF0000") + 
        labs( x = "Predicted Values",
            y = "Residuals Squared" ,
            title = "Basic Model ", 
            subtitle = "Graphical examination of Heteroskedasticity") +
            theme_classic()
Hetero_Test1

##  Advanced model -  Heteroskedasticity Graphical Examination
Hetero_Test2 <- ggplot(data, mapping = aes(x = data$fit_V2, y =data$residualSQ_V2)) +
  geom_point(color = "#0099f9") + 
      labs( x = "Predicted Values",
      y = "Residuals Squared" ,
      title = "Advanced Model ", 
      subtitle = "Graphical examination of Heteroskedasticity") + 
      theme_classic()
Hetero_Test2 


# Breusch-Pagan Test for Heteroskedasticity
bptest(ols, data=data)
bptest(Aols, data=data)

# White test for Heteroskedasticity
bptest(ols, ~fit+I(fit^2), data=data)
bptest(Aols, ~fit+I(fit^2), data=data)

# Investigating VIF, to see if any variables have a VIF of over 10.
vif(ols)
vif(Aols)

# Calculating AIC/BIC
AIC(ols)
BIC(ols)

AIC(Aols)
BIC(Aols)


### If we have Heteroskedasticity problem, then we report Robost Std Error
# HC3 option with small sample bias correction (consistent with STATA robust option)
OLSrobustse<-coeftest(ols, vcov = vcovHC(ols, "HC3"))
AOLSrobustse<-coeftest(Aols, vcov = vcovHC(Aols, "HC3"))

summary(OLSrobustse)
summary(AOLSrobustse)




# Stargazer with all 4 models, including the Robust Standard errors
stargazer(ols,Aols,OLSrobustse,AOLSrobustse,type="text",align=TRUE,
          keep.stat=c("n","rsq","f","aic","bic"), no.space=FALSE,df=FALSE, 
          title="Analysis of Crime Rate" ,
          column.labels =c("Basic OLS","Advanced OLS","RobostSE Basic OLS", "RobostSE Advanced OLS"),
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)

### For Presentation -  Stargazer with only Robust Standard error models ###
stargazer(OLSrobustse,AOLSrobustse,type="text",align=TRUE,
          keep.stat=c("n","rsq","f"), no.space=FALSE,df=FALSE, 
          column.labels =c("RobostSE Basic OLS", "RobostSE Advanced OLS"),
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)

## Beta Coefficients

bc1<-lm.beta(ols)
summary(bc1)

bc2<-lm.beta(Aols)
summary(bc2)

# Stargazer with Robust Standard error models using Beta Coefficients
stargazer(bc1,bc2,type="text",align=TRUE,
          keep.stat=c("n","rsq","f"), no.space=FALSE,df=FALSE, 
          column.labels =c("Basic OLS Beta Coeff", "Advanced OLS Beta Coeff"),
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)



######################################## Further Models Tested ##########################################

Model3 <- lm(Crime_percentage ~  Poverty_percent + Educated_adults + Per_capita_income +  Unemployment_rate + Capital_punishment +  Legal_marijuana , data = data )
summary(Model3)
Model3robustse<-coeftest(Model3, vcov = vcovHC(ols, "HC3"))


Model4 <- lm(Crime_percentage ~   Educated_adults + Unemployment_rate + Capital_punishment + Female_percentage + Middle_age_percentage + African_American_percentage, data = data)
summary(Model4)
Model4robustse<-coeftest(Model4, vcov = vcovHC(Aols, "HC3"))

# Stargazer with only Robust Standard error models
stargazer(Model3robustse,Model4robustse,type="text",align=TRUE,
          keep.stat=c("n","rsq","f"), no.space=FALSE,df=FALSE, 
          column.labels =c("RobostSE Basic OLS", "RobostSE Advanced OLS"),
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)

###############################################################################################################################

