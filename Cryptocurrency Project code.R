#preparing the environment
rm(list=ls())
options("scipen"=99999, digits=3)

#setwd("C:/Users/Senal Peiris/OneDrive/Desktop/6010 project")
setwd("C:/Users/shane/OneDrive/UC Work files/Econ Data Analysis/Project")

#install.packages("AER") 
#install.packages("tidyverse")
#install.packages("car")
#install.packages("gmodels")
#install.packages("stargazer")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("xts")
#install.packages("bayesm")
#install.packages("blsR")
#install.packages("PerformanceAnalytics")
#install.packages("corrplot")
#install.packages("Hmisc")
#install.packages("ggthemes")
#install.packages("gganimate")
#install.packages("transformr")
#install.packages("magick")
#install.packages("gifski")
#install.packages("av")


#*************************************************************************************** 
#*Packages
#**********************************************************************************
library(AER) #applied econometrics in R
library(tidyverse)#Data wrangling/cleansing
library(car)#Hypothesis testing
library(gmodels)#model fitting
library(stargazer)#Regression data visualization 
library(dplyr)#Data wrangling/cleansing
library(lubridate)#This package help to convert data into date/time formats
library(xts)#dealing with date/time materials
library(bayesm) #Do bayesian analysis using DID model to factor effects of COVID on Crypto prices
library(blsR) #to scrape data from the BLS for analysis
library(readr)  #read data(CSV,etc)
library(corrplot)  # Correlation coef and matrix
library(PerformanceAnalytics)#econometrics package for financial instruments
library(ggthemes) # Used for visualization and adding themes to plots/charts
library(gganimate)#animating visualizations
library(transformr)#manipulating visualizations
library(magick)#animating visualizations for markdowns
library(gifski)#animating visualizations for markdowns
library(av)#animating visualizations for markdowns
#*************************************************************************************** 
#*Data Import
#**********************************************************************************
####3(a) Load the Datasets
### Dependant Variable

dta_doge <- read_csv("Dogecoin.csv")
dta_eth <- read_csv("Ethereum.csv")
dta_bitcoin <- read_csv("bitcoin.csv")
dta_teth <- read_csv("Tether.csv")
data <- read.csv("CPI for All Urban Consumers.csv",header=FALSE)
dta_Nasdaq <- read.csv("NASDAQ 100 Tech.csv",header=TRUE)
dta_COVID <- read.csv("Covid-Binary.csv",header=TRUE)
dta_Interest <- read.csv("FEDFUNDS.csv",header=TRUE)

# Loading data for CPI  and data transformation
# if loading from BLS CPI <- get_series_table('CUUR0000SA0L1E',NA , 2010,2022)
#*************************************************************************************** 
#*Data Transformation
#**********************************************************************************
### 3(b) Import and Transformation
## CREATING DOGE MONTHLY AVERAGES
dta_doge$Date <- as.POSIXlt( dta_doge$Date, format="%m/%d/%Y %H:%M" )
#round dates down to Month
dta_doge$month <- floor_date(dta_doge$Date, "month")

#Aggregate daily details to by Month level
doge_month <- dta_doge %>%
  group_by(month) %>%
  summarize("avg_opening_price" = mean(Open))
names(doge_month)[1] ="date"


# CREATING ETHERIUM MONTHLY AVERAGES
dta_eth$Date <- as.POSIXlt( dta_eth$Date, format="%m/%d/%Y %H:%M" )
#round dates down to week
dta_eth$month <- floor_date(dta_eth$Date, "month")

##Aggregate daily details to by Month level
eth_month <- dta_eth%>%
  group_by(month) %>%
  summarize("avg_opening_price" = mean(Open))
str(dta_eth)
names(eth_month)[1] ="date"

## CREATING BITCOIN MONTHLY AVERAGES
dta_bitcoin$Date <- as.POSIXlt( dta_bitcoin$Date, format="%m/%d/%Y %H:%M" )
#round dates down to Month
dta_bitcoin$month <- floor_date(dta_bitcoin$Date, "month")

#find mean sales by Month
bitcoin_month <- dta_bitcoin%>%
  group_by(month) %>%
  summarize("avg_opening_price" = mean(Open))
names(bitcoin_month)[1] ="date"

#Tether data formatting
dta_teth$Date <- as.POSIXlt( dta_teth$Date, format="%m/%d/%Y %H:%M" )

#round dates down to Month
dta_teth$month <- floor_date(dta_teth$Date, "month")

##Aggregate daily details to by Month level
teth_month <- dta_teth%>%
  group_by(month) %>%
  summarize("avg_opening_price" = mean(Open))
names(teth_month)[1] ="date"



#CPI DATA TRANSFORMATION
#removing top rows
data <- data[-c(0,1,2,3,4,5,6,7,8,9,10,11), ] 

#removing extra unnecesary columns
data <- data[ -c(5:6) ]

#making headers from the new columns and then removing it from the data
colnames(data) <- as.character(data[1,])
data <- data[-c(1), ] 

#changing values in period to be a month
#str(dta_CPI)
data[data == "M01"] <- "01/01"
data[data == "M02"] <- "02/01"
data[data == "M03"] <- "03/01"
data[data == "M04"] <- "04/01"
data[data == "M05"] <- "05/01"
data[data == "M06"] <- "06/01"
data[data == "M07"] <- "07/01"
data[data == "M08"] <- "08/01"
data[data == "M09"] <- "09/01"
data[data == "M10"] <- "10/01"
data[data == "M11"] <- "11/01"
data[data == "M12"] <- "12/01"

data <- subset(data,Period != "S02")
data <- subset(data,Period != "S01")


#creating a new column named date and taking out the spaces inbetween data
data$Date <- gsub(" ", "",paste(data$Period,"/",data$Year))

dta_CPI <- data %>%
  select(Date,Value)

dta_CPI$Date <- mdy(dta_CPI$Date)
dta_CPI$Date <- as.POSIXlt( dta_CPI$Date, format="%m/%d/%Y %H:%M")

names(dta_CPI)[2] ="cpi"
dta_CPI$Date <- as.POSIXlt(dta_CPI$Date, format="%m/%d/%Y %H:%M")

dta_CPI <- select(dta_CPI,Date, cpi )
# Altered CPI from char to numeric
dta_CPI$cpi <- as.numeric(as.character(dta_CPI$cpi))

# NASDAQ data transformation

dta_Nasdaq$Date <- ymd(dta_Nasdaq$Date)
dta_Nasdaq$Date <- as.POSIXlt( dta_Nasdaq$Date, format="%Y/%m/%d")

dta_Nasdaq <- select(dta_Nasdaq,Date, Adj.Close)

# Covid data transformation


dta_COVID$Date <- mdy(dta_COVID$Date)
dta_COVID$Date <- as.POSIXlt( dta_COVID$Date, format="%Y/%m/%d")

# federal interest rates data transformation  

names(dta_Interest)[2] ="Interest Rate"
names(dta_Interest)[1] ="Date"

dta_Interest$Date <- ymd(dta_Interest$Date)
dta_Interest$Date <- as.POSIXlt( dta_Interest$Date, format="%m/%d/%Y %H:%M")



#********************************************************************************
#*******************JOINS***********************************************************
#********************************************************************************
colnames(dta_CPI) <- c("date","cpi")
colnames(dta_COVID) <- c("SNo","date","ba_covid")
colnames(dta_Nasdaq) <- c("date","nasdaq_close")
colnames(dta_Interest) <- c("date","interest_rate")
#Use Left Joins


# Creating Doge table
doge_join <-  left_join(doge_month,dta_CPI, by = "date")
doge_join <-  left_join(doge_join,dta_Nasdaq, by = "date")
doge_join <-  left_join(doge_join,dta_COVID, by = "date")
doge_join <-  left_join(doge_join,dta_Interest, by = "date")

#Creating bitcoin table
bitcoin_join <-  left_join(bitcoin_month,dta_CPI, by = "date")
bitcoin_join <-  left_join(bitcoin_join,dta_Nasdaq, by = "date")
bitcoin_join <-  left_join(bitcoin_join,dta_COVID, by = "date")
bitcoin_join <-  left_join(bitcoin_join,dta_Interest, by = "date")

#Creating Etherium table
eth_join <-  left_join(eth_month,dta_CPI, by = "date")
eth_join <-  left_join(eth_join,dta_Nasdaq, by = "date")
eth_join <-  left_join(eth_join,dta_COVID, by = "date")
eth_join <-  left_join(eth_join,dta_Interest, by = "date")

#Creating tether table
teth_join <-  left_join(teth_month,dta_CPI, by = "date")
teth_join <-  left_join(teth_join,dta_Nasdaq, by = "date")
teth_join <-  left_join(teth_join,dta_COVID, by = "date")
teth_join <-  left_join(teth_join,dta_Interest, by = "date")

#Creating alternate datasets for special use
bitcoin_join_2 <- bitcoin_join
doge_join_2 <- doge_join
eth_join_2 <- eth_join
teth_join_2 <- teth_join

# RENAMING COLUMNS TO JOIN COINS
colnames(bitcoin_join_2) <- c("date","avg_opening_price_bitcoin","bitcoin_Open","nasdaq_close","Low","ba_covid.","interest_rate")
colnames(doge_join_2) <- c("date","avg_opening_price_doge","cpi","nasdaq_close","ba_covid","interest_rate")
colnames(eth_join_2) <- c("date","avg_opening_price_eth","cpi","nasdaq_close","unknown","ba_covid","interest_rate_eth")
colnames(teth_join_2) <- c("date","avg_opening_price_teth","cpi","nasdaq_open","unknown","ba_covid","interest_rate_teth")

#JOINING THE COINS INTO ONE TABLE (ON DATE)
coin_join <- merge(x = bitcoin_join_2, y = doge_join_2, by = "date")
coin_join <- merge(x = coin_join, y = eth_join_2, by = "date")
coin_join <- merge(x = coin_join, y = teth_join_2, by = "date")


# Isolated Ind. Variables for use in vis
ind_var <-  full_join(dta_Nasdaq,dta_CPI, by = "date")
ind_var <-  full_join(ind_var,dta_Interest, by = "date")


#********************************************************************
#***************************Special Transform after Joins******************

#Had to find out data types for use in correlation plots. Used it for problem solving purposes
is.numeric(ind_var$cpi)
is.numeric(ind_var$nasdaq_close)
is.numeric(ind_var$interest_rate)

# Altered CPI from char to numeric
bitcoin_join$cpi <- as.numeric(as.character(bitcoin_join$cpi))
doge_join$cpi <- as.numeric(as.character(doge_join$cpi))
eth_join$cpi <- as.numeric(as.character(eth_join$cpi))
teth_join$cpi <- as.numeric(as.character(teth_join$cpi))
#**************************************************************************
#****************************************************************************

#Shanel

#******************************************************************************
#*****************************Exploratory Data Analysis 4A**************************
#************************************************************************************
#*
#*************UNDERSTANDING THE DATA WITH BASIC STATS *********************
# Summary of 
summary(dta_bitcoin)
summary(dta_doge)
summary(dta_eth)
summary(dta_teth)

summary(dta_CPI)
summary(dta_COVID)
summary(dta_Interest)
summary(dta_Nasdaq)

summary(dta_bitcoin[4])
summary(dta_doge[4])
summary(dta_eth[4])
summary(dta_teth[4])

#*******************************************************************************
#*#*****************************************************************************
#************VISUALIZATIONs*******************************************************
#*********************************************************************************



#All Cryptocurrencies plotted together 
plot_summary <- ggplot(coin_join, aes(x=date)) + 
  geom_line(aes(y = avg_opening_price_bitcoin), color = "red") + 
  geom_line(aes(y = avg_opening_price_doge), color="steelblue", linetype="solid") +
  geom_line(aes(y = avg_opening_price_eth), color = "darkgreen") + 
  geom_line(aes(y = avg_opening_price_teth), color="purple", linetype="longdash") +
  ggtitle("Cryptocurrency prices across time") +
  labs(x = "Time period", y = "Prices USD",
       title = "Cryptocurrency Prices",
       subtitle = "Fluctuations in currency prices between 2017 and 2021",
       caption = "Data: Kaggle", 
       tag = "Fig.1") +
  theme_economist() +
  theme(
    plot.title = element_text(color="Black", size=14, face="bold.italic",hjust = 0),
    axis.title.x = element_text(color="Black", size=11, face="bold" ,vjust = 0.5 ),
    axis.title.y = element_text(color="Black", size=11, face="bold",vjust = 0.5 )
  )  
#plot_summary
Animate_allcoins <- plot_summary + transition_reveal(date,range = NULL)
Animate_allcoins

#Plotting Coins separately

#Bitcoin Chart
plot_bitcoin <- ggplot(bitcoin_join, aes(x=date )) + 
  geom_line(aes(y= avg_opening_price), color = "red")+
  labs(x = "Time period", y = "Prices USD",
       title = "Bitcoin Prices",
       subtitle = "Fluctuations in Bitcoin prices between 2010 and 2021",
       caption = "Data: Kaggle", 
       tag = "Fig.2") +
  theme_economist() +
  theme(
    plot.title = element_text(color="Black", size=14, face="bold.italic",hjust = 0),
    axis.title.x = element_text(color="Black", size=11, face="bold" ,vjust = 0.5 ),
    axis.title.y = element_text(color="Black", size=11, face="bold",vjust = 0.5 )
  )  
#plot_bitcoin
Animate_bitcoin <- plot_bitcoin + transition_reveal(date,range = NULL)
Animate_bitcoin

#Doge Chart
plot_doge <- ggplot(doge_join, aes(x=date)) + 
  geom_line(aes(y= avg_opening_price), color = "steelblue")+
  labs(x = "Time period", y = "Prices USD",
       title = "Dogecoin Prices",
       subtitle = "Fluctuations in Dogecoin prices between 2017 and 2021",
       caption = "Data: Kaggle", 
       tag = "Fig.3") +
  theme_economist() +
  theme(
    plot.title = element_text(color="Black", size=14, face="bold.italic",hjust = 0),
    axis.title.x = element_text(color="Black", size=11, face="bold" ,vjust = 0.5 ),
    axis.title.y = element_text(color="Black", size=11, face="bold",vjust = 0.5 )
  ) 
#plot_doge
Animate_dogecoin <- plot_doge + transition_reveal(date,range = NULL)
Animate_dogecoin

#Ethereum Chart
plot_eth <- ggplot(eth_join, aes(x=date)) + 
  geom_line(aes(y= avg_opening_price), color = "darkgreen")+
  labs(x = "Time period", y = "Prices USD",
       title = "Ethereum Prices",
       subtitle = "Fluctuations in Dogecoin prices between 2016 and 2021",
       caption = "Data: Kaggle", 
       tag = "Fig.4") +
  theme_economist() +
  theme(
    plot.title = element_text(color="Black", size=14, face="bold.italic",hjust = 0),
    axis.title.x = element_text(color="Black", size=11, face="bold" ,vjust = 0.5 ),
    axis.title.y = element_text(color="Black", size=11, face="bold",vjust = 0.5 )
  ) 
#plot_eth
Animate_ethereum <- plot_eth + transition_reveal(date,range = NULL)
Animate_ethereum


#Tether Chart
plot_teth <- ggplot(teth_join, aes(x=date)) + 
  geom_line(aes(y= avg_opening_price), color = "purple")+
  labs(x = "Time period", y = "Prices USD",
       title = "Tether Prices",
       subtitle = "Fluctuations in Dogecoin prices between 2017 and 2021",
       caption = "Data: Kaggle", 
       tag = "Fig.5") +
  theme_economist() +
  theme(
    plot.title = element_text(color="Black", size=14, face="bold.italic",hjust = 0),
    axis.title.x = element_text(color="Black", size=11, face="bold" ,vjust = 0.5 ),
    axis.title.y = element_text(color="Black", size=11, face="bold",vjust = 0.5 )
  ) 
#plot_teth
Animate_tether <- plot_teth + transition_reveal(date,range = NULL)
Animate_tether

# Plotting relevant independent variables
Plot_indvar <- ggplot(ind_var, aes(x = date)) +
  geom_line (aes(y = interest_rate), color = "firebrick") +
  geom_line (aes(y = cpi) ,  color = "blue") +
  labs(x = "Time period", y = "Percentage (%)",
       title = "Inflation and Interest Ratess",
       subtitle = "Movement of marco economic facotors between 2010 and 2021",
       caption = "Data: Kaggle", 
       tag = "Fig.4") +
  theme_economist() +
  theme(
    plot.title = element_text(color="Black", size=14, face="bold.italic",hjust = 0),
    axis.title.x = element_text(color="Black", size=11, face="bold" ,vjust = 0.5 ),
    axis.title.y = element_text(color="Black", size=11, face="bold",vjust = 0.5 )
  ) 
Animate_independant <- Plot_indvar + transition_reveal(date,range = NULL)
Animate_independant
#Plot_indvar

#Had to find out data types for use in correlation plots. Used it for problem solving purposes
is.numeric(ind_var$cpi)
is.numeric(ind_var$nasdaq_close)
is.numeric(ind_var$interest_rate)

### Correlation of the Cryptocurrency prices (Dependent variables)


#Dropped all na values (1 row)
ind_var <- drop_na(ind_var)

#removing extra unnecesary columns
ind_var <- ind_var[ -c(1) ]
#Creating the Correlation Matrix
M <- cor(ind_var)
#corrplot(M, method = 'number', order = 'alphabet')
corrplot(M, method = 'color', order = 'alphabet')
#corrplot.mixed(M, method = 'color', order = 'alphabet')


### Correlation of the Cryptocurrency prices (Dependant variables)

coin_corr  <- select(coin_join,avg_opening_price_bitcoin,avg_opening_price_doge,avg_opening_price_eth,avg_opening_price_teth)
colnames(coin_corr) <- c("Bitcoin Price","Dogecoin Price","Ethereum Price","Tether Price")
N <- cor(coin_corr)
corrplot(N, method = 'color',order = 'alphabet')


#**********************
doge_reg <- lm(log(avg_opening_price)~cpi+nasdaq_close+ba_covid+interest_rate, data = doge_join)
summary(doge_reg)


bitcoin_reg <- lm(log(avg_opening_price)~cpi+nasdaq_close+ba_covid+interest_rate, data = bitcoin_join)
summary(bitcoin_reg)

eth_reg <- lm(log(avg_opening_price)~cpi+nasdaq_close+ba_covid+interest_rate, data = eth_join)
summary(eth_reg)


teth_reg <- lm(log(avg_opening_price)~cpi+nasdaq_close+ba_covid+interest_rate, data = teth_join)
summary(teth_reg)


stargazer(doge_reg,bitcoin_reg,eth_reg,teth_reg,type="text",align=TRUE,
          keep.stat=c("n","rsq","f"), no.space=FALSE,df=FALSE, 
          column.labels =c("doge_reg","bitcoin_reg","eth_reg","teth_reg"),
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)



