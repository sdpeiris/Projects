#Clear Console
rm(list=ls())

set.seed(77)

# Set Working directories
setwd("C:/Users/Senal Peiris/OneDrive/Desktop/Bayesian project")

##Install required packages
#install.packages("readxl")
#install.packages("maptools")
#install.packages("spdep")
#install.packages("leaflet")
#install.packages("RColorBrewer")
#install.packages("rgeos")
#install.packages("spatialreg")
#install.packages("rgdal")
#install.packages("tidyverse")
#install.packages("spatialprobit")

#Load required packages
library(readxl)
library(tidyverse)
library(maptools)
library(spdep)
library(spatialreg)
library(spdep)
library(leaflet)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(spatialprobit)

# Load the Project data summary file
project_dataset <- read_excel("Project data summary.xlsx")

colnames(project_dataset) <- c("url","state","lat","long","latitude","longitude","square_miles","co2_emmisions","2010_population","2017_population","2020_population","gdp_2017","gdp_per_capita","agriculture_land","agriculture_land_per_capita","Energy_mwh_2017","energy_mwh_per_capita","no_of_firms","no_of_firms_per_capita","avg_miles_travelled", "population_2016","population_growth")

summary(project_dataset)

## Exploring the distribution of C)2 Emmisions across the dataset
CO2_Plot <- ggplot(project_dataset, aes(x=co2_emmisions)) + 
  geom_boxplot() + xlab("CO2 Emisions 2017 (Million Metric Tons)") +
  theme_grey() + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
CO2_Plot

# Trim and make a table with just the columns we need for our analysis
#project_dataset <-  original_dataset
#project_dataset$log_co2emission <- log(project_dataset$co2_emmisions)

#Analyzing pollution through CO2 emissions 
# Defining threshold for high/low emission levels
project_dataset <- mutate(project_dataset , Emmision_level = as.double(project_dataset$co2_emmisions > 103))  # Defining the dependent variable

#Creating logs of vaiables for analysis
project_dataset$log_gdp <- log(project_dataset$gdp_2017)
project_dataset$log_agriculture <- log(project_dataset$agriculture_land)


summary(project_dataset$co2_emmisions)
#summary(project_dataset$log_co2emission)


n <- length(project_dataset$Emmision_level)
x <- cbind(project_dataset$population_growth, project_dataset$no_of_firms_per_capita, project_dataset$log_gdp)
coords <- cbind(project_dataset$longitude,project_dataset$latitude)

#create weight matrix based on 10 nearest neighbors
k1 <- knn2nb(knearneigh(coords, k=10))
W <- nb2listw(k1, zero.policy = T)
Wt <- as(W, "CsparseMatrix")
trMatc <- trW(Wt, type="mult")
#Wx <- as.matrix(Wt)%*%x
data <- data.frame(population_growth  = x[,1],
                   no_of_firms_per_capita = x[,2],
                   log_gdp = x[,3],
                   Emmision_level = project_dataset$Emmision_level)

reg = Emmision_level ~  population_growth + no_of_firms_per_capita + log_gdp


# Estimate SAR Probit model
sarprobit.fit1 <- sarprobit(reg,data=data, Wt, ndraw = 150000, burn.in = 5000, thinning = 1, m = 10)

summary(sarprobit.fit1)
impacts(sarprobit.fit1)
plot(sarprobit.fit1)

# Compare against a generic Probit model

basic_probit <- glm(reg, family = binomial(link = "probit"), data=project_dataset)
summary(basic_probit)

library(mfx)
library(texreg)

# Partial Effect At the average (PEA)
basic_Probit_PEA<-probitmfx(reg, data=project_dataset, atmean = TRUE)
basic_Probit_PEA



