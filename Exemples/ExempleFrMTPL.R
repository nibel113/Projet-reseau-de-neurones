###Insights from Inside Neural Networks 
###Ferrario, Andrea and Noll, Alexander and Wuthrich, Mario V.,
###(November 14, 2018).
###http://papers.ssrn.com/abstract=3226852

###tutorial about fitting NN to claims frequency data in insurance

library(CASdatasets)
library(tidyverse)
##French motor 3rd-party liability insurance portfolio, claim counts observed in 
## 1 year
data(freMTPL2freq)
data <- freMTPL2freq

##data cleaning as done in
##Noll, Alexander and Salzmann, Robert and Wuthrich, Mario V., 
##Case Study: French Motor Third-Party Liability Claims (November 8, 2018). 
##Available at SSRN: https://ssrn.com/abstract=3164764 or http://dx.doi.org/10.2139/ssrn.3164764

##exposure bigger than one year corrected as 1

##number of claims bigger than 4 corrected as 4
data$Exposure <- ifelse(data$Exposure>1,1,data$Exposure)
data$ClaimNb <- ifelse(data$ClaimNb>4,4,data$ClaimNb)


#### Shallow network

#data pre-processing

###ordered factors for Area
data$Area <- as.numeric(data$Area)

##transform VehGas to +/- .5

data$VehGas <- ifelse(data$VehGas=="Regular",0.5,-0.5)

###dummy coding for VehBrand and Region
data <- data.frame(model.matrix(~.-ClaimNb,data)[,-1])

##Capping and transformation
data$VehAge <- ifelse(data$VehAge>20,20,data$VehAge)

data$DrivAge <- ifelse(data$DrivAge>90,90,data$DrivAge)

data$BonusMalus <- ifelse(data$BonusMalus>150,150,data$BonusMalus)

data$Density <- log(data$Density)

###Scaling

##minmaxscale for Area, VehPower, VehAge
## DrivAge, BonusMalus, Density

minmax <- function(x){
  m <- min(x)
  M <- max(x)
  y <- 2*(x-m)/(M-m) -1
}

data$Area <- minmax(data$Area)
data$VehPower <- minmax(data$VehPower)
data$VehAge <- minmax(data$VehAge)
data$DrivAge <- minmax(data$DrivAge)
data$BonusMalus <- minmax(data$BonusMalus)
data$Density <- minmax(data$Density)

data <- data[,-1]

##
