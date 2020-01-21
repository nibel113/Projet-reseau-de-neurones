library(haven)
library(tidyverse)
library(neuralnet)
library(caret)

indata <- read_sas("C:/Users/Charlene/Desktop/SASUniversityEdition/myfolders/sasuser.v94/indata.sas7bdat", 
                   NULL)
colnames(indata) <- c("Age","Gender","GeoZone","EvRatio","VehiculeAge","BonusClass","Duration","NumberClaims","ClaimCost")

indata <- indata[which(indata$Duration!=0),] 

##minmax scaling

indata <- indata %>% 
   mutate(scaleAge=(Age-min(Age))/(max(Age)-min(Age)),
          scaleVehAge= (VehiculeAge - min(VehiculeAge))/(max(VehiculeAge)-min(VehiculeAge)),
          factGender= ifelse(Gender=="K",1,0),
          output= NumberClaims/Duration)
  
##neural network coding for one hidden layer and 2 neurons

##poisson deviance
loss <- function(x,y) 2*(y*log(y/exp(x))-y+exp(x))


nn2 <- neuralnet(output~scaleAge+scaleVehAge+factGender,data=indata,hidden=2,linear.output = T,threshold = 0.1,err.fct = loss)


##essai avec caret
pois_deviance <- function(data,lev=NULL,model=NULL)  
myControl <- trainControl(method = "none",summaryFunction = "mnLogLoss")
myGrid <- expand.grid(layer1=c(2),layer2=0,layer3=0)

model <- train(output~scaleAge+scaleVehAge+factGender,indata,method="neuralnet",
               trControl = myControl, tuneGrid = myGrid)
