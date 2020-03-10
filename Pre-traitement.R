##Prétraitements des données
library(CASdatasets)

data("freMTPLfreq")

dat <- freMTPLfreq


## 421 observations avec Exposure plus grand que 1
length(which(dat$Exposure>1))

dat$Exposure <- pmin(dat$Exposure, 1)
dat$Exposure <- pmin(dat$Exposure, 1)

##Définition des indices pour les échantillons d'apprentissage et de test
set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.8*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.8*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.8*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.8*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.8*length(which(dat$ClaimNb==4))), replace = FALSE))
learnNN <- dat[ll,]
testNN <- dat[-ll,]


##Défintion des indices pour l'échantillon de validation
ll2 <- sample(which(learnNN$ClaimNb==0), round(0.8*length(which(learnNN$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==1), round(0.8*length(which(learnNN$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==2), round(0.8*length(which(learnNN$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==3), round(0.8*length(which(learnNN$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==4), round(0.8*length(which(learnNN$ClaimNb==4))), replace = FALSE))

# MinMax scaler
PreProcess.Minimax <- function(var1, dat2){
  names(dat2)[names(dat2) == var1]  <- "V1"
  dat2$X <- as.numeric(dat2$V1)
  mean <- mean(learnNN[,var1])
  sd <- sd(learnNN[,var1])
  dat2$X <- (dat2$X-mean)/sd
  names(dat2)[names(dat2) == "V1"]  <- var1
  names(dat2)[names(dat2) == "X"]  <- paste(var1,"X", sep="")
  dat2
}

# Dummy coding 
PreProcess.CatDummy <- function(var1, short, dat2){
  names(dat2)[names(dat2) == var1]  <- "V1"
  n2 <- ncol(dat2)
  dat2$X <- as.integer(dat2$V1)
  n0 <- length(unique(dat2$X))
  for (n1 in 2:n0){dat2[, paste(short, n1, sep="")] <- as.integer(dat2$X==n1)}
  names(dat2)[names(dat2) == "V1"]  <- var1
  dat2[, c(1:n2,(n2+2):ncol(dat2))]
}

# Feature pre-processing using MinMax Scaler and Dummy Coding
Features.PreProcess <- function(dat2){
  dat2 <- PreProcess.Minimax("CarAge", dat2)   
  dat2 <- PreProcess.Minimax("DriverAge", dat2)   
  dat2 <- PreProcess.CatDummy("Brand", "Br", dat2)
  dat2$GasX <- as.integer(dat2$Gas)-1.5
  dat2$Density <- round(log(dat2$Density),3)
  dat2 <- PreProcess.Minimax("Density", dat2)   
  dat2 <- PreProcess.CatDummy("Region", "R", dat2)
  dat2 <- PreProcess.CatDummy("Power","Pw",dat2)
  dat2
}



learnNN <- Features.PreProcess(learnNN)
testNN <- Features.PreProcess(testNN)

features <- c(11:40)
XlearnNN <- as.matrix(learnNN[ll2,features])
YlearnNN <- as.matrix(learnNN[ll2,2])
WlearnNN <- as.matrix(learnNN[ll2,3])

XvalNN <- as.matrix(learnNN[-ll2,features])
YvalNN <- as.matrix(learnNN[-ll2,2])
WvalNN <- as.matrix(learnNN[-ll2,3])

XtestNN <- as.matrix(testNN[,features])
YtestNN <- as.matrix(testNN[,2])
WtestNN <- as.matrix(testNN[,3])

Xlearn_all <- as.matrix(learnNN[,features])
Ylearn_all <- as.matrix(learnNN$ClaimNb)
Wlearn_all <- as.matrix(learnNN[,3])







