###GLM

library(CASdatasets)
library(keras)
library(ggplot2)

data("freMTPLfreq")

dat <- freMTPLfreq

Poisson.Deviance <- function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}

## 421 exposure plus grand que 1
length(which(dat$Exposure>1))

dat$Exposure <- pmin(dat$Exposure, 1)


set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.75*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.75*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.75*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.75*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.75*length(which(dat$ClaimNb==4))), replace = FALSE))
learn <- dat[ll,]
test <- dat[-ll,]
(n_l <- nrow(learn))
(n_t <- nrow(test))

prop.table(table(dat$ClaimNb))
prop.table(table(learn$ClaimNb))
prop.table(table(test$ClaimNb))


str(dat)

attach(dat)

ggplot(dat, aes(x = Power, fill = Gas)) +
  geom_bar(position="fill")


### GLM
## glm poisson de base pas de sélection de variable 


glm1 <- glm(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas + Region + I(log(Density))+ offset(log(Exposure)),data=learn,family="poisson")

learn$fit <- fitted(glm1)
test$fit <- predict(glm1,newdata=test,type="response")

in_loss_glm1 <- Poisson.Deviance(learn$fit,learn$ClaimNb)
out_loss_glm1 <- Poisson.Deviance(test$fit,test$ClaimNb)


### Benchmark shallow network

##Prétraitements des données

# MinMax scaler
PreProcess.Minimax <- function(var1, dat2){
  names(dat2)[names(dat2) == var1]  <- "V1"
  dat2$X <- as.numeric(dat2$V1)
  dat2$X <- 2*(dat2$X-min(dat2$X))/(max(dat2$X)-min(dat2$X))-1
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
# note: chercher s'il serait pas mieux de limiter âge
Features.PreProcess <- function(dat2){
  dat2 <- PreProcess.Minimax("CarAge", dat2)   
  dat2 <- PreProcess.Minimax("DriverAge", dat2)   
  dat2 <- PreProcess.CatDummy("Brand", "Br", dat2)
  dat2$GasX <- as.integer(dat2$Gas)-1.5
  dat2$Density <- round(log(dat2$Density),2)
  dat2 <- PreProcess.Minimax("Density", dat2)   
  dat2 <- PreProcess.CatDummy("Region", "R", dat2)
  dat2 <- PreProcess.CatDummy("Power","Pw",dat2)
  dat2
}

dat2 <- Features.PreProcess(dat)

set.seed(100)
ll <- sample(which(dat2$ClaimNb==0), round(0.75*length(which(dat2$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat2$ClaimNb==1), round(0.75*length(which(dat2$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat2$ClaimNb==2), round(0.75*length(which(dat2$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat2$ClaimNb==3), round(0.75*length(which(dat2$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat2$ClaimNb==4), round(0.75*length(which(dat2$ClaimNb==4))), replace = FALSE))
learnNN <- dat2[ll,]
testNN <- dat2[-ll,]
(n_l <- nrow(learn))
(n_t <- nrow(test))

##Défintion des indice pour le validation set
set.seed(100)
ll2 <- sample(which(learnNN$ClaimNb==0), round(0.9*length(which(learnNN$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==1), round(0.9*length(which(learnNN$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==2), round(0.9*length(which(learnNN$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==3), round(0.9*length(which(learnNN$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==4), round(0.9*length(which(learnNN$ClaimNb==4))), replace = FALSE))

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

q1 <- 20
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  layer_dense(units = 64, activation = 'tanh') %>% 
  layer_dense(units = 1, activation = k_exp)
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    
summary(model)    
model %>% compile(loss = 'poisson', optimizer = 'nadam')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=1000, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN))
  (proc.time()-t1)}
plot(fit)

learnNN$fit.keras <- as.vector(model %>% predict(list(learnNN, learnNN)))
testNN$fit.keras <- as.vector(model %>% predict(list(XtestNN, WtestNN)))
Poisson.Deviance(testNN$fit.keras,testNN$ClaimNb)
