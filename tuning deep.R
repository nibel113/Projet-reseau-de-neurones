###Test quel algorithme est le meilleur pour commencer deep_network


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
ll <- sample(which(dat2$ClaimNb==0), round(0.8*length(which(dat2$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat2$ClaimNb==1), round(0.8*length(which(dat2$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat2$ClaimNb==2), round(0.8*length(which(dat2$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat2$ClaimNb==3), round(0.8*length(which(dat2$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat2$ClaimNb==4), round(0.8*length(which(dat2$ClaimNb==4))), replace = FALSE))
learnNN <- dat2[ll,]
testNN <- dat2[-ll,]
(n_l <- nrow(learnNN))
(n_t <- nrow(testNN))

##Défintion des indice pour le validation set
set.seed(200)
ll2 <- sample(which(learnNN$ClaimNb==0), round(0.8*length(which(learnNN$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==1), round(0.8*length(which(learnNN$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==2), round(0.8*length(which(learnNN$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==3), round(0.8*length(which(learnNN$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learnNN$ClaimNb==4), round(0.8*length(which(learnNN$ClaimNb==4))), replace = FALSE))

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



q1 <- 20
q2 <- 20
optimizers = c('sgd', 'adagrad', 'adadelta', 'rmsprop', 'adam', 'adamax', 'nadam')

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = q1, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dense(units = q2,activation='relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model2 <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    
summary(model2)    

#permet de saver les weights initials 
save_model_weights_hdf5(model2,"model2.hdf5")

model2 %>% compile(loss = 'poisson', optimizer = optimizers[1])




{t1 <- proc.time()
  fit <- model %>% fit(list(Xlearn_all, Wlearn_all), Ylearn_all, epochs=100, batch_size=10000)
  (proc.time()-t1)}
runtime <- c(c(45,4.87,20.77))

learnNN$fit.keras <- as.vector(model %>% predict(list(Xlearn_all, Wlearn_all)))
testNN$fit.keras <- as.vector(model %>% predict(list(XtestNN, WtestNN)))
outloss1 <- Poisson.Deviance(testNN$fit.keras,testNN$ClaimNb)
inloss1 <- Poisson.Deviance(learnNN$fit.keras,learnNN$ClaimNb)

#on reload les weights initiaux
load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[2])

{t1 <- proc.time()
  fit <- model %>% fit(list(Xlearn_all, Wlearn_all), Ylearn_all, epochs=100, batch_size=10000)
  (proc.time()-t1)}
runtime <- c(runtime,c(48.88,3.92,25.09))

learnNN$fit.keras <- as.vector(model %>% predict(list(Xlearn_all, Wlearn_all)))
testNN$fit.keras <- as.vector(model %>% predict(list(XtestNN, WtestNN)))
outloss2 <- Poisson.Deviance(testNN$fit.keras,testNN$ClaimNb)
inloss2 <- Poisson.Deviance(learnNN$fit.keras,learnNN$ClaimNb)

load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[3])

{t1 <- proc.time()
  fit <- model %>% fit(list(Xlearn_all, Wlearn_all), Ylearn_all, epochs=100, batch_size=10000)
  (proc.time()-t1)}
runtime <- c(runtime,c(47.34,4.16,21.22))

learnNN$fit.keras <- as.vector(model %>% predict(list(Xlearn_all, Wlearn_all)))
testNN$fit.keras <- as.vector(model %>% predict(list(XtestNN, WtestNN)))
outloss3 <- Poisson.Deviance(testNN$fit.keras,testNN$ClaimNb)
inloss3 <- Poisson.Deviance(learnNN$fit.keras,learnNN$ClaimNb)

load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[4])

{t1 <- proc.time()
  fit <- model %>% fit(list(Xlearn_all, Wlearn_all), Ylearn_all, epochs=100, batch_size=10000)
  (proc.time()-t1)}
runtime <- c(runtime,c(46.25,3.72,21.54))

learnNN$fit.keras <- as.vector(model %>% predict(list(Xlearn_all, Wlearn_all)))
testNN$fit.keras <- as.vector(model %>% predict(list(XtestNN, WtestNN)))
outloss4 <- Poisson.Deviance(testNN$fit.keras,testNN$ClaimNb)
inloss4 <- Poisson.Deviance(learnNN$fit.keras,learnNN$ClaimNb)

load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[5])

{t1 <- proc.time()
  fit <- model %>% fit(list(Xlearn_all, Wlearn_all), Ylearn_all, epochs=100, batch_size=10000)
  (proc.time()-t1)}
runtime <- c(runtime,c(47.2,4.26,21.47))

learnNN$fit.keras <- as.vector(model %>% predict(list(Xlearn_all, Wlearn_all)))
testNN$fit.keras <- as.vector(model %>% predict(list(XtestNN, WtestNN)))
outloss5 <- Poisson.Deviance(testNN$fit.keras,testNN$ClaimNb)
inloss5 <- Poisson.Deviance(learnNN$fit.keras,learnNN$ClaimNb)

load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[6])

{t1 <- proc.time()
  fit <- model %>% fit(list(Xlearn_all, Wlearn_all), Ylearn_all, epochs=100, batch_size=10000)
  (proc.time()-t1)}
runtime <- c(runtime,c(45.83,4.24,21.41))

learnNN$fit.keras <- as.vector(model %>% predict(list(Xlearn_all, Wlearn_all)))
testNN$fit.keras <- as.vector(model %>% predict(list(XtestNN, WtestNN)))
outloss6 <- Poisson.Deviance(testNN$fit.keras,testNN$ClaimNb)
inloss6 <- Poisson.Deviance(learnNN$fit.keras,learnNN$ClaimNb)

load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[7])

{t1 <- proc.time()
  fit <- model %>% fit(list(Xlearn_all, Wlearn_all), Ylearn_all, epochs=100, batch_size=10000)
  (proc.time()-t1)}
runtime <- c(runtime,c(48.3,4.14,22.5))

learnNN$fit.keras <- as.vector(model %>% predict(list(Xlearn_all, Wlearn_all)))
testNN$fit.keras <- as.vector(model %>% predict(list(XtestNN, WtestNN)))
outloss7 <- Poisson.Deviance(testNN$fit.keras,testNN$ClaimNb)
inloss7 <- Poisson.Deviance(learnNN$fit.keras,learnNN$ClaimNb)