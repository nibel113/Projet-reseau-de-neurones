###Test quel algorithme est le meilleur pour commencer


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



q1 <- 50
optimizers = c('sgd', 'adagrad', 'adadelta', 'rmsprop', 'adam', 'adamax', 'nadam')

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = q1, activation = 'relu',kernel_initializer = initializer_he_uniform()) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_uniform())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    
summary(model)    

#permet de saver les weights initials 
save_model_weights_hdf5(model,"model.hdf5")

model %>% compile(loss = 'poisson', optimizer = optimizers[1])

runtime <- matrix(NA,ncol=3)


{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=100, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN))
  runtime <- rbind(runtime, proc.time()-t1)}
plot(fit)

learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names_optimizer <- optimizers[1]
out_loss_optim <- Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])
in_loss_optim <- Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])

#on reload les weights initiaux
load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[2])

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=100, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN))
  runtime <- rbind(runtime, proc.time()-t1)}
plot(fit)

learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names_optimizer <- c(names_optimizer,optimizers[2])
(out_loss_optim <- c(out_loss_optim,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_optim <- c(in_loss_optim,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[3])

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=100, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN))
  runtime <- rbind(runtime, proc.time()-t1)}
plot(fit)

learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names_optimizer <- c(names_optimizer,optimizers[3])
(out_loss_optim <- c(out_loss_optim,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_optim <- c(in_loss_optim,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[4])

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=100, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN))
  runtime <- rbind(runtime, proc.time()-t1)}
plot(fit)

learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names_optimizer <- c(names_optimizer,optimizers[4])
(out_loss_optim <- c(out_loss_optim,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_optim <- c(in_loss_optim,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[5])

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=100, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN))
  runtime <- rbind(runtime, proc.time()-t1)}
plot(fit)

learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names_optimizer <- c(names_optimizer,optimizers[5])
(out_loss_optim <- c(out_loss_optim,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_optim <- c(in_loss_optim,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))
load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[6])

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=100, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN))
  runtime <- rbind(runtime, proc.time()-t1)}
plot(fit)

learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names_optimizer <- c(names_optimizer,optimizers[6])
(out_loss_optim <- c(out_loss_optim,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_optim <- c(in_loss_optim,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))
load_model_weights_hdf5(model,"model.hdf5")


model %>% compile(loss = 'poisson', optimizer = optimizers[7])

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=100, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN))
  runtime <- rbind(runtime, proc.time()-t1)}
plot(fit)

learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names_optimizer <- c(names_optimizer,optimizers[7])
(out_loss_optim <- c(out_loss_optim,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_optim <- c(in_loss_optim,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

y <- data.frame(names_optimizer,runtime[-1,3],in_loss_optim,out_loss_optim)
