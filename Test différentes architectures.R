##Benchmark shallow network et deep network
library(keras)
source("Pre-traitement.R")

#shallow 
q1 <- c(10,20,30,40,50,60)
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = q1[1], activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    

summary(model)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

which.min(data_fit$value[401:800])
ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))

Poisson.Deviance <- function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- "SN_10"
out_loss <- Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])
in_loss <- Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])

##20 hidden layers

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = q1[2], activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"SN_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#shallow 30

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = q1[3], activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"SN_30")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#shallow 40

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = q1[4], activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)
ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"SN_40")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#shallow 50
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = q1[5], activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"SN_50")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#shallow 60

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = q1[6], activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"SN_60")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

##deep 2 layers 
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_30")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

##deep 30 10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

## deep 30 20

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

##deep 30 40

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 40, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_40")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

## deep 30-50

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_50")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 30-60
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 60, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_60")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50_10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-20
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-30
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_30")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-40

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 40, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_40")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-50

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_50")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-60

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 60, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_60")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 30-50-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_50_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 30-50-20

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))



learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_50_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 30-30-30

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_30_30")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-30-10 

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_30_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 50-50-30

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_50_30")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-10-30-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_10_30_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 30-50-10-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_50_10_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 100-50-30-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 100, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_100_50_30_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-10-10-10
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_10_10_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-20-20-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_20_20_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#deep 50-50-50-50-50
k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_50_50_50_50")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 50-30-20

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_30_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 50-30-20-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_30_20_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 50-40-20

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 40, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_40_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 40-30-20

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 40, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_40_30_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 30-40-50-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 40, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_40_50_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 20-20-20-20

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_20_20_20_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 30-20-20-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_20_20_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 10-10-10-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_10_10_10_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 30-10-10-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_10_10_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 50-10-10-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_10_10_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 30-20-10-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_20_10_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

##deep 30-10-20-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_30_10_20_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 20-20

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_20_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 10-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_10_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 20-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_20_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 10-20

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_10_20")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 10-30

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_10_30")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 20-30

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_20_30")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)


#deep 10-50

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_10_50")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

#deep 10-40

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 40, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_10_40")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)

##deep 10-30-10

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 10, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_10_30_10")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)
