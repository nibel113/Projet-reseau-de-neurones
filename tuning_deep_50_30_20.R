#tuning deep 50-30-20

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

summary(model)    


model %>% compile(loss = 'poisson', optimizer = 'rmsprop')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),
                       callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16125,.163750))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- "init"
out_loss_50_30_20 <- Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])
in_loss_50_30_20 <- Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])

##normalization layers

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_batch_normalization() %>% 
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_batch_normalization() %>% 
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- c(deep_50_30_20,"normalization")
(out_loss_50_30_20 <- c(out_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_50_30_20 <- c(in_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

##dropout 0.05

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dropout(rate=0.05) %>% 
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.05) %>% 
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.05) %>% 
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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- c(deep_50_30_20,"drop_0.05")
(out_loss_50_30_20 <- c(out_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_50_30_20 <- c(in_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

##drop 0.1

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dropout(rate=0.1) %>% 
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.1) %>% 
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.1) %>% 
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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.18))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- c(deep_50_30_20,"drop_0.1")
(out_loss_50_30_20 <- c(out_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_50_30_20 <- c(in_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#drop 0.2

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dropout(rate=0.2) %>% 
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.2) %>% 
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.2) %>% 
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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- c(deep_50_30_20,"drop_0.2")
(out_loss_50_30_20 <- c(out_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_50_30_20 <- c(in_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#drop 0.15

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.15) %>% 
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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- c(deep_50_30_20,"drop_0.15")
(out_loss_50_30_20 <- c(out_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_50_30_20 <- c(in_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

## drop 0.15 + normalization

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_batch_normalization() %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.15) %>% 
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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- c(deep_50_30_20,"drop_0.15+normalization")
(out_loss_50_30_20 <- c(out_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_50_30_20 <- c(in_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#drop 0.15+ normalization + adam

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_batch_normalization() %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    

summary(model)    


model %>% compile(loss = 'poisson', optimizer = 'adam')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- c(deep_50_30_20,"drop_0.15+normalization+adam")
(out_loss_50_30_20 <- c(out_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_50_30_20 <- c(in_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

#drop 0.15+ normalization + adam +batch 10000

k_clear_session()
features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 50, activation = 'relu',kernel_initializer = initializer_he_normal()) %>%
  layer_batch_normalization() %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 30, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 20, activation = 'relu',kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(rate=0.15) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)    

summary(model)    


model %>% compile(loss = 'poisson', optimizer = 'adam')

{t1 <- proc.time()
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}
plot(fit)
data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))


learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
deep_50_30_20 <- c(deep_50_30_20,"drop_0.15+normalization+adam+batch10000")
(out_loss_50_30_20 <- c(out_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss_50_30_20 <- c(in_loss_50_30_20,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

