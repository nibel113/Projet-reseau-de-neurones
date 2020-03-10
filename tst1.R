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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=5000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2]))
Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2]))

x <- data.frame(names,in_loss,out_loss)

#deep 50-30-20 batch 6611
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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=6611,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=20,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_30_20_batch6611")
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
  fit <- model %>% fit(list(XlearnNN, WlearnNN), YlearnNN, epochs=500, batch_size=10000,validation_data=list(list(XvalNN,WvalNN),YvalNN),callbacks=list(callback_early_stopping(patience=50,restore_best_weights = T,min_delta = 0.00001)))
  (proc.time()-t1)}

data_fit <- as.data.frame(fit)

ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.165))#+
#scale_x_continuous(limits=c(250,375))




learnNN$fit.keras[ll2] <- as.vector(model %>% predict(list(XlearnNN, WlearnNN)))
learnNN$fit.keras[-ll2] <- as.vector(model %>% predict(list(XvalNN, WvalNN)))
names <- c(names,"DN_50_30_20_batch10000")
(out_loss <- c(out_loss,Poisson.Deviance(learnNN$fit.keras[-ll2],learnNN$ClaimNb[-ll2])))
(in_loss <- c(in_loss,Poisson.Deviance(learnNN$fit.keras[ll2],learnNN$ClaimNb[ll2])))

x <- data.frame(names,in_loss,out_loss)
