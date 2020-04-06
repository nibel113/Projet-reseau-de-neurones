# Hyperparameter flags ---------------------------------------------------
FLAGS <- flags(
  flag_numeric("dropout1", 0.25),
  flag_numeric("dropout2", 0.5),
  #flag_string("optimizer","rmsprop"),
  flag_integer("hidden1",64),
  flag_integer("hidden2",128)#,
  #flag_integer("batch",10000),
  #flag_string("act","relu"),
  #flag_string("epochs",500)#,
  #flag_numeric("l1",0.01),
  #flag_numeric("l2",0.01)
)



# Define Model --------------------------------------------------------------

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = FLAGS$hidden1,activation="relu",kernel_initializer = initializer_he_normal(seed=1L)) %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate=FLAGS$dropout1) %>% 
  layer_dense(units = FLAGS$hidden2,activation="relu",kernel_initializer = initializer_he_normal(seed=2L)) %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate=FLAGS$dropout2) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal(seed=3L))
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)

model %>% compile(loss = Poisson.Deviance, optimizer = "nadam")

# Training & Evaluation ----------------------------------------------------

history <- model %>% fit(list(XlearnNN, WlearnNN), 
                         YlearnNN,
                         validation_data=list(list(XvalNN,WvalNN),YvalNN),
                         epochs=1000, 
                         batch_size=8192,
                         callbacks=list(
                           callback_early_stopping(patience=25,restore_best_weights = T))
                         )

data_fit <- as.data.frame(history)


ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()

score <- model %>% evaluate(
  list(XtestNN,WtestNN), YtestNN,
  verbose = 0
)

