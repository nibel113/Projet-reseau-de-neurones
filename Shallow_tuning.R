# Hyperparameter flags ---------------------------------------------------

FLAGS <- flags(
  flag_numeric("dropout1", 0.5),
  flag_string("optimizer","nadam"),
  flag_integer("hidden1",128),
  flag_integer("batch",8192),
  flag_string("act","relu"),
  flag_string("epochs",5000),
  flag_numeric("l1",0),
  flag_numeric("l2",0)
)


# Define Model --------------------------------------------------------------

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = FLAGS$hidden1,activation=FLAGS$act,kernel_initializer = initializer_he_normal(seed=1L),kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, l2 = FLAGS$l2)) %>%
  layer_batch_normalization() %>% 
  layer_dropout(FLAGS$dropout1) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal(seed=2L))
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)

model %>% compile(loss = Poisson.Deviance, optimizer = FLAGS$optimizer)

# Training & Evaluation ----------------------------------------------------

history <- model %>% fit(list(XlearnNN, WlearnNN), 
                         YlearnNN,
                         validation_data=list(list(XvalNN,WvalNN),YvalNN),
                         epochs=FLAGS$epochs, 
                         batch_size=FLAGS$batch,
                         callbacks=list(callback_early_stopping(patience=25,restore_best_weights = T,min_delta = 0.00001)))


data_fit <- as.data.frame(history)


ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits=c(0.25,0.27))

score <- model %>% evaluate(
  list(XtestNN,WtestNN), YtestNN,
  verbose = 0
)


