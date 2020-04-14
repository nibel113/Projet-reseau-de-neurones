# Hyperparameter flags ---------------------------------------------------
FLAGS <- flags(
  flag_numeric("dropout1", 0.25),
  flag_numeric("dropout2", 0),
  flag_integer("hidden1",16),
  flag_integer("hidden2",16),
  flag_numeric("l1_1",0),
  flag_numeric("l2_1",0),
  flag_numeric("l1_2",0),
  flag_numeric("l2_2",0)
)

lambda.hom <- sum(learnNN$ClaimNb)/sum(learnNN$Exposure)

# Define Model --------------------------------------------------------------

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = FLAGS$hidden1,activation="relu",kernel_initializer = initializer_he_normal(seed=1L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1_1, l2 = FLAGS$l2_1)) %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate=FLAGS$dropout1) %>% 
  layer_dense(units = FLAGS$hidden2,activation="relu",kernel_initializer = initializer_he_normal(seed=2L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1_2, l2 = FLAGS$l2_2)) %>% 
  layer_batch_normalization() %>%
  layer_dropout(rate=FLAGS$dropout2) %>% 
  layer_dense(units=1, activation='linear', 
              weights=list(array(0, dim=c(FLAGS$hidden2,1)), array(log(lambda.hom), dim=c(1))))

volumes.0 <- layer_input(shape=c(1))                     # define network for offset

merged <- list(net, volumes.0) %>%                          # combine the two networks
  layer_add(name='Add') %>% 
  layer_dense(units=1, activation=k_exp, trainable=FALSE,
              weights=list(array(1, dim=c(1,1)), array(0, dim=c(1))))

model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)

model %>% compile(loss = Poisson.Deviance, optimizer = "nadam")

# Training & Evaluation ----------------------------------------------------

history <- model %>% fit(list(XlearnNN, WlearnNN), 
                         YlearnNN,
                         validation_data=list(list(XvalNN,WvalNN),YvalNN),
                         epochs=1000, 
                         batch_size=8192,
                         callbacks=list(
                           callback_early_stopping(patience=10,restore_best_weights = T),
                           callback_reduce_lr_on_plateau(factor = 0.05))
                         )

data_fit <- as.data.frame(history)


ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()

score <- model %>% evaluate(
  list(XtestNN,WtestNN), YtestNN,
  verbose = 0
)


