# Hyperparameter flags ---------------------------------------------------

FLAGS <- flags(
  flag_numeric("dropout1", 0.5),
  flag_integer("hidden1",32),
  flag_integer("hidden2",32),
  flag_numeric("l1",0),
  flag_numeric("l2",0)
)
lambda.hom <- sum(learn_prepped$ClaimNb)/sum(learn_prepped$Offset)

# Define Model --------------------------------------------------------------

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = FLAGS$hidden1,activation="relu",kernel_initializer = initializer_he_normal(seed=1L),kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, l2 = FLAGS$l2)) %>%
  layer_batch_normalization() %>%  
  layer_dropout(FLAGS$dropout1) %>%
  layer_dense(units = FLAGS$hidden2,activation="relu",kernel_initializer = initializer_he_normal(seed=3L),kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, l2 = FLAGS$l2)) %>%
  layer_dense(units=1, activation='linear', 
              weights=list(array(0, dim=c(FLAGS$hidden2,1)), array(log(lambda.hom), dim=c(1))))
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = k_log, use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_add() %>% 
  layer_dense(units=1, activation=k_exp, name = 'Response', trainable=FALSE,
              weights=list(array(1, dim=c(1,1)), array(0, dim=c(1))))

model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)

model %>% compile(loss = Poisson.Deviance, optimizer = "nadam")

# Training & Evaluation ----------------------------------------------------

history <- model %>% fit(list(XlearnNN, WlearnNN), 
                         YlearnNN,
                         validation_data=list(list(XvalNN,WvalNN),YvalNN),
                         epochs=1000, 
                         batch_size=8192,
                         callbacks=list(callback_early_stopping(patience=10),
                                        callback_reduce_lr_on_plateau(factor = 0.05)))


data_fit <- as.data.frame(history)

ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()

ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits=c(0.245,0.27))



score <- model %>% evaluate(
  list(XtestNN,WtestNN), YtestNN,
  verbose = 0
)

name <- c(name,"256_drop.5_l2_0.001avec_norm")

out_loss <- c(out_loss,unname(score[1]))

results_shallow <- data.frame(name,out_loss)
results_shallow