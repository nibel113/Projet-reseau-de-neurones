##Référence tfruns et comment créer fonction de perte custom
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
# Hyperparameter flags ---------------------------------------------------

FLAGS <- flags(
  flag_numeric("dropout1", 0.4),
  flag_string("optimizer","rmsprop","adam"),
  flag_integer("hidden1",20),
  flag_integer("batch",10000),
  flag_string("act","relu"),
  flag_string("epochs",500),
  flag_numeric("l1",0.01),
  flag_numeric("l2",0.01)
)


# Define Model --------------------------------------------------------------

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = FLAGS$hidden1,activation=FLAGS$act,kernel_initializer = initializer_he_normal(),kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, l2 = FLAGS$l2)) %>%
  layer_batch_normalization() %>% 
  layer_dropout(FLAGS$dropout1) %>% 
  layer_dense(units = 1, activation = k_exp)
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

##meilleure graphique à faire
data_fit <- as.data.frame(history)


ggplot(data_fit,aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits = c(0.16,.170))#+

score <- model %>% evaluate(
  list(XtestNN,WtestNN), YtestNN,
  verbose = 0
)


