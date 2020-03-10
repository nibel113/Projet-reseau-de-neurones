
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
data("freMTPLfreq")
# Hyperparameter flags ---------------------------------------------------

FLAGS <- flags(
  flag_numeric("dropout1", 0.4),
  flag_string("optimizer","rmsprop"),
  flag_integer("hidden1",20),
  flag_integer("batch",10000),
  flag_string("act","relu"),
  flag_string("epochs",500)
)

# Data Preparation ---------------------------------------------------

dat <- freMTPLfreq %>% 
  as_tibble() %>% 
  mutate_at(vars(Power, Gas,Brand,Region), factor) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))



set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.8*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.8*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.8*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.8*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.8*length(which(dat$ClaimNb==4))), replace = FALSE))
learn <- dat[ll,]
testNN <- dat[-ll,]


##Défintion des indices pour l'échantillon de validation
ll2 <- sample(which(learn$ClaimNb==0), round(0.75*length(which(learn$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learn$ClaimNb==1), round(0.75*length(which(learn$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==2), round(0.75*length(which(learn$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==3), round(0.75*length(which(learn$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==4), round(0.75*length(which(learn$ClaimNb==4))), replace = FALSE))


learnNN <- learn[ll2,]
valNN <- learn[-ll2,]

rec_obj <- recipe(ClaimNb ~ ., # Throw out id column, but use all other variables as predictors
                  data = learnNN %>% select(-PolicyID)) %>% 
  step_center(CarAge, DriverAge, Density) %>% # Subtract column mean 
  step_scale(CarAge, DriverAge, Density) %>%  # Divide columns by standard deviation                              # Apply log transform
  step_dummy(Power, Gas,Brand,Region,one_hot = T,preserve = F) %>% 
  prep(training = learnNN)
  

# Use recipe to "bake" the final data 
learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) # Bake the recipe
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)

features <- c(2:4,6:36)
XlearnNN <- as.matrix(learn_prepped[,features])
YlearnNN <- as.matrix(learn_prepped[,5])
WlearnNN <- as.matrix(learn_prepped[,1])

XvalNN <- as.matrix(val_prepped[,features])
YvalNN <- as.matrix(val_prepped[,5])
WvalNN <- as.matrix(val_prepped[,1])

XtestNN <- as.matrix(test_prepped[,features])
YtestNN <- as.matrix(test_prepped[,5])
WtestNN <- as.matrix(test_prepped[,1])

# Define Model --------------------------------------------------------------

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = FLAGS$hidden1,activation=FLAGS$act,kernel_initializer = initializer_he_normal()) %>% 
  layer_dropout(FLAGS$dropout1) %>% 
  layer_dense(units = 1, activation = k_exp,kernel_initializer = initializer_he_normal())
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)

model %>% compile(loss = 'poisson', optimizer = FLAGS$optimizer)

# Training & Evaluation ----------------------------------------------------

history <- model %>% fit(list(XlearnNN, WlearnNN), 
                         YlearnNN,
                         validation_data=list(list(XvalNN,WvalNN),YvalNN),
                         epochs=FLAGS$epochs, 
                         batch_size=FLAGS$batch,
                         callbacks=list(callback_early_stopping(patience=10,restore_best_weights = T,min_delta = 0.00001)))

plot(history)

score <- model %>% evaluate(
  list(XtestNN,WtestNN), YtestNN,
  verbose = 0
)

Xlearn_all <- rbind(XlearnNN,XvalNN)
Ylearn_all <- rbind(YlearnNN,YvalNN)
Wlearn_all <- rbind(WlearnNN,WvalNN)

Poisson.Deviance <- function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}

learn_prepped$fit.keras <- model %>% predict(list(XlearnNN, WlearnNN))
test_prepped$fit.keras <- model %>% predict(list(XtestNN, WtestNN))
outloss <- Poisson.Deviance(test_prepped$fit.keras,test_prepped$ClaimNb)

inloss <- Poisson.Deviance(learn_prepped$fit.keras,learn_prepped$ClaimNb)
