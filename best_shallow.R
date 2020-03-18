##Runing best shallow



library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
data("freMTPLfreq")
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
  step_range(CarAge, DriverAge, Density) %>% # min max
  step_dummy(Power, Gas,Brand,Region,one_hot = T,preserve = F) %>% 
  prep(training = learnNN)


# Use recipe to "bake" the final data 
learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) # Bake the recipe
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)

features <- c(2:4,6:36)
XlearnNN <- as.matrix(learn_prepped[,features])
YlearnNN <- as.numeric(as.matrix(learn_prepped[,5]))
WlearnNN <- as.matrix(learn_prepped[,1])

XvalNN <- as.matrix(val_prepped[,features])
YvalNN <- as.numeric(as.matrix(val_prepped[,5]))
WvalNN <- as.matrix(val_prepped[,1])

XtestNN <- as.matrix(test_prepped[,features])
YtestNN <- as.numeric(as.matrix(test_prepped[,5]))
WtestNN <- as.matrix(test_prepped[,1])

##Référence tfruns et comment créer fonction de perte custom
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
# Hyperparameter flags ---------------------------------------------------




##Création d'une fonction de perte sur mesure, on doit utiliser les fonctions de keras, k_**
Poisson.Deviance <- function(y_true,y_pred){
  
  2*(k_mean(y_pred)-k_mean(y_true)+k_mean(k_log(((y_true+k_epsilon())/(y_pred+k_epsilon()))^y_true)))
  
}






# Define Model --------------------------------------------------------------


features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # define network for features
net <- features.0 %>%
  #on ajoute kernel initialize direct dans le dense layer
  layer_dense(units = 100,activation="relu",kernel_initializer = initializer_he_normal(),kernel_regularizer = regularizer_l1_l2(l1 = 0, l2 = 0.01)) %>% 
  layer_dropout(0.05) %>% 
  layer_dense(units = 1, activation = k_exp)
volumes.0 <- layer_input(shape=c(1))                     # define network for offset
offset <- volumes.0 %>%
  layer_dense(units = 1, activation = 'linear', use_bias=FALSE, trainable=FALSE, weights=list(array(1, dim=c(1,1))))
merged <- list(net, offset) %>%                          # combine the two networks
  layer_multiply() 
model <- keras_model(inputs=list(features.0, volumes.0), outputs=merged)

model %>% compile(loss = Poisson.Deviance, optimizer = "adam")

# Training & Evaluation ----------------------------------------------------

model %>% fit(list(XlearnNN, WlearnNN), 
                         YlearnNN,
                         validation_data=list(list(XvalNN,WvalNN),YvalNN),
                         epochs=500, 
                         batch_size=5000,
                         callbacks=list(callback_early_stopping(patience=25,restore_best_weights = T,min_delta = 0.00001)))

plot(history)

score <- model %>% evaluate(
  list(XtestNN,WtestNN), YtestNN,
  verbose = 0
)

yfit <- predict(model,list(XtestNN,WtestNN))
