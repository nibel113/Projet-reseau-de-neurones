##Prétraitements des données
library(CASdatasets)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)
data("freMTPLfreq")

dat <- freMTPLfreq %>%
  as_tibble() %>%
  mutate_at(vars(Gas, Brand, Region), factor) %>%
  mutate_at(vars(Power),as.integer) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))

dat$ClaimNb <- pmin(dat$ClaimNb, 4) 


set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.8*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.8*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.8*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.8*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.8*length(which(dat$ClaimNb==4))), replace = FALSE))

## on remet l'ordre aléatoire
ll <- sample(ll,size=length(ll),replace = F)

## création test et entrainement
learn <- dat[ll,]
testNN <- dat[-ll,]


##Défintion des indices pour l'échantillon de validation
set.seed(200)
ll2 <- sample(which(learn$ClaimNb==0), round(0.75*length(which(learn$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learn$ClaimNb==1), round(0.75*length(which(learn$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==2), round(0.75*length(which(learn$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==3), round(0.75*length(which(learn$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==4), round(0.75*length(which(learn$ClaimNb==4))), replace = FALSE))

## on remet l'indiçage aléatoire
ll2 <- sample(ll2,size=length(ll2),replace = F)
learnNN <- learn[ll2,]
valNN <- learn[-ll2,]

rec_obj <-
  recipe(ClaimNb ~ ., # Throw out id column, but use all other variables as predictors
         data = learnNN %>% select(-PolicyID)) %>%
  step_log(Density) %>%
  step_range(CarAge, DriverAge, Density,Power) %>% # min max
  step_dummy(Gas,
             one_hot = F,
             preserve = F) %>% 
  step_integer(Region,Brand,zero_based = T) %>% 
  prep(training = learnNN)

learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) # Bake the recipe
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)
# definition of feature variables (non-categorical)

# learning data
Xlearn <- as.matrix(learn_prepped[, c(2,3,4,7,9)])  # design matrix learning sample
Brlearn <- as.matrix(learn_prepped$Brand)
Relearn <- as.matrix(learn_prepped$Region)
Ylearn <- as.matrix(learn_prepped$ClaimNb)
# testing data
Xtest <- as.matrix(test_prepped[, c(2,3,4,7,9)])  # design matrix learning sample
Brtest <- as.matrix(test_prepped$Brand)
Retest <- as.matrix(test_prepped$Region)
Ytest <- as.matrix(test_prepped$ClaimNb)
# validation data
Xval <- as.matrix(val_prepped[, c(2,3,4,7,9)])  # design matrix learning sample
Brval <- as.matrix(val_prepped$Brand)
Reval <- as.matrix(val_prepped$Region)
Yval <- as.matrix(val_prepped$ClaimNb)

# choosing the right volumes for EmbNN and CANN
Vlearn <- as.matrix(log(learn_prepped$Offset))
Vtest <- as.matrix(log(test_prepped$Offset))
Vval <- as.matrix(log(val_prepped$Offset))


lambda.hom <- sum(learn_prepped$ClaimNb)/sum(learn_prepped$Offset)

Poisson.Deviance <- function(y_true,y_pred){
  
  2*(k_mean(y_pred) - k_mean(y_true) +k_mean(k_log(((y_true + k_epsilon()) / (y_pred + k_epsilon())) ^ y_true)))
  
}