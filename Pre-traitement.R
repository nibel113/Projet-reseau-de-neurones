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
  mutate_at(vars(Power, Gas, Brand, Region), factor) %>%
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))



set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.8*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.8*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.8*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.8*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.8*length(which(dat$ClaimNb==4))), replace = FALSE))

## on remet l'ordre aléatoire
ll <- sample(ll,size=length(ll))

## création test et entrainement
learn <- dat[ll,]
testNN <- dat[-ll,]


##Défintion des indices pour l'échantillon de validation
ll2 <- sample(which(learn$ClaimNb==0), round(0.75*length(which(learn$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learn$ClaimNb==1), round(0.75*length(which(learn$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==2), round(0.75*length(which(learn$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==3), round(0.75*length(which(learn$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==4), round(0.75*length(which(learn$ClaimNb==4))), replace = FALSE))

## on remet l'indiçage aléatoire
ll2 <- sample(ll2,size=length(ll2))
learnNN <- learn[ll2,]
valNN <- learn[-ll2,]

rec_obj <-
  recipe(ClaimNb ~ ., # Throw out id column, but use all other variables as predictors
         data = learnNN %>% select(-PolicyID)) %>%
  step_log(Density) %>%
  step_range(CarAge, DriverAge, Density) %>% # min max
  step_dummy(Power,
             Gas,
             Brand,
             Region,
             one_hot = T,
             preserve = F) %>%
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


##Création d'une fonction de perte sur mesure, on doit utiliser les fonctions de keras, k_**
Poisson.Deviance <- function(y_true,y_pred){
  
  2*(k_mean(y_pred) - k_mean(y_true) +k_mean(k_log(((y_true + k_epsilon()) / (y_pred + k_epsilon())) ^ y_true)))
  
}



