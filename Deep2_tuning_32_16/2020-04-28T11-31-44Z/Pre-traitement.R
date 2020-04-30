##Prétraitements des données
library(CASdatasets)
library(keras)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)
data("freMTPLfreq")

dat <- freMTPLfreq %>%
  as_tibble() %>%
  mutate_at(vars(Gas, Brand, Region), factor) %>%
  mutate_at(vars(Power),as.integer) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))%>% 
  mutate(DriverAge= if_else(DriverAge > 90,90,DriverAge)) %>% 
  mutate(CarAge = if_else(CarAge > 20,20,CarAge))

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
         data = learnNN) %>% step_rm(PolicyID) %>%
  step_log(Density) %>%
  step_range(CarAge, DriverAge, Density,Power) %>% # min max
  step_dummy(Gas,
             Brand,
             Region,
             one_hot = F,
             preserve = F) %>%
  prep(training = learnNN)


# Use recipe to "bake" the final data 
learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) # Bake the recipe
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)

features <- c(2:5,7:22)
XlearnNN <- as.matrix(learn_prepped[,features])
YlearnNN <- as.numeric(as.matrix(learn_prepped[,6]))
WlearnNN <- as.matrix(log(learn_prepped[,1]))

XvalNN <- as.matrix(val_prepped[,features])
YvalNN <- as.numeric(as.matrix(val_prepped[,6]))
WvalNN <- as.matrix(log(val_prepped[,1]))

XtestNN <- as.matrix(test_prepped[,features])
YtestNN <- as.numeric(as.matrix(test_prepped[,6]))
WtestNN <- as.matrix(log(test_prepped[,1]))


##Création d'une fonction de perte sur mesure, on doit utiliser les fonctions de keras, k_**
Poisson.Deviance <- function(y_true,y_pred){
  
  2*k_mean( y_pred - y_true + k_log( ( y_true + k_epsilon() ) / ( y_pred + k_epsilon() ) ) * y_true, axis = -1)
  
}
