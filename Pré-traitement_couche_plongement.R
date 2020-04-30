##Prétraitements des données
library(CASdatasets)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     
library(glue)       
library(zeallot)
library(tfruns)

data("freMTPLfreq")

## Pré-traitement idem 
# on traite Power comme entier;
# Gas, Brand, Region comme facteur;
# toutes les expositions plus élevées qu'1 sont ramenées à 1;
# les assurés plus âgés que 85 sont ramenés à 85,
# âge maximal des autos à 20 ans

dat <- freMTPLfreq %>%
  as_tibble() %>%
  mutate_at(vars(Gas, Brand, Region), factor) %>%
  mutate_at(vars(Power),as.integer) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))%>% 
  mutate(DriverAge= ifelse(DriverAge > 85,85,DriverAge))%>% 
  mutate(CarAge = ifelse(CarAge > 20,20,CarAge))


## Même échantillonnage stratifié que dans Pre-traitement.R
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

# Recette différente
# Cette fois-ci, les variables Brand et Region sont transformées
# en entier avec une base à 0. 
rec_obj <-
  recipe(ClaimNb ~ ., 
         data = learnNN) %>% step_rm(PolicyID)%>%
  step_log(Density) %>%
  step_range(CarAge, DriverAge, Density,Power) %>% # min max
  step_dummy(Gas,
             one_hot = F,
             preserve = F) %>% 
  step_integer(Region,Brand,zero_based = T) %>% 
  prep(training = learnNN)


## Aplication de la recette
learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) 
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)


# On crée 4 matrices d'intrants et un vecteur de variable réponse
# entrainement
Xlearn <- as.matrix(learn_prepped[, c(2,3,4,7,9)])  
Brlearn <- as.matrix(learn_prepped$Brand)
Relearn <- as.matrix(learn_prepped$Region)
Vlearn <- as.matrix(log(learn_prepped$Offset))
Ylearn <- as.matrix(learn_prepped$ClaimNb)

# test
Xtest <- as.matrix(test_prepped[, c(2,3,4,7,9)])  
Brtest <- as.matrix(test_prepped$Brand)
Retest <- as.matrix(test_prepped$Region)
Vtest <- as.matrix(log(test_prepped$Offset))
Ytest <- as.matrix(test_prepped$ClaimNb)

# validation 
Xval <- as.matrix(val_prepped[, c(2,3,4,7,9)])  
Brval <- as.matrix(val_prepped$Brand)
Reval <- as.matrix(val_prepped$Region)
Vval <- as.matrix(log(val_prepped$Offset))
Yval <- as.matrix(val_prepped$ClaimNb)



# Pour initialisation du réseau
lambda.hom <- sum(learn_prepped$ClaimNb)/sum(learn_prepped$Offset)


# Même fonction de perte sur mesure  que dans Pre-traitement.R
Poisson.Deviance <- function(y_true,y_pred){
  
  2*(k_mean(y_pred) - k_mean(y_true) +k_mean(k_log(((y_true + k_epsilon()) / (y_pred + k_epsilon())) ^ y_true)))
  
}