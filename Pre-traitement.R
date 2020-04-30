##Prétraitements des données
library(CASdatasets)
library(keras)
library(tidyverse)
library(recipes)     
library(glue)        
library(zeallot)
library(tfruns)

data("freMTPLfreq")

## Pré-traitement
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
  mutate(DriverAge= ifelse(DriverAge > 85,85,DriverAge)) %>% 
  mutate(CarAge = ifelse(CarAge > 20,20,CarAge))

## On stratifie les données sur le nombre de réclamations
## on a 20% test, 20% validation et 60% entrainement
set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.8*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.8*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.8*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.8*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.8*length(which(dat$ClaimNb==4))), replace = FALSE))

## on remet l'ordre aléatoire, juste pour être sûr que les données ne  
## sont pas ordonnées lorsque l'algorithme  fait les mini-batch
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


## Création d'une recette pour traiter les données.
## Cette recette est appris sur les données d'entrainement
## et on la cuit, « bake », sur tous les échantillons par la suite.
## Ça fais en sorte que le min et max sont appris sur l'échantillon d'entrainement,
## Dans notre cas, ça fais pas vraiment de différence, car tous les échantillons
## devraient avoir assez de données pour avoir tous les mêmes min et max pour chaque variable.
## Ça serait plus important dans le cas où on standardise en utilisant la moyenne et
## l'écart-type.

rec_obj <-
  recipe(ClaimNb ~ ., 
         data = learnNN) %>% step_rm(PolicyID) %>% #PolicyId n'est pas importante
  step_log(Density) %>%  # on prend le log de Density
  step_range(CarAge, DriverAge, Density,Power) %>% # (x - min(x) )/ (max(x) - min(x) )
  step_dummy(Gas,
             Brand,
             Region,
             one_hot = F,
             preserve = F) %>% # on crée des variables indicatrices
  prep(training = learnNN) # on apprend sur l'échantillon d'entrainement


# On utilise la recette sur chaque échantillon
learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) 
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)

# On doit mettre les données sous forme de matrice.
# Les réseaux ont deux couches d'intrant distintes X et W,
# soit la matrice d'incidence et le offset.
# On crée une matrice pour chaque et un vecteur de la variable réponse

features <- c(2:5,7:22)
XlearnNN <- as.matrix(learn_prepped[,features])
YlearnNN <- as.numeric(as.matrix(learn_prepped[,6]))
# on prend le log de l'exposition
WlearnNN <- as.matrix(log(learn_prepped[,1]))

# Même chose pour l'échantillon de validation et de test
XvalNN <- as.matrix(val_prepped[,features])
YvalNN <- as.numeric(as.matrix(val_prepped[,6]))
WvalNN <- as.matrix(log(val_prepped[,1]))

XtestNN <- as.matrix(test_prepped[,features])
YtestNN <- as.numeric(as.matrix(test_prepped[,6]))
WtestNN <- as.matrix(log(test_prepped[,1]))


## Création d'une fonction de perte sur mesure, 
## on doit utiliser les fonctions de keras, k_**.

Poisson.Deviance <- function(y_true,y_pred){
  
  2*k_mean( y_pred - y_true + k_log( ( y_true + k_epsilon() ) / ( y_pred + k_epsilon() ) ) * y_true, axis = -1)
  
}
