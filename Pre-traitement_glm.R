##Prétraitements des données
library(CASdatasets)
library(keras)
library(tidyverse)
library(recipes)     
library(glue)        
library(zeallot)
library(tfruns)
data("freMTPLfreq")

# Pré-traitement idem
dat <- freMTPLfreq %>%
  as_tibble() %>%
  mutate_at(vars(Gas, Brand, Region), factor) %>%
  mutate_at(vars(Power),as.integer) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure)) %>% 
  mutate(DriverAge= ifelse(DriverAge > 85,85,DriverAge))%>% 
  mutate(CarAge = ifelse(CarAge > 20,20,CarAge))


# Échantillonnage stratifié

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

# on crée des objets distincts (learnglm,testglm,valglm), puisqu'on va utiliser
# les échantillons glm et échantillons des réseaux de neurones en même temps
# lors de l'interprétation
testglm <- dat[-ll,]


##Défintion des indices pour l'échantillon de validation
set.seed(200)
ll2 <- sample(which(learn$ClaimNb==0), round(0.75*length(which(learn$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learn$ClaimNb==1), round(0.75*length(which(learn$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==2), round(0.75*length(which(learn$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==3), round(0.75*length(which(learn$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==4), round(0.75*length(which(learn$ClaimNb==4))), replace = FALSE))

## on remet l'indiçage aléatoire
ll2 <- sample(ll2,size=length(ll2),replace = F)

learnglm <- learn[ll2,]
valglm <- learn[-ll2,]

