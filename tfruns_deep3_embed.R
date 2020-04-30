library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  


source("Pré-traitement_couche_plongement.R")

## Attention: peut être long à rouler
## Il n'y a pas de résultats sur le dépôt pour ces runs, car il n'ont 
## pas été utilisé pour le rapport par manque de temps.
## Les hyperparamètres trouvés pour les réseaux sans couche de plongement 
## ont été utilisés

runs <- tuning_run("Deep3_embed_tuning.R",sample=0.1,
                   runs_dir = "Deep3_embedded_nb_neuronne",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0),
                     hidden1=c(32,512),
                     hidden2=c(16,32,64,128,256,512),
                     hidden3=c(16,32,64,128,256,512),
                     l1=c(0),
                     l2=c(0)
                   )
)


view(ls_runs(runs_dir ="Deep3_embedded_nb_neuronne" ))
