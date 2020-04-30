library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)    
library(glue)        
library(zeallot)  


source("Pré-traitement_couche_plongement.R")


## Attention: peut être long à rouler
## Il n'y a pas de résultats sur le dépôt pour ces runs, car il n'ont 
## pas été utilisé pour le rapport par manque de temps.
## Les hyperparamètres trouvés pour les réseaux sans couche de plongement 
## ont été utilisés


runs <- tuning_run("Shallow_embed_tuning.R",
                   runs_dir = "shallow_embedded_nb_neuronne",
                   flags = list(
                     dropout1 = c(0),
                     hidden1=c(8,16,32,64,128,256,512),
                     l1=c(0),
                     l2=c(0)
                   )
)

View(ls_runs(runs_dir ="shallow_embedded_nb_neuronne" ))


