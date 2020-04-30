library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     
library(glue)        
library(zeallot)  


source("glm.R")
source("Pré-traitement_couche_plongement.R")

## Attention: peut être long à rouler
## Il n'y a pas de résultats sur le dépôt pour ces runs, car il n'ont 
## pas été utilisé pour le rapport par manque de temps.
## Les hyperparamètres trouvés pour les réseaux sans couche de plongement 
## ont été utilisés

runs <- tuning_run("Cann_deep3_tunning.R",
                   runs_dir = "Deep3_Cann_nb_neuronne",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0),
                     hidden1 = c(8, 16, 32),
                     hidden2 = c(16, 32, 64),
                     hidden3 = c(8, 16, 32),
                     l1=c(0),
                     l2=c(0)
                   )
)

View(ls_runs(runs_dir ="Deep3_Cann_nb_neuronne" ))

