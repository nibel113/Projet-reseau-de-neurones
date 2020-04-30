library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     
library(glue)        
library(zeallot)  

## On fait rouler le code pour préparer les données
source("Pre-traitement.R")

# On utilise tuning_run pour faire une recherche d'hyperparamètres.
# Les <noms> doivent être ceux qui figurent dans le fichier "*_tuning.R".
# La fonction permet de créer des dossier automatiquement où chaque "run" sera sauvegarder.
# Plusieurs info seront stockés, dont les hyperparamètres, l'entrainement du modèle via l'objet créer 
# par fit(). 
# Ça offre l'avantage d'ordonner les "runs", mais ça peut être plus long au final
# à tout faire rouler. Voir https://tensorflow.rstudio.com/tools/tfruns/overview/ pour plus d'information.


## on teste d'abord différents nombre de neurones
## en gardant les autres hyperparamètres à 0

## Attention: peut être long à rouler
## La fonction demande toujours de confirmer (valeur par défaut = TRUE) si on veut bien 
## faire une recherche du nombre de combinaison, ici 7, dans le code.
## On peut faire une recherche aléatoire en ajoutant sample = {entre 0 et 1}
## Les résultats qui figurent dans le rapport sont disponible dans le dépôt.
## Il s'agit de simplement faire rouler seulement les fonctions ls_runs 

runs <- tuning_run(file = "Shallow_tuning.R",
                   runs_dir = "shallow_nb_neuronne",
                   confirm = T,
                   flags = list(
                     dropout1 = c(0),
                     hidden1 = c(8,16,32,64,128,256,512),
                     l1 = c(0),
                     l2 = c(0)
                     )
                   )



## ls_runs crée une matrice avec un résumé (complet) des informations de chaque 
## "runs" dans le dossier qu'on lui passe en argument

View(ls_runs(runs_dir = "shallow_nb_neuronne"))



## Sera utile pour la création de tableau dans le fichier product_tableau.R
## On ordonne sur l'erreur de validation du plus petit au plus grand

tab_shal_neur <- ls_runs(runs_dir = "shallow_nb_neuronne",
                         latest_n = 7,
                         order = metric_val_loss,
                         decreasing = F )



## On teste différents hyperparamètres pour 
## la meilleure erreur de validation à l'étape précédente

runs <- tuning_run("Shallow_tuning.R",
                   runs_dir = "shallow_32",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     hidden1=c(32),
                     l1=c(0,0.0001),
                     l2=c(0,0.0001)
                   )
)


tab_shal_32 <- ls_runs(runs_dir = "shallow_32",latest_n = 12,order=metric_val_loss,decreasing=F)


