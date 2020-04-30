library(tfruns)
library(keras)
library(tidyverse)
library(recipes)     
library(glue)        
library(zeallot)  
source("Pre-traitement.R")

## Attention: peut être long à rouler
## Les résultats qui figurent dans le rapport sont disponible dans le dépôt.
## Il s'agit de simplement faire rouler seulement les fonctions ls_runs 



runs <- tuning_run("Deep_2hidden_tuning.R", 
                   runs_dir = "Deep2_tuning_nb_neurone_bon",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0),
                     hidden1=c(8,16,32,64),
                     hidden2=c(8,16,32,64),
                     l1_1=c(0),
                     l2_1=c(0),
                     l1_2=c(0),
                     l2_2=c(0)
                   )
)



View(ls_runs(runs_dir = "Deep2_tuning_nb_neurone_bon"))
tab_deep2 <- ls_runs(runs_dir = "Deep2_tuning_nb_neurone_bon",
                     latest_n = 16,
                     order = metric_val_loss,
                     decreasing = F)


runs <- tuning_run("Deep_2hidden_tuning.R", 
                   runs_dir = "Deep2_tuning_8_8",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     dropout2 = c(0,0.25,0.5),
                     hidden1=c(8),
                     hidden2=c(8),
                     l1=c(0,0.0001),
                     l2=c(0,0.0001)
                   )
)


View(ls_runs(runs_dir = "Deep2_tuning_8_8"))
tab_deep2_88 <- ls_runs(runs_dir = "Deep2_tuning_8_8",
                        order = metric_val_loss,
                        decreasing = F)


runs <- tuning_run("Deep_2hidden_tuning.R", 
                   runs_dir = "Deep2_tuning_32_16",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     dropout2 = c(0,0.25,0.5),
                     hidden1=c(32),
                     hidden2=c(16),
                     l1=c(0),
                     l2=c(0)
                   )
)
View(ls_runs(runs_dir = "Deep2_tuning_32_16"))


runs <- tuning_run("Deep_2hidden_tuning.R", 
                   runs_dir = "Deep2_tuning_32_16",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0.25),
                     hidden1=c(32),
                     hidden2=c(16),
                     l1=c(0,0.0001),
                     l2=c(0,0.0001)
                   )
)
View(ls_runs(runs_dir = "Deep2_tuning_32_16"))

tab_deep2_32_16 <- ls_runs(runs_dir = "Deep2_tuning_32_16",latest_n=8,order = metric_val_loss,decreasing = F)



