library(tfruns)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
source("Pre-traitement.R")

## Attention: peut être long à rouler
## Les résultats qui figurent dans le rapport sont disponible dans le dépôt.
## Il s'agit de simplement faire rouler seulement les fonctions ls_runs 


runs <- tuning_run("Deep_3hidden_tuning.R", 
                   runs_dir = "Deep3_tuning_nb_neurone",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0),
                     hidden1 = c(8, 16, 32),
                     hidden2 = c(8, 16, 32),
                     hidden3 = c(8, 16, 32),
                     l1 = c(0),
                     l2 = c(0))
)

view(ls_runs(runs_dir = "Deep3_tuning_nb_neurone"))

tab_deep3 <- ls_runs(runs_dir = "Deep3_tuning_nb_neurone",latest_n=12,order=metric_val_loss,decreasing = F)

## on tune les dropouts pour la meilleure combinaison des neurones
runs <- tuning_run("Deep_3hidden_tuning.R",
                   runs_dir = "Deep3_tuning_32_16_8_dropout",
                   flags = list(
                     dropout1 = c(0, 0.25, 0.5),
                     dropout2 = c(0, 0.25, 0.5),
                     hidden1 = c(32),
                     hidden2 = c(16),
                     hidden3 = c(8),
                     l1 = c(0),
                     l2 = c(0))
)

view(ls_runs(runs_dir = "Deep3_tuning_32_16_8_dropout"))
tab_deep3_32_16_8 <- ls_runs(runs_dir = "Deep3_tuning_32_16_8_dropout",order=metric_val_loss,decreasing = F)

runs <- tuning_run("Deep_3hidden_tuning.R",
                   runs_dir = "Deep3_tuning_32_16_8_dropout",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0.5),
                     hidden1 = c(32),
                     hidden2 = c(16),
                     hidden3 = c(8),
                     l1=c(0, 0.00001),
                     l2=c(0, 0.00001))
)

view(ls_runs(runs_dir = "Deep3_tuning_32_16_8_dropout"))
tab_deep3_32_16_8 <- ls_runs(runs_dir = "Deep3_tuning_32_16_8_dropout",order=metric_val_loss,decreasing = F)


runs <- tuning_run("Deep_3hidden_tuning.R",
                   runs_dir = "Deep3_tuning_8_8_8_dropout",
                   flags = list(
                     dropout1 = c(0, 0.25, 0.5),
                     dropout2 = c(0, 0.25, 0.5),
                     hidden1 = c(8),
                     hidden2 = c(8),
                     hidden3 = c(8),
                     l1 = c(0),
                     l2 = c(0))
)

view(ls_runs(runs_dir = "Deep3_tuning_8_8_8_dropout"))
tab_deep3_8_8_8 <- ls_runs(runs_dir = "Deep3_tuning_8_8_8_dropout",order=metric_val_loss,decreasing = F)



