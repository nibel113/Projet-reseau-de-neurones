library(tfruns)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
source("Pre-traitement.R")

## on cherche les meilleures combinaisons de neurones par couche cachée
## sans dropout, ni régularisation
runs <- tuning_run("Deep_3hidden_tuning.R", 
                   runs_dir = "Deep3_tuning_nb_neurone",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0),
                     hidden1=c(16,32,64),
                     hidden2=c(64,256),
                     hidden3=c(16,32),
                     l1_1=c(0),
                     l2_1=c(0),
                     l1_2=c(0),
                     l2_2=c(0),
                     l1_3=c(0),
                     l2_3=c(0))
)

view(ls_runs(runs_dir = "Deep3_tuning_nb_neurone"))

## on tune les dropouts pour la meilleure combinaison des neurones
runs <- tuning_run("Deep_3hidden_tuning.R",
                   runs_dir = "Deep3_tuning_64_64_32_dropout",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     dropout2 = c(0,0.25,0.5),
                     hidden1=c(64),
                     hidden2=c(64),
                     hidden3=c(32),
                     l1_1=c(0),
                     l2_1=c(0),
                     l1_2=c(0),
                     l2_2=c(0),
                     l1_3=c(0),
                     l2_3=c(0))
)

view(ls_runs(runs_dir = "Deep3_tuning_64_64_32_dropout"))

## on tune régularisation lasso
runs <- tuning_run("Deep_3hidden_tuning.R",
                   runs_dir = "Deep3_tuning_64_64_32_dropout0_0.5_l2",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0.5),
                     hidden1=c(64),
                     hidden2=c(64),
                     hidden3=c(32),
                     l1_1=c(0),
                     l2_1=c(0,0.0001),
                     l1_2=c(0),
                     l2_2=c(0,0.0001),
                     l1_3=c(0),
                     l2_3=c(0,0.0001))
)

view(ls_runs(runs_dir = "Deep3_tuning_64_64_32_dropout0_0.5_l2"))

## on tune l1
runs <- tuning_run("Deep_3hidden_tuning.R",
                   runs_dir = "Deep3_tuning_64_64_32_dropout0_0.5_l1",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0.5),
                     hidden1=c(64),
                     hidden2=c(64),
                     hidden3=c(32),
                     l1_1=c(0,0.0001),
                     l2_1=c(0),
                     l1_2=c(0,0.0001),
                     l2_2=c(0),
                     l1_3=c(0,0.0001),
                     l2_3=c(0.0001))
)

view(ls_runs(runs_dir = "Deep3_tuning_64_64_32_dropout0_0.5_l1"))

## On tune la valeur des paramètres choisies
runs <- tuning_run("Deep_3hidden_tuning.R",
                   runs_dir = "Deep3_tuning_64_64_32_drop2_l2",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0.25,0.5),
                     hidden1=c(64),
                     hidden2=c(64),
                     hidden3=c(32),
                     l1_1=c(0),
                     l2_1=c(0),
                     l1_2=c(0),
                     l2_2=c(0),
                     l1_3=c(0),
                     l2_3=c(0.01,0.001,0.0001))
)

view(ls_runs(runs_dir = "Deep3_tuning_64_64_32_drop2_l2"))
##drop2 0.5 et l2 0.001



## on tune la combinaison qui semble apprendre le plus facilement avec 
## le plus petit loss pour le training set, peut-être qu'en y ajoutant des dropout et 
## des régularisations, on sera capable de garder cette facilité sans surajuster.

runs <- tuning_run("Deep_3hidden_tuning.R", 
                   runs_dir = "Deep3_tuning_64_256_32_dropout",
                   flags = list(
                     dropout1 = c(0,.25,.5),
                     dropout2 = c(0,.25,.5),
                     hidden1=c(64),
                     hidden2=c(256),
                     hidden3=c(32),
                     l1_1=c(0),
                     l2_1=c(0),
                     l1_2=c(0),
                     l2_2=c(0),
                     l1_3=c(0),
                     l2_3=c(0))
)
view(ls_runs(runs_dir = "Deep3_tuning_64_256_32_dropout"))


runs <- tuning_run("Deep_3hidden_tuning.R", 
                   runs_dir = "Deep3_tuning_64_256_32_dropout.5_.5_l2",
                   flags = list(
                     dropout1 = c(.5),
                     dropout2 = c(.5),
                     hidden1=c(64),
                     hidden2=c(256),
                     hidden3=c(32),
                     l1_1=c(0),
                     l2_1=c(0,0.0001),
                     l1_2=c(0),
                     l2_2=c(0,0.0001),
                     l1_3=c(0),
                     l2_3=c(0,0.0001))
)
view(ls_runs(runs_dir = "Deep3_tuning_64_256_32_dropout.5_.5_l2"))
