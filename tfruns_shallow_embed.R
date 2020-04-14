library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  


source("Pré-traitement_couche_plongement.R")

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


runs <- tuning_run("Shallow_embed_tuning.R",
                   runs_dir = "shallow_embedded_512_drop",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     hidden1=c(512),
                     l1=c(0),
                     l2=c(0)
                   )
)

View(ls_runs(runs_dir ="shallow_embedded_512_drop" ))

runs <- tuning_run("Shallow_embed_tuning.R",
                   runs_dir = "shallow_embedded_128_drop",
                   flags = list(
                     dropout1 = c(0.25,0.5),
                     hidden1=c(128),
                     l1=c(0),
                     l2=c(0)
                   )
)

View(ls_runs(runs_dir ="shallow_embedded_128_drop" ))
