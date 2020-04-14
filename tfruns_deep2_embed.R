library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  


source("Pré-traitement_couche_plongement.R")

runs <- tuning_run("Deep2_embed_tuning.R",sample=0.2,
                   runs_dir = "Deep2_embedded_nb_neurone",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0),
                     hidden1=c(8,16,32,64,128,256,512),
                     hidden2=c(8,16,32,64,128,256,512),
                     l1_1=c(0),
                     l2_1=c(0),
                     l1_2=c(0),
                     l2_2=c(0)
                   )
)

View(ls_runs(runs_dir = "Deep2_embedded_nb_neuronne"))
