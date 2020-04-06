library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  


source("Pre-traitement.R")
runs <- tuning_run("Shallow_tuning.R",
                   runs_dir = "choix_NombreDeCoucheShallow",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     optimizer= c("nadam"),
                     hidden1=c(16,32,64,128,256,512),
                     batch=c(8192),
                     act=c("relu"),
                     epochs=5000,
                     l1=c(0),
                     l2=c(0)
                     )
                   )
