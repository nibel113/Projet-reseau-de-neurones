library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  


source("Pre-traitement.R")
runs <- tuning_run("Shallow_tuning.R",sample = 0.25,
                   runs_dir = "shallow_tuning_select_hidden_10",
                   flags = list(
                     dropout1 = c(0,0.05,0.1),
                     optimizer= c("adam"),
                     hidden1=c(10),
                     batch=c(10000),
                     act=c("relu"),
                     epochs=500,
                     l1=c(0,0.01,0.05,0.1),
                     l2=c(0,0.01,0.05,0.1)
                     )
                   )
