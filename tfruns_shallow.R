library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  

source("Pre-traitement.R")
runs <- tuning_run("Shallow_tuning.R", sample=0.005,
                   runs_dir = "shallow_tuning",
                   flags = list(
                     dropout1 = c(0,0.1,0.05,0.01),
                     optimizer= c('rmsprop',"adam"),
                     hidden1=c(10,20,30,40,50,100),
                     batch=c(5000,10000),
                     act=c("relu","tanh"),
                     epochs=500,
                     l1=c(0,0.01,0.05,0.1),
                     l2=c(0,0.01,0.05,0.1)
                     )
                   )
