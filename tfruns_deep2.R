library(tfruns)

library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
source("Pre-traitement.R")

#runs <- tuning_run("Deep_2hidden_tuning.R", 
#                   runs_dir = "Deep2_tuning_layers",
#                   flags = list(
#                     dropout1 = c(0),
#                     optimizer= c('nadam'),
#                     hidden1=c(10,20,30,40,50,100),
#                     hidden2=c(10,20,30,40,50,100),
#                     batch=c(10000),
#                     act=c("relu"),
#                     epochs=500,
#                     l1=c(0),
#                     l2=c(0)
#                     )
#)

runs <- tuning_run("Deep_2hidden_tuning.R", sample = 0.1, 
                   runs_dir = "Deep2_tuning",
                   flags = list(
                     dropout1 = c(0, 0.1, 0.25),
                     optimizer= c('rmsprop', 'adam',"nadam"),
                     hidden1=c(30),
                     hidden2=c(10),
                     batch=c(5000,10000),
                     act=c("relu"),
                     epochs=500,
                     l1=c(0,0.01,0.05,0.1),
                     l2=c(0,0.01,0.05,0.1)
                   )
)
