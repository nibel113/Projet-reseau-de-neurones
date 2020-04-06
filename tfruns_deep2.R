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

runs <- tuning_run("Deep_2hidden_tuning.R", sample=0.2,
                   runs_dir = "Deep2_tuning",
                   flags = list(
                     dropout1 = c(0,0.5),
                     dropout2 = c(0,0.5),
                     #optimizer= c("nadam"),
                     hidden1=c(16,32,64,128,256),
                     hidden2=c(16,32,64,128)#,
                     #batch=c(8192),
                     #act=c("relu"),
                     #epochs=15#,
                     #l1=c(0),
                     #l2=c(0)
                   )
)
