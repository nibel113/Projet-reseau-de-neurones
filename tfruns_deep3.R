library(tfruns)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
source("Pre-traitement.R")

runs <- tuning_run("Deep_3hidden_tuning.R", sample = 0.002, 
                   runs_dir = "Deep3_tuning",
                   flags = list(
                     dropout1 = c(0.01, 0.1, 0.05),
                     dropout2 = c(0.01, 0.1, 0.05),
                     dropout3 = c(0.01, 0.1, 0.05),
                     optimizer= c('rmsprop', 'adam'),
                     hidden1=c(10,30,50,100),
                     hidden2=c(10,30,50,100),
                     hidden3=c(10,30,50,100),
                     batch=c(5000,10000),
                     act=c("relu"),
                     epochs=500,
                     lr_annealing=c(0.1,0.05),
                     l1=c(0,0.01,0.05,0.1),
                     l2=c(0,0.01,0.05,0.1))
)
