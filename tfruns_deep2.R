library(tfruns)

library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
source("Pre-traitement.R")


runs <- tuning_run("Deep_2hidden_tuning.R", 
                   runs_dir = "Deep2_tuning_nb_neurone_bon",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0),
                     hidden1=c(16,32,64,128,256),
                     hidden2=c(16,32,64,128),
                     l1=c(0),
                     l2=c(0)
                   )
)
View(ls_runs(runs_dir = "Deep2_tuning_nb_neurone_bon"))

runs <- tuning_run("Deep_2hidden_tuning.R", sample=0.01,
                   runs_dir = "Deep2_tuning_16_16",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     dropout2 = c(0,0.25,0.5),
                     hidden1=c(16),
                     hidden2=c(16),
                     l1_1=c(0,0.001,0.0001),
                     l2_1=c(0,0.001,0.0001),
                     l1_2=c(0,0.001,0.0001),
                     l2_2=c(0,0.001,0.0001)
                   )
)

View(ls_runs(runs_dir = "Deep2_tuning_16_16"))

runs <- tuning_run("Deep_2hidden_tuning.R", sample=0.01,
                   runs_dir = "Deep2_tuning_32_128",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     dropout2 = c(0,0.25,0.5),
                     hidden1=c(32),
                     hidden2=c(128),
                     l1_1=c(0,0.001,0.0001),
                     l2_1=c(0,0.001,0.0001),
                     l1_2=c(0,0.001,0.0001),
                     l2_2=c(0,0.001,0.0001)
                   )
)

View(ls_runs(runs_dir = "Deep2_tuning_32_128"))

runs <- tuning_run("Deep_2hidden_tuning.R", sample=0.01,
                   runs_dir = "Deep2_tuning_32_16",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     dropout2 = c(0,0.25,0.5),
                     hidden1=c(32),
                     hidden2=c(16),
                     l1_1=c(0,0.001,0.0001),
                     l2_1=c(0,0.001,0.0001),
                     l1_2=c(0,0.001,0.0001),
                     l2_2=c(0,0.001,0.0001)
                   )
)
View(ls_runs(runs_dir = "Deep2_tuning_32_16"))

runs <- tuning_run("Deep_2hidden_tuning.R", sample=0.01,
                   runs_dir = "Deep2_tuning_128_32",
                   flags = list(
                     dropout1 = c(0,0.25,0.5),
                     dropout2 = c(0,0.25,0.5),
                     hidden1=c(128),
                     hidden2=c(32),
                     l1_1=c(0,0.001,0.0001),
                     l2_1=c(0,0.001,0.0001),
                     l1_2=c(0,0.001,0.0001),
                     l2_2=c(0,0.001,0.0001)
                   )
)
View(ls_runs(runs_dir = "Deep2_tuning_128_32"))
