library(tfruns)

library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
source("Pre-traitement.R")



runs <- tuning_run(
  "Deep_4hidden_tuning.R",
  sample = 0.5,
  runs_dir = "Deep4_tuning_numb_neurone",
  flags = list(
    dropout1 = c(.5),
    #dropout2 = c(0.01, 0.1, 0.05),
    #dropout3 = c(0.01, 0.1, 0.05),
    #dropout4 = c(0.01, 0.1, 0.05),
    optimizer = c('nadam'),
    hidden1 = c(32),
    hidden2 = c(64),
    hidden3 = c(128),
    hidden4 = c(16),
    batch = c(8192),
    act = c("relu"),
    epochs = 500,
    l1 = c(0,0.01,0.05,0.1),
    l2 = c(0,0.01,0.05,0.1)
  )
)


