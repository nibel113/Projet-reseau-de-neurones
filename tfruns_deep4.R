library(tfruns)

library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
source("Pre-traitement.R")

## Attention: peut être long à rouler
## Les résultats qui figurent dans le rapport sont disponible dans le dépôt.
## Il s'agit de simplement faire rouler seulement les fonctions ls_runs 


runs <- tuning_run(
  "Deep_4hidden_tuning.R",
  runs_dir = "Deep4_tuning_nb_neurone",
  flags = list(
    dropout1 = c(0),
    dropout2 = c(0),
    dropout3 = c(0),
    hidden1 = c(8, 16, 32),
    hidden2 = c(8, 16, 32),
    hidden3 = c(8, 16),
    hidden4 = c(8, 16),
    l1 = c(0),
    l2 = c(0)
  )
)

View(ls_runs(runs_dir = "Deep4_tuning_nb_neurone"))

tab_deep4 <- ls_runs(runs_dir = "Deep4_tuning_nb_neurone",latest_n = 36,order = metric_val_loss,decreasing = F)

runs <- tuning_run(
  "Deep_4hidden_tuning.R",
  runs_dir = "Deep4_tuning_16_16_16_16",
  flags = list(
    dropout1 = c(0, 0.5),
    dropout2 = c(0, 0.5),
    dropout3 = c(0, 0.5),
    hidden1 = c(16),
    hidden2 = c(16),
    hidden3 = c(16),
    hidden4 = c(16),
    l1=c(0),
    l2=c(0)
  )
)

View(ls_runs(runs_dir = "Deep4_tuning_16_16_16_16"))

tab_deep4_16 <- ls_runs(runs_dir = "Deep4_tuning_16_16_16_16",order = metric_val_loss,decreasing = F)


runs <- tuning_run(
  "Deep_4hidden_tuning.R",
  runs_dir = "Deep4_tuning_16_32_8_16_l2",
  flags = list(
    dropout1 = c(0, 0.5),
    dropout2 = c(0, 0.5),
    dropout3 = c(0, 0.5),
    hidden1 = c(16),
    hidden2 = c(32),
    hidden3 = c(8),
    hidden4 = c(16),
    l1=c(0),
    l2=c(0)
  )
)

View(ls_runs(runs_dir = "Deep4_tuning_16_32_8_16_l2"))


runs <- tuning_run(
  "Deep_4hidden_tuning.R",
  runs_dir = "Deep4_tuning_16_32_8_16_l2",
  flags = list(
    dropout1 = c(0),
    dropout2 = c(0),
    dropout3 = c(0.5),
    hidden1 = c(16),
    hidden2 = c(32),
    hidden3 = c(8),
    hidden4 = c(16),
    l1 = c(0, 0.000001),
    l2 = c(0, 0.000001)
  )
)

View(ls_runs(runs_dir = "Deep4_tuning_16_32_8_16_l2"))
tab_deep4_16_32_8_16 <- ls_runs(runs_dir = "Deep4_tuning_16_32_8_16_l2",order = metric_val_loss,decreasing = F)

runs <- tuning_run(
  "Deep_4hidden_tuning.R",
  runs_dir = "Deep4_tuning_16_16_16_16",
  flags = list(
    dropout1 = c(0.5),
    dropout2 = c(0),
    dropout3 = c(0),
    hidden1 = c(16),
    hidden2 = c(16),
    hidden3 = c(16),
    hidden4 = c(16),
    l1 = c(0, 0.000001),
    l2 = c(0, 0.000001)
  )
)



tab_deep4_16 <- ls_runs(runs_dir = "Deep4_tuning_16_16_16_16",order = metric_val_loss,decreasing = F)


