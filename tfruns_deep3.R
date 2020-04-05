library(tfruns)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
source("Pre-traitement.R")

runs <- tuning_run("Deep_3hidden_tuning.R", sample = 0.4, 
                   runs_dir = "Deep3_tuning_num_neurone",
                   flags = list(
                     dropout1 = c(0),
                     dropout2 = c(0),
                     dropout3 = c(0),
                     optimizer= c('nadam'),
                     hidden1=c(10,30,50,100),
                     hidden2=c(10,30,50,100),
                     hidden3=c(10,30,50,100),
                     batch=c(10000),
                     act=c("relu"),
                     epochs=1000,
                     l1=c(0),
                     l2=c(0))
)

#runs <- tuning_run("Deep_3hidden_tuning.R", sample = 0.0075, 
                   #runs_dir = "Deep3_tuning",
                  # flags = list(
                    # dropout1 = c(0,0.01, 0.1, 0.05),
                  #   dropout2 = c(0,0.01, 0.1, 0.05),
                 #    dropout3 = c(0,0.01, 0.1, 0.05),
                  #   optimizer= c('rmsprop', 'adam'),
                   #  hidden1=c(30),
                    # hidden2=c(10),
                     #hidden3=c(50),
                    # batch=c(5000,10000),
                     #act=c("relu"),
              #       epochs=500,
               #      l1=c(0,0.01,0.05,0.1),
                #     l2=c(0,0.01,0.05,0.1))
#)
