library(tfruns)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
data("freMTPLfreq")
# Data Preparation ---------------------------------------------------

dat <- freMTPLfreq %>% 
  as_tibble() %>% 
  mutate_at(vars(Power, Gas,Brand,Region), factor) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))



set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.8*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.8*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.8*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.8*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.8*length(which(dat$ClaimNb==4))), replace = FALSE))
learn <- dat[ll,]
testNN <- dat[-ll,]


##Défintion des indices pour l'échantillon de validation
ll2 <- sample(which(learn$ClaimNb==0), round(0.75*length(which(learn$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learn$ClaimNb==1), round(0.75*length(which(learn$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==2), round(0.75*length(which(learn$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==3), round(0.75*length(which(learn$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==4), round(0.75*length(which(learn$ClaimNb==4))), replace = FALSE))


learnNN <- learn[ll2,]
valNN <- learn[-ll2,]

rec_obj <- recipe(ClaimNb ~ ., # Throw out id column, but use all other variables as predictors
                  data = learnNN %>% select(-PolicyID)) %>% 
  step_range(CarAge, DriverAge, Density) %>% # min max
  step_dummy(Power, Gas,Brand,Region,one_hot = T,preserve = F) %>% 
  prep(training = learnNN)


# Use recipe to "bake" the final data 
learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) # Bake the recipe
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)

features <- c(2:4,6:36)
XlearnNN <- as.matrix(learn_prepped[,features])
YlearnNN <- as.numeric(as.matrix(learn_prepped[,5]))
WlearnNN <- as.matrix(learn_prepped[,1])

XvalNN <- as.matrix(val_prepped[,features])
YvalNN <- as.numeric(as.matrix(val_prepped[,5]))
WvalNN <- as.matrix(val_prepped[,1])

XtestNN <- as.matrix(test_prepped[,features])
YtestNN <- as.numeric(as.matrix(test_prepped[,5]))
WtestNN <- as.matrix(test_prepped[,1])

runs <- tuning_run("Shallow_tuning.R", sample=0.005,
                   runs_dir = "shallow_tuning_poisdeviance",
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
