library(CASdatasets)
library(keras)
library(ggplot2)
library(caret)
library(h2o)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)  
data("freMTPLfreq")

dat <- freMTPLfreq

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
  step_center(CarAge, DriverAge, Density) %>% # Subtract column mean 
  step_scale(CarAge, DriverAge, Density) %>%  # Divide columns by standard deviation
  step_log(Exposure) %>%                                # Apply log transform
  prep(training = learnNN)                                # Use `learnNN` set to prepare recipes object

# Use recipe to "bake" the final data 
learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) # Bake the recipe
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)


h2o.init(nthreads = 4,port = 11223) # Use 4 CPUs and custom port
h2o.no_progress()                    # Disable progress bars for nicer output
learn.h2o <- as.h2o(learn_prepped)   # Upload data to h2o
test.h2o <- as.h2o(test_prepped)
val.h2o <- as.h2o(val_prepped)

x <- setdiff(colnames(learn.h2o), c("ClaimNb", "Offset")) 
y <- "ClaimNb"      # Target variable
offset <- "Offset"  # log(exposure)

#Permet de calculer la deviance
get_deviance <- function(y_pred, y_true) {
  2 * (sum(y_pred) - sum(y_true) + sum(log((y_true / y_pred) ^ (y_true)))) / nrow(y_pred)
}


dl_grid <- list(
  hidden = list(10, 20, 50, 100, c(10, 10), c(10, 20), c(20, 10), c(20, 20),
                c(50, 20), c(100, 50), c(10, 10, 10), c(50, 25, 10), c(100, 50, 25),
                c(10, 10, 10, 10), c(20, 20, 20, 20), c(50, 50, 30, 20)),
  
  activation = c("Rectifier", "Tanh", "Maxout", "RectifierWithDropout",
                 "TanhWithDropout", "MaxoutWithDropout"),
  
  input_dropout_ratio = c(0, 0.05, 0.1),
  l1 = seq(0, 1e-4, 1e-6),
  l2 = seq(0, 1e-4, 1e-6),
  epochs = 500
)

strategy  <-  list(strategy = "RandomDiscrete",
                   max_runtime_secs = 1200,
                   seed = 1,
                   stopping_rounds = 5,           # Early stopping
                   stopping_tolerance = 0.001,
                   stopping_metric = "deviance")

dl_random_grid <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "dl_grid_random",
  training_frame = learn.h2o,
  validation_frame= val.h2o,
  seed = 1,
  
  x = setdiff(colnames(learn.h2o), c("ClaimNb", "Offset")),
  y = "ClaimNb",
  offset_column = "Offset",
  distribution = "poisson",
  
  hyper_params = dl_grid,
  search_criteria = strategy,
  standardize=F,
  categorical_encoding="OneHotExplicit"
)

dnn_gridperf <- h2o.getGrid(dl_random_grid,sort_by = "deviance")

best_model <- h2o.getModel(dl_random_grid@summary_table$model_ids[[1]])

pred_learn <- predict(best_model, learn.h2o)
pred_test <- predict(best_model, test.h2o)

in_sample <- get_deviance(pred_learn, learn.h2o$ClaimNb)
out_of_sample <- get_deviance(pred_test, test.h2o$ClaimNb)