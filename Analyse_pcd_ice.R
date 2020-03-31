#Analyse pcd et ice shallow
library("iml")
library(dplyr)    # for data wrangling
library(ggplot2)  # for general visualization
library(keras)    # for fitting DNNs
library(pdp)      # for partial depe
library(vip)      # for visualizing feature importance
library(glue)        # For conveniently concatenating strings
library(zeallot)
source("best_shallow.R")

ll2 <- sample(which(learn_prepped$ClaimNb==0), round(0.034*length(which(learn_prepped$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learn_prepped$ClaimNb==1), round(0.034*length(which(learn_prepped$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn_prepped$ClaimNb==2), round(0.034*length(which(learn_prepped$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn_prepped$ClaimNb==3), round(0.034*length(which(learn_prepped$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn_prepped$ClaimNb==4), round(0.034*length(which(learn_prepped$ClaimNb==4))), replace = FALSE))

learn_prepped_plot <- learn_prepped[ll2,]

features <- c(2:4,6:36)
XlearnNN_plot <- as.matrix(learn_prepped_plot[,features])
YlearnNN_plot <- as.numeric(as.matrix(learn_prepped_plot[,5]))
WlearnNN_plot <- as.matrix(learn_prepped_plot[,1])

pred_wrapper <- function(object, newdata) {
  predict(object, x = list(as.matrix(newdata[,-35]),as.matrix(newdata[,35]))) %>%
    as.vector()
}

p1 <- vip(
  object = model,                     # fitted model
  method = "permute",                 # permutation-based VI scores
  num_features = ncol(XlearnNN_plot),       # default only plots top 10 features
  pred_wrapper = pred_wrapper,            # user-defined prediction function
  train = as.data.frame(list(XlearnNN_plot, WlearnNN_plot)) ,    # training data
  target = YlearnNN_plot,                   # response values used for training
  metric = "rsquared",                # evaluation metric
  progress = "text"                 # request a text-based progress bar
)
#> Warning: Setting `method = "permute"` is experimental, use at your own
#> risk!
print(p1) 


p2 <- pdp::partial(model, pred.var = "DriverAge", pred.fun = pred_wrapper, 
              train = as.data.frame(list(XlearnNN_plot, WlearnNN_plot))) %>%
  autoplot(alpha = 0.1)
p3 <- pdp::partial(model, pred.var = "Density", pred.fun = pred_wrapper, 
              train = as.data.frame(list(XlearnNN_plot, WlearnNN_plot))) %>%
  autoplot(alpha = 0.1)
grid.arrange(p2, p3, ncol = 2)


pdp_wrapper <- function(object, newdata) {
  predict(object, x = list(as.matrix(newdata[,-35]),as.matrix(newdata[,35]))) %>%
    as.vector() %>%
    mean()  # aggregate ICE curves
}

p4 <- pdp::partial(model, pred.var = "CarAge", chull = TRUE, 
              pred.fun = pdp_wrapper, train = as.data.frame(list(XlearnNN_plot, WlearnNN_plot))) %>%
  autoplot()
print(p4)  # display plot
