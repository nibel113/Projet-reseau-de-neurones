###############################################
#########  neural network (with embeddings)
###############################################

##Prétraitements des données
library(CASdatasets)
library(keras)
library(CASdatasets)
library(tidyverse)
library(recipes)     # Library for data processing
library(glue)        # For conveniently concatenating strings
library(zeallot)
data("freMTPLfreq")

dat <- freMTPLfreq %>%
  as_tibble() %>%
  mutate_at(vars(Power, Gas, Brand, Region), factor) %>%
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))

dat$ClaimNb <- pmin(dat$ClaimNb, 4) 


set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.8*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.8*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.8*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.8*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.8*length(which(dat$ClaimNb==4))), replace = FALSE))

## on remet l'ordre aléatoire
ll <- sample(ll,size=length(ll))

## création test et entrainement
learn <- dat[ll,]
testNN <- dat[-ll,]


##Défintion des indices pour l'échantillon de validation
ll2 <- sample(which(learn$ClaimNb==0), round(0.75*length(which(learn$ClaimNb==0))), replace = FALSE)
ll2 <- c(ll2,sample(which(learn$ClaimNb==1), round(0.75*length(which(learn$ClaimNb==1))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==2), round(0.75*length(which(learn$ClaimNb==2))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==3), round(0.75*length(which(learn$ClaimNb==3))), replace = FALSE))
ll2 <- c(ll2,sample(which(learn$ClaimNb==4), round(0.75*length(which(learn$ClaimNb==4))), replace = FALSE))

## on remet l'indiçage aléatoire
ll2 <- sample(ll2,size=length(ll2))
learnNN <- learn[ll2,]
valNN <- learn[-ll2,]

rec_obj <-
  recipe(ClaimNb ~ ., # Throw out id column, but use all other variables as predictors
         data = learnNN %>% select(-PolicyID)) %>%
  step_log(Density) %>%
  step_range(CarAge, DriverAge, Density) %>% # min max
  step_dummy(Gas,
             one_hot = T,
             preserve = F) %>% 
  step_integer(Power,Region,Brand,zero_based = T) %>% 
  prep(training = learnNN)

learn_prepped <- bake(rec_obj, new_data = learnNN) %>% rename(Offset = Exposure) # Bake the recipe
test_prepped <- bake(rec_obj, new_data = testNN) %>% rename(Offset = Exposure)
val_prepped <- bake(rec_obj, new_data = valNN) %>% rename(Offset=Exposure)
# definition of feature variables (non-categorical)

# learning data
Xlearn <- as.matrix(learn_prepped[, c(3,4,7,9,10)])  # design matrix learning sample
Brlearn <- as.matrix(learn_prepped$Brand)
Relearn <- as.matrix(learn_prepped$Region)
Pwlearn <- as.matrix(learn_prepped$Power)
Ylearn <- as.matrix(learn_prepped$ClaimNb)
# testing data
Xtest <- as.matrix(test_prepped[, c(3,4,7,9,10)])  # design matrix learning sample
Brtest <- as.matrix(test_prepped$Brand)
Retest <- as.matrix(test_prepped$Region)
Pwtest <- as.matrix(test_prepped$Power)
Ytest <- as.matrix(test_prepped$ClaimNb)
# validation data
Xval <- as.matrix(val_prepped[, c(3,4,7,9,10)])  # design matrix learning sample
Brval <- as.matrix(val_prepped$Brand)
Reval <- as.matrix(val_prepped$Region)
Pwval <- as.matrix(val_prepped$Power)
Yval <- as.matrix(val_prepped$ClaimNb)

# choosing the right volumes for EmbNN and CANN
Vlearn <- as.matrix(log(learn_prepped$Offset))
Vtest <- as.matrix(log(test_prepped$Offset))
Vval <- as.matrix(log(val_prepped$Offset))
lambda.hom <- sum(learn_prepped$ClaimNb)/sum(learn_prepped$Offset)

CANN <- 0  # 0=Embedding NN, 1=CANN

if (CANN==1){
  Vlearn <- as.matrix(log(learn$fitGLM2))
  Vtest <- as.matrix(log(test$fitGLM2))
  lambda.hom <- sum(learn$ClaimNb)/sum(learn$fitGLM2)
}
lambda.hom

# hyperparameters of the neural network architecture
BrLabel <- length(unique(learn_prepped$Brand))
ReLabel <- length(unique(learn_prepped$Region))
PwLabel <- length(unique(learn_prepped$Power))
q1 <- 20   
q2 <- 15
q3 <- 10
d <- 2         # dimensions embedding layers for categorical features
# define the network architecture
Design   <- layer_input(shape = c(5),  dtype = 'float32', name = 'Design')
Brand <- layer_input(shape = c(1),   dtype = 'int32', name = 'Brand')
Region   <- layer_input(shape = c(1),   dtype = 'int32', name = 'Region')
Power    <- layer_input(shape = c(1),   dtype = 'int32', name = "Power")
LogVol   <- layer_input(shape = c(1),   dtype = 'float32', name = 'LogVol')
#
BrandEmb = Brand %>% 
  layer_embedding(input_dim = BrLabel, output_dim = d, input_length = 1, name = 'BrandEmb') %>%
  layer_flatten(name='Brand_flat')

RegionEmb = Region %>% 
  layer_embedding(input_dim = ReLabel, output_dim = d, input_length = 1, name = 'RegionEmb') %>%
  layer_flatten(name='Region_flat')

PowerEmb = Power %>% 
  layer_embedding(input_dim = PwLabel, output_dim = d, input_length = 1, name = 'PowerEmb') %>%
  layer_flatten(name='Power_flat')

Network = list(Design, BrandEmb, RegionEmb,PowerEmb) %>% layer_concatenate(name='concate') %>% 
  layer_dense(units=32, activation='relu', name='hidden1') %>%
  layer_dense(units=64, activation='relu', name='hidden2') %>%
  layer_dense(units=16, activation='relu', name='hidden3') %>%
  layer_dense(units=1, activation='linear', name='Network', 
              weights=list(array(0, dim=c(16,1)), array(log(lambda.hom), dim=c(1))))

Response = list(Network, LogVol) %>% layer_add(name='Add') %>% 
  layer_dense(units=1, activation=k_exp, name = 'Response', trainable=FALSE,
              weights=list(array(1, dim=c(1,1)), array(0, dim=c(1))))


model <- keras_model(inputs = c(Design, Brand, Region, Power, LogVol), outputs = c(Response))

Poisson.Deviance <- function(y_true,y_pred){
  
  2*(k_mean(y_pred) - k_mean(y_true) +k_mean(k_log(((y_true + k_epsilon()) / (y_pred + k_epsilon())) ^ y_true)))
  
}

model %>% compile(optimizer = "nadam", loss = Poisson.Deviance)


history <- model %>% fit(list(Xlearn, Brlearn, Relearn, Pwlearn, Vlearn), 
                         Ylearn,
                         validation_data=list(list(Xval, Brval, Reval, Pwval, Vval),Yval),
                         epochs=100, 
                         batch_size=8192,
                         callbacks=list(
                           callback_early_stopping(patience=25,restore_best_weights = T,min_delta = 0.00001),
                           callback_tensorboard(log_dir = "tf_dir"))
)


data_fit <- as.data.frame(history)


ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()+
  scale_y_continuous(limits=c(0.25,.26))

score <- model %>% evaluate(
  list(Xtest, Brtest, Retest, Pwtest, Vtest), 
  Ytest,
  verbose = 0
)
