## Pour faire rouler directement décommenter la prochaine ligne 
#source("Pré-traitement_couche_plongement.R")


# Hyperparameter flags ---------------------------------------------------

FLAGS <- flags(
  flag_numeric("dropout1", 0), 
  flag_numeric("dropout2", 0.25),
  flag_integer("hidden1",32),
  flag_integer("hidden2",16),
  flag_numeric("l1",0),
  flag_numeric("l2",0)
)


BrLabel <- length(unique(learn_prepped$Brand))
ReLabel <- length(unique(learn_prepped$Region))

d <- 2         # dimensions des couches de plongement

# On définit les autres couches d'intrants

#matrice d'intrants sans les variables catégorielles
Design   <- layer_input(shape = c(ncol(Xlearn)),  dtype = 'float32') 

# Couche pour la variable Brand 
Brand <- layer_input(shape = c(1),   dtype = 'int32') 

# couche pour la variable Region
Region   <- layer_input(shape = c(1),   dtype = 'int32')

# couche pour le offser préalablement transformé sur les logarithmes
LogVol   <- layer_input(shape = c(1),   dtype = 'float32')

# on définit la couche de plongement pour Brand
BrandEmb = Brand %>% 
  # elle prend comme intrant la couche d'intrant et fait sortir un vecteur de dimension d
  layer_embedding(input_dim = BrLabel, 
                  output_dim = d, 
                  input_length = 1) %>%
  layer_flatten()

# même chose pour Region
RegionEmb <-  Region %>% 
  layer_embedding(input_dim = ReLabel, 
                  output_dim = d, 
                  input_length = 1) %>%
  layer_flatten()




Network <- list(Design, BrandEmb, RegionEmb) %>% 
  layer_concatenate() %>% 
  layer_dense(units = FLAGS$hidden1, 
              activation = 'relu',
              kernel_initializer = initializer_he_normal(seed=1L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, 
                                                     l2 = FLAGS$l2)) %>%
  
  layer_batch_normalization() %>% 
  layer_dropout(rate = FLAGS$dropout1) %>% 
  
  layer_dense(units = FLAGS$hidden2, 
              activation =' relu',
              kernel_initializer = initializer_he_normal(seed=2L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, 
                                                     l2 = FLAGS$l2)) %>%
  
  layer_batch_normalization() %>% 
  layer_dropout(rate = FLAGS$dropout2) %>% 
  
  layer_dense(units = 1, 
              activation = 'linear',
              weights = list(array(0, dim=c(FLAGS$hidden2,1)), 
                             array(log(lambda.hom), dim=c(1))
                             )
              )

Response <- list(Network, LogVol) %>% 
  layer_add() %>% 
  layer_dense(units = 1, 
              activation = k_exp,
              trainable = FALSE,
              weights = list(array(1, dim=c(1,1)), 
                             array(0, dim=c(1))
                             )
              )


model_deep2_embed <- keras_model(inputs = c(Design, Brand, Region, LogVol), outputs = c(Response))



model_deep2_embed %>% compile(optimizer = "nadam", loss = Poisson.Deviance)


hist_deep2_embeded <- model_deep2_embed %>% 
  fit(x = list(Xlearn, Brlearn, Relearn, Vlearn),
      y = Ylearn,
      validation_data = list(list(Xval, Brval, Reval, Vval),Yval),
      epochs = 1000,
      batch_size = 8192,
      callbacks = list(callback_early_stopping(patience = 20, restore_best_weights = T),
                       callback_reduce_lr_on_plateau(factor = 0.05)
                       )
      )

data_fit <- as.data.frame(hist_deep2_embeded)


ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()

score_deep2_embed <- model_deep2_embed %>% 
  evaluate(x = list(Xtest, Brtest, Retest, Vtest),
           Ytest,
           verbose = 0
           )
