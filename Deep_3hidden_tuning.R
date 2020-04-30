## Pour faire rouler directement décommenter la prochaine ligne 
#source("Pre-traitement.R")


# Hyperparameter flags ---------------------------------------------------

FLAGS <- flags(
  flag_numeric("dropout1", 0),
  flag_numeric("dropout2", 0.5),
  flag_integer("hidden1",32),
  flag_integer("hidden2",16),
  flag_integer("hidden3",8),
  flag_numeric("l1",0),
  flag_numeric("l2",0)
)

# Pour l'initialisation du réseau
lambda.hom <- sum(learnNN$ClaimNb)/sum(learnNN$Exposure)

# Définition du modèle --------------------------------------------------------------

features.0 <- layer_input(shape=c(ncol(XlearnNN)))         # couche d'intrant 


net <- features.0 %>%
  
  layer_dense(units = FLAGS$hidden1,
              activation = "relu",
              kernel_initializer = initializer_he_normal(seed=1L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, 
                                                     l2 = FLAGS$l2)) %>% 
  layer_batch_normalization() %>%
  layer_dropout(FLAGS$dropout1) %>% 
  
  layer_dense(units = FLAGS$hidden2,
              activation = "relu",
              kernel_initializer = initializer_he_normal(seed=2L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, 
                                                     l2 = FLAGS$l2)) %>%
  layer_batch_normalization() %>%
  layer_dropout(FLAGS$dropout2) %>% 
  layer_dense(units = FLAGS$hidden3,
              activation = "relu",
              kernel_initializer = initializer_he_normal(seed=3L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, 
                                                     l2 = FLAGS$l2)) %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 1, 
              activation = 'linear', 
              weights = list(array(0, dim=c(FLAGS$hidden3,1)),
                             array(log(lambda.hom), dim=c(1))
                             )
              )

volumes.0 <- layer_input(shape = c(1))                     # couche d'intrants pour le offset

merged <- list(net, volumes.0) %>%                         
  layer_add() %>% 
  layer_dense(units = 1, 
              activation = k_exp, 
              trainable = FALSE,
              weights = list(array(1, dim=c(1,1)),
                             array(0, dim=c(1))
                             )
              )

model_deep3 <- keras_model(inputs = list(features.0, volumes.0), outputs = merged)

model_deep3 %>% compile(loss = Poisson.Deviance, optimizer = "nadam")

# Entrainement et évaluation ----------------------------------------------------

hist_deep3 <-  model_deep3 %>% 
  fit(x = list(XlearnNN, WlearnNN), 
      y = YlearnNN,
      validation_data = list(list(XvalNN,WvalNN),YvalNN),
      epochs = 1000,
      batch_size = 8192,
      callbacks = list(callback_early_stopping(patience = 20, restore_best_weights = T),
                       callback_reduce_lr_on_plateau(factor = 0.5)
                       )
      )

data_fit <- as.data.frame(hist_deep3)


ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x = epoch,y = value,col = data))+
  geom_point()



score_deep3 <- model_deep3 %>% 
  evaluate(
    x = list(XtestNN,WtestNN), 
    y = YtestNN,
    verbose = 0
)

