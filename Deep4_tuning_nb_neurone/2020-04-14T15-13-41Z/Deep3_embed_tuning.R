# Hyperparameter flags ---------------------------------------------------

FLAGS <- flags(
  flag_numeric("dropout1", 0.25), 
  flag_numeric("dropout2", 0.25),
  flag_numeric("dropout3", 0.25),
  flag_integer("hidden1",32),
  flag_integer("hidden2",32),
  flag_integer("hidden3",32),
  flag_numeric("l1_1",0),
  flag_numeric("l2_1",0),
  flag_numeric("l1_2",0),
  flag_numeric("l2_2",0),
  flag_numeric("l1_3",0),
  flag_numeric("l2_3",0)
)


# hyperparameters of the neural network architecture
BrLabel <- length(unique(learn_prepped$Brand))
ReLabel <- length(unique(learn_prepped$Region))

d <- 2         # dimensions embedding layers for categorical features
# define the network architecture
Design   <- layer_input(shape = c(ncol(Xlearn)),  dtype = 'float32', name = 'Design')
Brand <- layer_input(shape = c(1),   dtype = 'int32', name = 'Brand')
Region   <- layer_input(shape = c(1),   dtype = 'int32', name = 'Region')
LogVol   <- layer_input(shape = c(1),   dtype = 'float32', name = 'LogVol')
#
BrandEmb = Brand %>% 
  layer_embedding(input_dim = BrLabel, output_dim = d, input_length = 1, name = 'BrandEmb') %>%
  layer_flatten(name='Brand_flat')

RegionEmb = Region %>% 
  layer_embedding(input_dim = ReLabel, output_dim = d, input_length = 1, name = 'RegionEmb') %>%
  layer_flatten(name='Region_flat')



Network = list(Design, BrandEmb, RegionEmb) %>% layer_concatenate(name='concate') %>% 
  layer_dense(units=FLAGS$hidden1, 
              activation='relu', 
              name='hidden1',
              kernel_initializer = initializer_he_normal(seed=1L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1_1, l2 = FLAGS$l2_1)) %>%
  layer_batch_normalization() %>% 
  layer_dropout(rate = FLAGS$dropout1) %>% 
  layer_dense(units=FLAGS$hidden2, 
              activation='relu', 
              name='hidden2',
              kernel_initializer = initializer_he_normal(seed=2L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1_2, l2 = FLAGS$l2_2)) %>%
  layer_batch_normalization() %>% 
  layer_dropout(rate = FLAGS$dropout2) %>% 
  layer_dense(units=FLAGS$hidden3, 
              activation='relu', 
              name='hidden2',
              kernel_initializer = initializer_he_normal(seed=3L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1_3, l2 = FLAGS$l2_3)) %>%
  layer_batch_normalization() %>%
  layer_dense(units=1, 
              activation='linear', 
              name='Network', 
              weights=list(array(0, dim=c(FLAGS$hidden2,1)), 
                           array(log(lambda.hom), dim=c(1))
              )
  )

Response = list(Network, LogVol) %>% layer_add(name='Add') %>% 
  layer_dense(units=1, activation=k_exp, name = 'Response', trainable=FALSE,
              weights=list(array(1, dim=c(1,1)), array(0, dim=c(1))))


model <- keras_model(inputs = c(Design, Brand, Region, LogVol), outputs = c(Response))



model %>% compile(optimizer = "nadam", loss = Poisson.Deviance)


history <- model %>% fit(list(Xlearn, Brlearn, Relearn, Vlearn), 
                         Ylearn,
                         validation_data=list(list(Xval, Brval, Reval, Vval),Yval),
                         epochs=100, 
                         batch_size=8192,
                         callbacks=list(
                           callback_early_stopping(patience=10),
                           callback_reduce_lr_on_plateau(factor=0.005)
                         )
)


data_fit <- as.data.frame(history)


ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()

score <- model %>% evaluate(
  list(Xtest, Brtest, Retest, Vtest), 
  Ytest,
  verbose = 0
)