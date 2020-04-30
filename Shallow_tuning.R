# Hyperparameter flags ---------------------------------------------------

## Flags qu'on utilise dans le réseau.
## Les valeurs par défaut sont les valeurs optimales trouvées
## On peut rouler le code içi directement ou à partir des fichiers 
## tfruns_*.R

## Pour faire rouler directement décommenter la prochaine ligne 
#source("Pre-traitement.R")

## les Flags peuvent des numeric, integer, string et boolean
## donc tout ce qui est dans ces catégories peut être tuner.
## Les "<noms>" sont les référence dans le modèle à l'aide de 
## FLAGS$<noms> et dans le fichier tfruns_*.R à l'aide de seulement <noms>
FLAGS <- flags(
  flag_numeric("dropout1", 0.25),
  flag_integer("hidden1",32),
  flag_numeric("l1",0),
  flag_numeric("l2",0)
)

## initialisation du réseau à partir de la moyenne observée
lambda.hom <- sum(learnNN$ClaimNb)/sum(learnNN$Exposure)



# Définition du modèle --------------------------------------------------------------

features.0 <- layer_input(shape = c(ncol(XlearnNN)))         #couche d'intrant


## définition de la structure du réseau

## on ajoute de façon séquentielle les couches 
## l'opérateur %>% permet une meilleure lecture du code
## il permet de réduire l'utilisation des parenthèses

## Exemple: layer_dense(features.0,units = ...) est équivalent à ce qui suit 
## features.0 est le premier argument de layer_dense


net <- features.0 %>%
  #on ajoute kernel initializer direct dans le dense layer 
  layer_dense(units = FLAGS$hidden1,
              activation = "relu",
              # le seed permet une certaine repoductibilité,
              # mais le nombre de poids à initialiser dépend du nombre de neurones.
              # Si on teste deux différents dropouts pour le même nombre de neurones,par exemple,
              # ça permet de garder les même poids à l'initialisation.
              kernel_initializer = initializer_he_normal(seed = 1L),
              kernel_regularizer = regularizer_l1_l2(l1 = FLAGS$l1, l2 = FLAGS$l2)) %>%   # Flags définis pour les taux de régularisation
  
  layer_batch_normalization() %>%  # couche de normalisation
  layer_dropout(FLAGS$dropout1) %>%  # taux de dropout
  
  layer_dense(units = 1, 
              activation = 'linear', 
              # le paramètre weights prend comme argument une liste de 2 vecteur
              # le premier est le vecteur de poids de la couche, il doit être de la dimension:
              # c(intrant, sortie), le deuxième est le vecteur des biais de dimensions: c(sortie)
              # en initialisant ainsi, la sortie du réseau au départ est log(lambda.hom),soit la 
              # moyenne observée
              weights = list(array(0, dim=c(FLAGS$hidden1,1)), 
                             array(log(lambda.hom), dim=c(1))
                             )
              ) 

volumes.0 <- layer_input(shape=c(1))                     # couche d'intrant pour le offset

merged <- list(net, volumes.0) %>%    
  # on ajoute le offset et on applique la transformation exp
  layer_add(name='Add') %>% 
  # on applique la transformation exp, k_exp est la fonction de keras le permettant
  # on spécifie trainable=False pour ne pas entrainer le poids de cette couche
  # elle sert seulement à appliquer une transformation, le poids est à 1 et il n'y a pas de biais
  layer_dense(units = 1, 
              activation = k_exp, 
              trainable = FALSE,
              weights = list(array(1, dim=c(1,1)), 
                             array(0, dim=c(1))
                             )
              )

## On construit le modèle 
## la fonction keras_model prend en argument les intrants et la sortie du réseau 
model_shallow <- keras_model(inputs = list(features.0, volumes.0), outputs = merged)


## Phase de compilation
## C'est içi qu'on spécifie la fonction de perte, l'algorithme utilisé et les métriques additionnelles
## on utilise nadam qui est une version de "adam" avec une accelération Nesterov
## on applique la deviance de poisson définie dans le fichier Pre-traitement.R
model_shallow %>% compile(loss = Poisson.Deviance, optimizer = "nadam")

# Entrainement & évaluation ----------------------------------------------------

hist_shallow <- model_shallow %>% 
  fit(x = list(XlearnNN, WlearnNN), 
      y = YlearnNN,
      # on doit fournir les données de validation sous forme de list(intrant,sortie)
      validation_data = list(list(XvalNN,WvalNN),YvalNN),
      epochs = 1000, 
      batch_size = 8192,
      ## Définition de la règle d'arrêt et de la diminution du taux d'apprentissage
      callbacks=list(callback_early_stopping(patience = 20,restore_best_weights = T),
                     callback_reduce_lr_on_plateau(factor = 0.05)
                     )
      )



## visualisation de l'entrainement

data_fit <- as.data.frame(hist_shallow)

ggplot(data_fit[which(!is.na(data_fit$value)),],aes(x=epoch,y=value,col=data))+
  geom_point()




## Évaluation sur les données test
score_shallow<- model_shallow %>% 
  evaluate(
    x=list(XtestNN,WtestNN), 
    y=YtestNN,
    verbose = 0
    )



