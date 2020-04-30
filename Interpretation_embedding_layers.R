# Helper packages
library(dplyr)      
library(ggplot2)    
library(recipes)   
library(iml)       

## Avant de faire rouler ce code il faut avoir un modèle Cann avec couche de 
## plongement d'entrainer, et on passera son nom à l'argument model de la fonction Predictor 
## plus bas. 

## À rouler seulement, si leurs résultats ne figurent pas dans l'environnement de 
## travail. Normalement, si un modèle est prêt, on ne devrait pas à avoir à les faire rouler
#source("glm.R")
#source("Pré-traitement_couche_plongement.R")

## Pré-traitement
# On y va différemment ici, pusiqu'on faire la transformation
# des données à l'intérieur complétement de la fonction qu'on crée plus bas.
# L'idée est qu'on veut que les graphiques retournent des valeurs avec les 
# variables sous leurs formes de base, sauf pour Density, où on veut 
# avoir les graphiques sous log.

dat2 <- freMTPLfreq %>%
  as_tibble() %>%
  mutate_at(vars(Gas, Brand, Region,Power), factor) %>%
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))%>% 
  mutate(DriverAge= ifelse(DriverAge > 85,85,DriverAge))%>% 
  mutate_at(vars(Density),log) %>% 
  mutate(CarAge = ifelse(CarAge > 20,20,CarAge))



set.seed(1000)

# On choisit aléatoirement 1000 observations.
ll <- sample(1:nrow(dat2),1000,replace = F)

## On crée un data.frame pour les features
## C'est le data.frame qu'on donnera en argument à 
## la fonction Predictor. L'algorithme va calculer les graphiques pour 
## ces valeurs exclusivement.
## On n'inclut pas la variable réponse ou l'exposition.

feature_ind <- 4:10
learn <- dat2[ll,feature_ind]

## on crée un vecteur de réponse
YlearnNN_plot <- dat2[ll,2]

## 
response <- as.vector(YlearnNN_plot)
colnames(response) <- "response"

## on crée une fonction sur mesure pour retourner
## les valeurs prédites du modèle.
## Cette fonction prend en argument le data.frame "learn qu'on
## a créer plus haut. On veut donc transformé learn à 
## l'intérieur de cette fonction et retourner 
## la prédiction du réseau.

pred <- function(object, newdata)  {
  ## on fixe l'exposure à 1, pour l'interprétation
  newdata$Exposure <- 1
  newdata$ClaimNb <- 1
  # On transforme Power et Density pour que le réseau les traitent
  newdata$Power <- as.integer(newdata$Power)
  newdata$Density <- exp(newdata$Density)
  ##On recrée la même recette que dans le fichier
  ## Pré-traitement_couche_plongement.R
  rec_obj <-
    recipe(ClaimNb ~ ., 
           data = learnNN) %>% step_rm(PolicyID) %>%
    step_log(Density) %>% 
    step_range(CarAge, DriverAge, Density,Power) %>% # min max
    step_dummy(Gas,
               one_hot = F,
               preserve = F) %>% 
    step_integer(Region,Brand,zero_based = T) %>% 
    prep(training = learnNN)
  
  ## On crée les matrices pour chaque couche d'intrants
  learn_prepped2 <- bake(rec_obj, new_data = newdata) %>% rename(Offset = Exposure)
  X<- as.matrix(learn_prepped2[, c(2,3,4,7,9)])  # design matrix learning sample
  Br <- as.matrix(learn_prepped2$Brand)
  Re <- as.matrix(learn_prepped2$Region)
  ## on crée l'intrant offset à partir des prédictions de modnb2
  ## Il est possible de transformer cette fonction pour qu'elle
  ## puisse traiter un modèle avec couche de plongement sans 
  ## être modèle Cann en décommentant la ligne suivante et en commentant
  ## l'autre ligne après
  ## v <- matrix(0,nrow=nrow(newdata))
  V <- as.matrix(predict(modnb2,newdata = newdata,type="response"))
  results <- as.vector(predict(object, list(X, Br, Re, V)))
  return(results)
}

## Création du Predicteur
components_iml <- Predictor$new(
  model = model_deep3_cann, # changer le nom pour le modèle en question
  data = learn,
  y = response,
  predict.fun = pred
)




## Partial dependance plot

# DriverAge
pdp.driverage_cann <- FeatureEffect$new(components_iml, "DriverAge", method = "pdp",
                                   grid.size = 65)
plot(pdp.driverage_cann)

# Power
pdp.power_cann <- FeatureEffect$new(components_iml, "Power", method = "pdp",
                               grid.size = 12)
plot(pdp.power_cann)

# CarAge
pdp.carage_cann <- FeatureEffect$new(components_iml, "CarAge", method = "pdp",
                                grid.size = 20)
plot(pdp.carage_cann)

# Brand
pdp.brand_cann <- FeatureEffect$new(components_iml, "Brand", method = "pdp",
                               grid.size = 7)
plot(pdp.brand_cann)+coord_flip()

# Gas
pdp.gas_cann <- FeatureEffect$new(components_iml, "Gas", method = "pdp",
                             grid.size = 2)
plot(pdp.gas_cann)

# Region
pdp.region_cann <- FeatureEffect$new(components_iml, "Region", method = "pdp",
                                grid.size = 10)
plot(pdp.region_cann)+coord_flip()

# Density
pdp.density_cann <- FeatureEffect$new(components_iml, "Density", method = "pdp",
                                 grid.size = 50)
plot(pdp.density_cann)

## ICE

# DriverAge
ice.driverage_cann <- FeatureEffect$new(components_iml, "DriverAge", method = "ice",
                                   grid.size = 65)

plot(ice.driverage_cann)


# Power
ice.power_cann <- FeatureEffect$new(components_iml, "Power", method = "ice",
                               grid.size = 12)
plot(ice.power_cann)

# CarAge
ice.carage_cann <- FeatureEffect$new(components_iml, "CarAge", method = "ice",
                                grid.size = 20)
plot(ice.carage_cann)


# Brand
ice.brand_cann <- FeatureEffect$new(components_iml, "Brand", method = "ice",
                               grid.size = 7)
plot(ice.brand_cann)+coord_flip()

# Gas
ice.gas_cann <- FeatureEffect$new(components_iml, "Gas", method = "ice",
                             grid.size = 50)
plot(ice.gas_cann)

# Region
ice.region_cann <- FeatureEffect$new(components_iml, "Region", method = "ice",
                                grid.size = 10)
plot(ice.region_cann)+coord_flip()

# Density
ice.density_cann <- FeatureEffect$new(components_iml, "Density", method = "ice",
                                 grid.size = 50)
plot(ice.density_cann)


## Interaction
interact_cann <- Interaction$new(components_iml)

## Graphiques d'interaction du modèle cann_deep3
plot(interact_cann)+
  scale_y_discrete(name="Variables")+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))

##Gas
interact.gas <- Interaction$new(components_iml,feature = "Gas")
plot(interact.gas)+
  scale_y_discrete(name="Variables")+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))

# Gas-Power pdp
interaction_pdp.gas.power_cann <- FeatureEffect$new(components_iml, 
                                                         feature = c("Gas","Power"),
                                                         method = "pdp", grid.size = c(2,12))
plot(interaction_pdp.gas.power_cann)+
  scale_fill_continuous(name="Réponse prédite")+
  theme_classic()+
  ggtitle("Cann_deep3")

# Gas-Brand pdp
interaction_pdp.gas.brand_cann <- FeatureEffect$new(components_iml, 
                                                    feature = c("Gas","Brand"),
                                                    method = "pdp", grid.size = c(2,7))
plot(interaction_pdp.gas.brand_cann)+
  scale_fill_continuous(name="Réponse \n prédite")+
  scale_y_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))+
  theme_classic()+
  ggtitle("Cann_deep3")


## Brand
interact.brand <- Interaction$new(components_iml,feature = "Brand")

plot(interact.brand)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))

# Brand-DriverAge pdp
interaction_pdp.brand.driver_cann <- FeatureEffect$new(components_iml, 
                                                    feature = c("Brand","DriverAge"),
                                                    method = "pdp", grid.size = c(7,20))
plot(interaction_pdp.brand.driver_cann)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.3))+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))
  

# Crand-CarAge
interaction_pdp.brand.car_cann <- FeatureEffect$new(components_iml, 
                                                       feature = c("Brand","CarAge"),
                                                       method = "pdp", grid.size = c(7,20))

plot(interaction_pdp.brand.car_cann)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.1))+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))


## CarAge
interact.car <- Interaction$new(components_iml,feature = "CarAge")
plot(interact.car)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))



## DriverAge
interact.driverage <- Interaction$new(components_iml,feature = "DriverAge")
plot(interact.driverage)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))

# DriverAge-Density pdp
interaction_pdp.driver.density_cann <- FeatureEffect$new(components_iml, 
                                                    feature = c("DriverAge","Density"),
                                                    method = "pdp", grid.size = c(20,20))



plot(interaction_pdp.driver.density_cann)+
  scale_fill_continuous(name="Réponse \n prédite")+
  theme_classic()+
  ggtitle("Cann_deep3")


# DriverAge-Power
interaction_pdp.driver.power_cann <- FeatureEffect$new(components_iml,
                                                       feature=c("DriverAge","Power"),
                                                       method="pdp",grid.size=c(20,12))
plot(interaction_pdp.driver.power_cann)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.3))+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))


## Density
interact.density_cann <- Interaction$new(components_iml,feature = "Density")
plot(interact.density_cann)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))


# Density-Brand pdp
interaction_pdp.density.brand_cann <- FeatureEffect$new(components_iml,
                                                       feature=c("Density","Brand"),
                                                       method="pdp",grid.size=c(20,7))

plot(interaction_pdp.density.brand_cann)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.12))+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))


# Density-gas pdp
interaction_pdp.density.gas_cann <- FeatureEffect$new(components_iml,
                                                        feature=c("Density","Gas"),
                                                        method="pdp",grid.size=c(20,2))

plot(interaction_pdp.density.gas_cann)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.12))+
  ggtitle("Cann_deep3")+
  theme_classic()+
  theme(title = element_text(size=8))


## Graphiques pour modèle cann_shallow

components_iml_cann_shallow <- Predictor$new(
  model = model_cann_shallow,
  data = learn,
  y = response,
  predict.fun = pred
)




## Partial dependance plot

# DriverAge
pdp.driverage_cann_shallow<- FeatureEffect$new(components_iml_cann_shallow, "DriverAge", method = "pdp",
                                        grid.size = 65)

plot(pdp.driverage_cann_shallow)

# Power
pdp.power_cann_shallow<- FeatureEffect$new(components_iml_cann_shallow, "Power", method = "pdp",
                                    grid.size = 12)
plot(pdp.power_cann_shallow)

# CarAge
pdp.carage_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "CarAge", method = "pdp",
                                     grid.size = 20)
plot(pdp.carage_cann_shallow)

# Brand
pdp.brand_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "Brand", method = "pdp",
                                    grid.size = 7)
plot(pdp.brand_cann_shallow)+coord_flip()

# Gas
pdp.gas_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "Gas", method = "pdp",
                                  grid.size = 2)
plot(pdp.gas_cann_shallow)


# Region
pdp.region_cann_shallow<- FeatureEffect$new(components_iml_cann_shallow, "Region", method = "pdp",
                                     grid.size = 10)
plot(pdp.region_cann_shallow)+coord_flip()

# Density
pdp.density_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "Density", method = "pdp",
                                      grid.size = 50)
plot(pdp.density_cann_shallow)

## ICE

# DriverAge
ice.driverage_cann_shallow<- FeatureEffect$new(components_iml_cann_shallow, "DriverAge", method = "ice",
                                        grid.size = 65)

plot(ice.driverage_cann_shallow)

# Power
ice.power_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "Power", method = "ice",
                                    grid.size = 12)
plot(ice.power_cann_shallow)

# CarAge
ice.carage_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "CarAge", method = "ice",
                                     grid.size = 20)
plot(ice.carage_cann_shallow)

# Brand
ice.brand_cann_shallow<- FeatureEffect$new(components_iml_cann_shallow, "Brand", method = "ice",
                                    grid.size = 7)
plot(ice.brand_cann_shallow)+coord_flip()

# Gas
ice.gas_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "Gas", method = "ice",
                                  grid.size = 2)
plot(ice.gas_cann_shallow)

# Region
ice.region_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "Region", method = "ice",
                                     grid.size = 10)
plot(ice.region_cann_shallow)+coord_flip()

# Density
ice.density_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, "Density", method = "ice",
                                      grid.size = 50)
plot(ice.density_cann_shallow)



##Interactions

interact_cann_shallow <- Interaction$new(components_iml_cann_shallow)
plot(interact_cann_shallow)+
  scale_y_discrete(name="Variables")+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))

## Gas
interact.gas_cann_shallow <- Interaction$new(components_iml_cann_shallow,feature = "Gas")
plot(interact.gas_cann_shallow)+
  scale_y_discrete(name="Variables")+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))

# Gas-Power pdp
interaction_pdp.gas.power_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, 
                                                    feature = c("Gas","Power"),
                                                    method = "pdp", grid.size = c(2,12))
plot(interaction_pdp.gas.power_cann_shallow)+
  scale_fill_continuous(name="Réponse prédite")+
  theme_classic()+
  ggtitle("Cann_shallow")

# Gas-Brand pdp
interaction_pdp.gas.brand_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, 
                                                    feature = c("Gas","Brand"),
                                                    method = "pdp", grid.size = c(2,7))
plot(interaction_pdp.gas.brand_cann_shallow)+
  scale_fill_continuous(name="Réponse \n prédite")+
  scale_y_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))+
  scale_x_discrete(name=NULL)+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=10))

## Brand
interact.brand_cann_shallow <- Interaction$new(components_iml_cann_shallow,feature = "Brand")

plot(interact.brand_cann_shallow)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))

# Brand-DriverAge pdp
interaction_pdp.brand.driver_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, 
                                                       feature = c("Brand","DriverAge"),
                                                       method = "pdp", grid.size = c(7,20))
plot(interaction_pdp.brand.driver_cann_shallow)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.25))+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))


# Brand-Carage pdp
interaction_pdp.brand.car_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, 
                                                    feature = c("Brand","CarAge"),
                                                    method = "pdp", grid.size = c(7,20))
plot(interaction_pdp.brand.car_cann_shallow)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.1))+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))


## CarAge

interact.car_cann_shallow <- Interaction$new(components_iml_cann_shallow,
                                             feature = "CarAge")

plot(interact.car_cann_shallow)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))

## DriverAge

interact.driverage_cann_shallow <- Interaction$new(components_iml_cann_shallow,feature = "DriverAge")

plot(interact.driverage_cann_shallow)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))


# DriverAge-Density pdp
interaction_pdp.driver.density_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow, 
                                                         feature = c("DriverAge","Density"),
                                                         method = "pdp", grid.size = c(20,20))



plot(interaction_pdp.driver.density_cann_shallow)+
  scale_fill_continuous(name="Réponse \n prédite")+
  theme_classic()+
  ggtitle("Cann_shallow")



# DriverAge-Power pdp
interaction_pdp.driver.power_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow,
                                                               feature=c("DriverAge","Power"),
                                                               method="pdp",grid.size=c(20,12))
plot(interaction_pdp.driver.power_cann_shallow)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.35))+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))


##Density

interact.density_cann_shallow <- Interaction$new(components_iml_cann_shallow,feature = "Density")

plot(interact.density_cann_shallow)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))

# Density-Brand pdp
interaction_pdp.density.brand_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow,
                                                        feature=c("Density","Brand"),
                                                        method="pdp",grid.size=c(20,7))

plot(interaction_pdp.density.brand_cann_shallow)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.12))+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))


# Density-gas pdp
interaction_pdp.density.gas_cann_shallow <- FeatureEffect$new(components_iml_cann_shallow,
                                                                feature=c("Density","Gas"),
                                                                method="pdp",grid.size=c(20,7))

plot(interaction_pdp.density.gas_cann_shallow)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.12))+
  ggtitle("Cann_shallow")+
  theme_classic()+
  theme(title = element_text(size=8))
