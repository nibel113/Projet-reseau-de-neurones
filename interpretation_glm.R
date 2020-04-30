library(dplyr)     
library(ggplot2)    

library(recipes)   
library(iml)       

# Même principe que le fichier Interpretation_embedding_layers.R
dat2 <- freMTPLfreq %>%
  as_tibble() %>%
  mutate_at(vars(Gas, Brand, Region,Power), factor) %>%
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))%>% 
  mutate(DriverAge= ifelse(DriverAge > 85,85,DriverAge))%>% 
  mutate_at(vars(Density),log) %>% 
  mutate(CarAge = ifelse(CarAge > 20,20,CarAge))


set.seed(1000)

ll <- sample(1:nrow(dat2),1000,replace = F)

## On crée un data.frame pour les features
learnglm <- dat2[ll,]

## on crée un vecteur de réponse
Ylearn_glm <- dat2[ll,2]

response_glm <- as.vector(Ylearn_glm)
colnames(response_glm) <- "response"
## on crée une fonction sur mesure pour retourner
## les valeurs pr?dites du modèles

## Foction de prédiction simple
pred <- function(object, newdata)  {
  ## on fixe l'exposure à 1, pour l'interprétation
  newdata$Exposure <- 1
  # On retransforme Power et Density
  newdata$Power <- as.integer(newdata$Power)
  newdata$Density <- exp(newdata$Density)
  results <- as.vector(predict(modnb2,newdata = newdata,type="response"))
  return(results)
}

## Création du Prédicteur
components_iml_glm <- Predictor$new(
  model = modnb2,
  data = learnglm,
  y = response_glm,
  predict.fun = pred
)




## Partial dependance plot

# DriverAge
pdp.driverage_glm <- FeatureEffect$new(components_iml_glm, "DriverAge", method = "pdp",
                                        grid.size = 65)

plot(pdp.driverage_glm)

# Power
pdp.power_glm <- FeatureEffect$new(components_iml_glm, "Power", method = "pdp",
                                    grid.size = 12)
plot(pdp.power_glm)

# CarAge
pdp.carage_glm <- FeatureEffect$new(components_iml_glm, "CarAge", method = "pdp",
                                     grid.size = 20)
plot(pdp.carage_glm)

# Brand
pdp.brand_glm <- FeatureEffect$new(components_iml_glm, "Brand", method = "pdp",
                                    grid.size = 7)
plot(pdp.brand_glm)+coord_flip()

# Gas
pdp.gas_glm <- FeatureEffect$new(components_iml_glm, "Gas", method = "pdp",
                                  grid.size = 2)
plot(pdp.gas_glm)

# Region
pdp.region_glm <- FeatureEffect$new(components_iml_glm, "Region", method = "pdp",
                                     grid.size = 10)
plot(pdp.region_glm)+coord_flip()

# Density
pdp.density_glm <- FeatureEffect$new(components_iml_glm, "Density", method = "pdp",
                                      grid.size = 50)
plot(pdp.density_glm)

## ICE

# DriverAge
ice.driverage_glm <- FeatureEffect$new(components_iml_glm, 
                                       "DriverAge", 
                                       method = "ice",
                                       grid.size = 65)

plot(ice.driverage_glm)

# Power
ice.power_glm <- FeatureEffect$new(components_iml_glm, "Power", method = "ice",
                                    grid.size = 12)
plot(ice.power_glm)

# CarAge
ice.carage_glm <- FeatureEffect$new(components_iml_glm, "CarAge", method = "ice",
                                     grid.size = 20)
plot(ice.carage_glm)

# Brand
ice.brand_glm <- FeatureEffect$new(components_iml_glm, "Brand", method = "ice",
                                    grid.size = 7)
plot(ice.brand_glm)+coord_flip()

# Gas
ice.gas_glm <- FeatureEffect$new(components_iml_glm, "Gas", method = "ice",
                                  grid.size = 50)
plot(ice.gas_glm)

# Region
ice.region_glm<- FeatureEffect$new(components_iml_glm, "Region", method = "ice",
                                     grid.size = 50)
plot(ice.region_glm)+coord_flip()

# Density
ice.density_glm <- FeatureEffect$new(components_iml_glm, "Density", method = "ice",
                                      grid.size = 40)
plot(ice.density_glm)

##Interaction

interact_glm <- Interaction$new(components_iml_glm)
plot(interact_glm)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))

## Gas
interact.gas_glm<- Interaction$new(components_iml_glm,feature = "Gas")
plot(interact.gas_glm)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))

# Gas-Power pdp
interaction_pdp.gas.power_glm <- FeatureEffect$new(components_iml_glm,
                                                   feature = c("Gas","Power"),
                                                   method = "pdp", 
                                                   grid.size = c(2,12))
plot(interaction_pdp.gas.power_glm)+
  scale_fill_continuous(name="Réponse prédite")+
  theme_classic()+
  ggtitle("Glm")

# Gas-Brand pdp
interaction_pdp.gas.brand_glm <- FeatureEffect$new(components_iml_glm, 
                                                            feature = c("Gas","Brand"),
                                                            method = "pdp", grid.size = c(2,7))
plot(interaction_pdp.gas.brand_glm)+
  scale_fill_continuous(name="Réponse \n prédite")+
  scale_y_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))+
  theme_classic()+
  ggtitle("Glm")

## Brand
interact.brand_glm <- Interaction$new(components_iml_glm,feature = "Brand")
plot(interact.brand_glm)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))

# Brand-DriverAge pdp
interaction_pdp.brand.driver_glm <- FeatureEffect$new(components_iml_glm, 
                                                               feature = c("Brand","DriverAge"),
                                                               method = "pdp", grid.size = c(7,20))
plot(interaction_pdp.brand.driver_glm)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.035,0.11))+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))

# Brand-CarAge pdp
interaction_pdp.brand.car_glm <- FeatureEffect$new(components_iml_glm,
                                                   feature = c("Brand","CarAge"),
                                                   method = "pdp", 
                                                   grid.size = c(7,20))
plot(interaction_pdp.brand.car_glm)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.05,0.095))+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))


## CarAge
interact.car_glm <- Interaction$new(components_iml_glm,feature = "CarAge")
plot(interact.car_glm)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))


## DriverAge

interact.driverage_glm <- Interaction$new(components_iml_glm,feature = "DriverAge")
plot(interact.driverage_glm)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))


# DriverAge-Density pdp
interaction_pdp.driver.density_glm <- FeatureEffect$new(components_iml_glm,
                                                        feature = c("DriverAge","Density"),
                                                        method = "pdp", 
                                                        grid.size = c(20,20))


plot(interaction_pdp.driver.density_glm)+
  scale_fill_continuous(name="Réponse \n prédite")+
  theme_classic()+
  ggtitle("Glm")




##Density

interact.density_glm<- Interaction$new(components_iml_glm,feature = "Density")
plot(interact.density_glm)+
  scale_y_discrete(name=NULL)+
  scale_x_continuous(name="Interaction")+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))


# Density-Brand pdp
interaction_pdp.density.brand_glm <- FeatureEffect$new(components_iml_glm,
                                                       feature=c("Density","Brand"),
                                                       method="pdp",
                                                       grid.size=c(20,7))

plot(interaction_pdp.density.brand_glm)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.03,0.13))+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))+
  scale_color_discrete(labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))


# Density-Gas pdp
interaction_pdp.density.gas_glm <- FeatureEffect$new(components_iml_glm,
                                                       feature=c("Density","Gas"),
                                                       method="pdp",grid.size=c(20,7))

plot(interaction_pdp.density.gas_glm)+
  scale_y_continuous(name="Réponse prédite",limits=c(0.03,0.12))+
  ggtitle("Glm")+
  theme_classic()+
  theme(title = element_text(size=8))

