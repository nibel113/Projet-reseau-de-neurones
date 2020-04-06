# Helper packages
library(dplyr)      # for data wrangling
library(ggplot2)    # for awesome graphics

# Modeling packages
library(h2o)       # for interfacing with H2O
library(recipes)   # for ML recipes
library(rsample)   # for data splitting
library(xgboost)   # for fitting GBMs

# Model interpretability packages
library(pdp)       # for partial dependence plots (and ICE curves)
library(vip)       # for variable importance plots
library(iml)       # for general IML-related functions
library(DALEX)     # for general IML-related functions
library(lime) 

## on crée le plus petit sous-échantillon possible qui reste représentatif
set.seed(100)
ll <- sample(which(dat$ClaimNb==0), round(0.1675*length(which(dat$ClaimNb==0))), replace = FALSE)
ll <- c(ll,sample(which(dat$ClaimNb==1), round(0.1675*length(which(dat$ClaimNb==1))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==2), round(0.1675*length(which(dat$ClaimNb==2))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==3), round(0.1675*length(which(dat$ClaimNb==3))), replace = FALSE))
ll <- c(ll,sample(which(dat$ClaimNb==4), round(0.1675*length(which(dat$ClaimNb==4))), replace = FALSE))



## On crée un data.frame pour les features
feature_ind <- 4:10
learn <- dat[ll,feature_ind]
## on crée un vecteur de réponse
YlearnNN_plot <- dat[ll,2]

response <- as.vector(YlearnNN_plot)
colnames(response) <- "response"
## on crée une fonction sur mesure pour retourner
## les valeurs pr?dites du modèles

pred <- function(object, newdata)  {
  ## on fixe l'exposure à 1, pour l'interprétation
  features_ind <- c(2:4,6:36)
  newdata$Exposure <- 1
  newdata$ClaimNb <- 1
  learn_prepped <- bake(rec_obj, new_data = newdata) %>% rename(Offset = Exposure)
  X <- as.matrix(learn_prepped[,features_ind])
  results <- as.vector(predict(object, list(X,matrix(1,nrow=nrow(newdata)))))
  return(results)
}
pred(model,learn)
components_iml <- Predictor$new(
  model = model,
  data = learn,
  y = response,
  predict.fun = pred
)

## foncton de perte sur mesure
Poisson.Deviance.iml <- function(actual, predicted){
  2*(sum(predicted)-sum(actual)+sum(log((actual/predicted)^(actual))))/length(predicted)
}


imp <- FeatureImp$new(components_iml, loss = Poisson.Deviance.iml,compare = "difference")
plot(imp)
imp$results

## Partial dependance plot
pdp.driverage <- FeatureEffect$new(components_iml, "DriverAge", method = "pdp",
                  grid.size = 50)

plot(pdp.driverage)

pdp.power <- FeatureEffect$new(components_iml, "Power", method = "pdp",
                               grid.size = 50)
plot(pdp.power)

pdp.carage <- FeatureEffect$new(components_iml, "CarAge", method = "pdp",
                          grid.size = 50)
plot(pdp.carage)

pdp.brand <- FeatureEffect$new(components_iml, "Brand", method = "pdp",
                          grid.size = 50)
plot(pdp.brand)+coord_flip()

pdp.gas <- FeatureEffect$new(components_iml, "Gas", method = "pdp",
                          grid.size = 50)
plot(pdp.gas)

pdp.region <- FeatureEffect$new(components_iml, "Region", method = "pdp",
                          grid.size = 50)
plot(pdp.region)+coord_flip()

pdp.density <- FeatureEffect$new(components_iml, "Density", method = "pdp",
                          grid.size = 50)
plot(pdp.density)


## ICE
ice.driverage <- FeatureEffect$new(components_iml, "DriverAge", method = "ice",
                                   grid.size = 50)

plot(ice.driverage)

ice.power <- FeatureEffect$new(components_iml, "Power", method = "ice",
                               grid.size = 50)
plot(ice.power)

ice.carage <- FeatureEffect$new(components_iml, "CarAge", method = "ice",
                                grid.size = 50)
plot(ice.carage)

ice.brand <- FeatureEffect$new(components_iml, "Brand", method = "ice",
                               grid.size = 50)
plot(ice.brand)+coord_flip()

ice.gas <- FeatureEffect$new(components_iml, "Gas", method = "ice",
                             grid.size = 50)
plot(ice.gas)

ice.region <- FeatureEffect$new(components_iml, "Region", method = "ice",
                                grid.size = 50)
plot(ice.region)+coord_flip()

ice.density <- FeatureEffect$new(components_iml, "Density", method = "ice",
                                 grid.size = 50)
plot(ice.density)


ale <- FeatureEffect$new(components_iml,feature = "DriverAge")
ale$plot()


ale$set.feature("CarAge")
ale$plot()

ale$set.feature("Region")
ale$plot()

ale$set.feature("Power")
ale$plot()

interact <- Interaction$new(components_iml)
plot(interact)

interact.brand <- Interaction$new(components_iml,feature = "Brand")
plot(interact.brand)

interact.driverage <- Interaction$new(components_iml,feature = "DriverAge")
plot(interact.driverage)

interaction_pdp.brand.region <- Partial$new(
  components_iml, 
  c("Brand", "Region"), 
  ice = FALSE, 
  grid.size = 50
) 
plot(interaction_pdp.brand.region)

interaction_pdp.driver.brand <- FeatureEffect$new(components_iml, 
                                                  feature = c("DriverAge","Brand"),
                                                  method = "pdp", grid.size=30)
plot(interaction_pdp.driver.brand)

interaction_pdp.driver.region <- FeatureEffect$new(components_iml,
                                                    feature=c("DriverAge","Region"),
                                                    method="pdp",grid.size=30)
plot(interaction_pdp.driver.region)

inter_pdp.brand.power<- FeatureEffect$new(components_iml,
                                          feature=c("Brand","Power"),
                                          method="pdp",grid.size=30)
plot(inter_pdp.brand.power)


tree <- TreeSurrogate$new(components_iml, maxdepth = 3)
plot(tree)


lime.explain <- LocalModel$new(components_iml, x.interest = learn[1, ])

lime.explain$results

plot(lime.explain)


shapley <- Shapley$new(components_iml, x.interest = learn[1, ])
shapley$plot()

results <- shapley$results
head(results)
