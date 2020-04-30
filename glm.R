
source("Pre-traitement_glm.R")

library(MASS)

## déviance de poisson pour tester l'erreur sur les données test

Poisson.Deviance.glm <- function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}

## Modèle qui suit un binomiale négative 
modnb2 <- glm.nb(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas  + I(log(Density))+offset(log(Exposure)),data=learnglm)



learnglm$fit <- fitted(modnb2)
testglm$fit <- predict(modnb2,newdata=testglm,type = "response")
valglm$fit <- predict(modnb2,newdata = valglm,type="response")
in_loss_glm<- Poisson.Deviance.glm(learnglm$fit,learnglm$ClaimNb)
out_loss_glm <- Poisson.Deviance.glm(testglm$fit,testglm$ClaimNb)

