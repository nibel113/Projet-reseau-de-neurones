
dat <- freMTPLfreq

Poisson.Deviance.glm <- function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}

glm1 <- glm(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas + Region + I(log(Density))+ offset(log(Exposure)),data=learnNN,family="poisson")

learnNN$fit <- fitted(glm1)
testNN$fit <- predict(glm1,newdata=testNN,type="response")

names <- "glm1"
in_loss<- Poisson.Deviance.glm(learnNN$fit,learnNN$ClaimNb)
out_loss <- Poisson.Deviance.glm(testNN$fit,testNN$ClaimNb)
