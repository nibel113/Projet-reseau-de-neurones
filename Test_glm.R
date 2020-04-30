source("Pre-traitement_glm.R")

##Déviance de poisson
Poisson.Deviance.glm <- function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}

## on teste un glm poisson avec toutes les variables explicatives
glm1 <- glm(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas + Region + I(log(Density))+offset(log(Exposure)),data=learnglm,family="poisson")

## Utile pour les tableaux du rapport
sum <- summary(glm1)
statWald<- sum$coefficients[,"Pr(>|z|)"]
names(statWald)[5:10] <- paste("Br",1:6,sep=".")
names(statWald)[12:20] <- paste("Re",1:9,sep=".")
drop <- drop1[,c(1,4,5)]



## Test rapport de vraisemblance
anova(glm1)
drop1 <- drop1(glm1,test = "Chisq")

## on elève Region
glm2 <- update(glm1,~.-Region)

## le reste semble adéquat
drop1(glm2,test="Chisq")
summary(glm2)

## il semble y avoir de la sous-dispersion
chi.carre <- sum(((learnglm$ClaimNb - fitted(glm2))^2)/(fitted(glm2)))
1-pchisq(chi.carre,summary(glm2)$df.residual)



library(MASS)

## on tente un glm binomiale négative pour traiter la sous-dispersion

modnb <- glm.nb(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas + Region + I(log(Density))+offset(log(Exposure)),data=learnglm)
summary(modnb)
drop1(modnb,test="Chisq")

## on enlève Region
modnb2 <- update(modnb,~.-Region)
summary(modnb2)

## le modnb2 est le meilleure selon l'AIC
AIC(glm1,glm2,modnb,modnb2)

## le reste semble beau
anova(modnb2)
drop1(modnb2,test="Chisq")


##On choisit le modnb2 comme modèle de base

learnglm$fit <- fitted(modnb2)
testglm$fit <- predict(modnb2,newdata=testglm,type = "response")
valglm$fit <- predict(modnb2,newdata = valglm,type="response")
in_loss_glm<- Poisson.Deviance.glm(learnglm$fit,learnglm$ClaimNb)
out_loss_glm <- Poisson.Deviance.glm(testglm$fit,testglm$ClaimNb)

