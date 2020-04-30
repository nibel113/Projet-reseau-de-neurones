
source("Pre-traitement_glm.R")
Poisson.Deviance.glm <- function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}

glm1 <- glm(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas + Region + I(log(Density))+offset(log(Exposure)),data=learnglm,family="poisson")

sum <- summary(glm1)
statWald<- sum$coefficients[,"Pr(>|z|)"]
names(statWald)[5:10] <- paste("Br",1:6,sep=".")
names(statWald)[12:20] <- paste("Re",1:9,sep=".")
anova(glm1)



drop1(glm1,test = "Chisq")
glm2 <- update(glm1,~.-Region)
drop1(glm2,test="Chisq")
summary(glm2)

chi.carre <- sum(((learnglm$ClaimNb - fitted(glm2))^2)/(fitted(glm2)))
1-pchisq(chi.carre,summary(glm2)$df.residual)



library(MASS)

modnb <- glm.nb(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas + Region + I(log(Density))+offset(log(Exposure)),data=learnglm)
summary(modnb)
drop1(modnb,test="Chisq")
modnb2 <- update(modnb,~.-Region)
summary(modnb2)

AIC(glm1,glm2,modnb,modnb2)

anova(modnb2)
drop1(modnb2,test="Chisq")


1/summary(modnb2)$theta

twoll.nb <- summary(modnb2)$twologlik
twoll.pois <- 2*c(logLik(glm2))
stat.trv <- twoll.nb-twoll.pois

0.5*(1-pchisq(stat.trv,8))

library(pscl)

zeroinf <- zeroinfl(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas + Region + I(log(Density))+offset(log(Exposure))|1,data=learnglm)
summary(zeroinf)

zeroinf2 <- zeroinfl(ClaimNb~ Power + CarAge + DriverAge + Brand  + Region + I(log(Density))+offset(log(Exposure))|1,data=learnglm)
summary(zeroinf2)
statobs <- 2*(zeroinf$loglik- zeroinf2$loglik )

1-pchisq(statobs,1)

zeroinf3 <- zeroinfl(ClaimNb~ Power + CarAge + DriverAge + Brand + Gas  + I(log(Density))+offset(log(Exposure))|1,data=learnglm)
summary(zeroinf3)

statobs <- 2*(zeroinf$loglik- zeroinf3$loglik )

1-pchisq(statobs,1)

glm2$aic
AIC(zeroinf,glm2,modnb2)

learnglm$fit <- fitted(modnb2)
testglm$fit <- predict(modnb2,newdata=testglm,type = "response")
valglm$fit <- predict(modnb2,newdata = valglm,type="response")
in_loss_glm<- Poisson.Deviance.glm(learnglm$fit,learnglm$ClaimNb)
out_loss_glm <- Poisson.Deviance.glm(testglm$fit,testglm$ClaimNb)

