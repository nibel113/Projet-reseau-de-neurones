library(ggplot2)

dat <- data.frame(x=0:10,y=c(4,2,1,0.6,0.5,0.4,0.35,0.3,0.25,0.2,0.1),z=c(4,3,2,1,0.5,0.5,0.6,0.7,0.9,1.5,2.5))

ggplot()+
  geom_smooth(aes(x=x,y=y,color="Entrainement"),data=dat,method = "loess",se = F)+
  geom_smooth(aes(x=x,y=z,color="Validation"),data=dat,se=F)+
  scale_color_manual(c("Entrainement","Validation"),values = c("blue","red"),name="Échantillon")+
  theme_classic()+
  scale_x_discrete("Complexité")+
  scale_y_discrete("Erreur de prédiction")



