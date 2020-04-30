library(dplyr)      # for data wrangling
library(ggplot2)



#Pdp univarié des trois modèles

##DriverAge

pdp.driverage_cann$predictor$data$X$DriverAge
pdp.driverage_cann_shallow$results
pdp.driverage_glm$results

plot.drv.cann <- plot(pdp.driverage_cann)


ggplot(data=pdp.driverage_cann$results,aes(x=DriverAge,y=.y.hat))+
  geom_line(mapping=aes(color="Cann_deep3"))+
  geom_line(mapping=aes(color="Cann_shallow"),data=pdp.driverage_cann_shallow$results)+
  geom_line(mapping=aes(color="Glm"),dat=pdp.driverage_glm$results)+
  geom_rug(alpha=0.2,data=plot.drv.cann$layers[[2]][["data"]],position="jitter",size=0.5,length = unit(0.03,"npc"),outside = F,na.rm = F,sides="b")+
  scale_y_continuous(limits=c(0,0.25),name="Réponse prédite")+
  labs(color="Modèle")+
  theme_classic()
  

##ice
plot.drv.ice.cann <- plot(ice.driverage_cann)+scale_y_continuous(limits=c(0,0.6))+ggtitle("Cann_deep3")+ylab(NULL)+theme(title = element_text(size=8))+theme_classic()
plot.drv.ice.shallow <- plot(ice.driverage_cann_shallow)+scale_y_continuous(limits=c(0,0.6))+ggtitle("Cann_shallow")+ylab("Réponse prédite")+theme(title = element_text(size=8))+theme_classic()
plot.drv.ice.glm <- plot(ice.driverage_glm)+scale_y_continuous(limits=c(0,0.2))+ggtitle("Glm")+ylab(NULL)+theme(title = element_text(size=8))+theme_classic()

plot_grid(plot.drv.ice.shallow,plot.drv.ice.cann,plot.drv.ice.glm,ncol=3)


##Power

plot.power.cann <- plot(pdp.power_cann)+ggtitle("Cann_deep3")+scale_y_continuous(limits=c(0,0.11))+ylab(NULL)+theme(title = element_text(size=8))+theme_classic()
plot.power.shallow <- plot(pdp.power_cann_shallow)+scale_y_continuous(limits=c(0,0.11))+ggtitle("Cann_shallow")+ylab("Réponse prédite")+theme(title = element_text(size=8))+theme_classic()
plot.power.glm <- plot(pdp.power_glm)+ggtitle("Glm")+scale_y_continuous(limits=c(0,0.1))+ylab("Réponse prédite")+theme(title = element_text(size=8))+theme_classic()

plot_grid(plot.power.shallow,plot.power.cann,plot.power.glm)


##ice

plot.ice.power.cann <- plot(ice.power_cann)+ggtitle("Cann_deep3")+scale_y_continuous(name=NULL)+theme(title = element_text(size=8))+theme_classic()
plot.ice.power.shallow <- plot(ice.power_cann_shallow)+ggtitle("Cann_shallow")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()
plot.ice.power.glm <- plot(ice.power_glm)+ggtitle("Glm")+scale_y_continuous(name=NULL)+theme(title = element_text(size=8))+theme_classic()

plot_grid(plot.ice.power.shallow,plot.ice.power.cann,plot.ice.power.glm)


##CarAge

plot.car.cann <- plot(pdp.carage_cann)


ggplot(data=pdp.carage_cann$results,aes(x=CarAge,y=.y.hat))+
  geom_line(mapping=aes(color="Cann_deep3"))+
  geom_line(mapping=aes(color="Cann_shallow"),data=pdp.carage_cann_shallow$results)+
  geom_line(mapping=aes(color="Glm"),dat=pdp.carage_glm$results)+
  geom_rug(alpha=0.2,data=plot.car.cann$layers[[2]][["data"]],position="jitter",size=0.5,length = unit(0.03,"npc"),outside = F,na.rm = F,sides="b")+
  scale_y_continuous(limits=c(0.06,0.085),name="Réponse prédite")+
  labs(color="Modèle")+
  theme_classic()

##ice

plot.car.ice.cann <- plot(ice.carage_cann)+scale_y_continuous(limits=c(0,0.4))+ggtitle("Cann_deep3")+ylab(NULL)+theme(title = element_text(size=8))+theme_classic()
plot.car.ice.shallow <- plot(ice.carage_cann_shallow)+scale_y_continuous(limits=c(0,0.4))+ggtitle("Cann_shallow")+ylab("Réponse prédite")+theme(title = element_text(size=8))+theme_classic()
plot.car.ice.glm <- plot(ice.carage_glm)+scale_y_continuous(limits=c(0,0.2))+ggtitle("Glm")+ylab(NULL)+theme(title = element_text(size=8))+theme_classic()

plot_grid(plot.car.ice.shallow,plot.car.ice.cann,plot.car.ice.glm,ncol=2)


##Brand


plot.brand.cann <- plot(pdp.brand_cann)+ggtitle("Cann_deep3")+scale_y_continuous(name = "Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(label=NULL,name=NULL)
plot.brand.shallow <- plot(pdp.brand_cann_shallow)+ggtitle("Cann_shallow")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(name=NULL,labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))
plot.brand.glm <- plot(pdp.brand_glm)+ggtitle("Glm")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(name=NULL,labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))

plot_grid(plot.power.shallow,plot.power.cann,plot.power.glm)


##ice

plot.ice.brand.cann <- plot(ice.brand_cann)+ggtitle("Cann_deep3")+scale_y_continuous(name = "Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(label=NULL,name=NULL)
plot.ice.brand.shallow <- plot(ice.brand_cann_shallow)+ggtitle("Cann_shallow")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(name=NULL,labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))
plot.ice.brand.glm <- plot(ice.brand_glm)+ggtitle("Glm")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(name=NULL,labels=c("Fiat","Japanese","Mercedes","Opel","Other","Renault","Volkswagen"))

##Gas


plot.gas.cann <- plot(pdp.gas_cann)+ggtitle("Cann_deep3")+scale_y_continuous(name = NULL)+theme(title = element_text(size=8))+theme_classic()
plot.gas.shallow <- plot(pdp.gas_cann_shallow)+ggtitle("Cann_shallow")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()
plot.gas.glm <- plot(pdp.gas_glm)+ggtitle("Glm")+scale_y_continuous(name = "Réponse prédite")+theme(title = element_text(size=8))+theme_classic()

plot_grid(plot.power.shallow,plot.power.cann,plot.power.glm)


##ice

plot.ice.gas.cann <- plot(ice.gas_cann)+ggtitle("Cann_deep3")+scale_y_continuous(name = NULL)+theme(title = element_text(size=8))+theme_classic()
plot.ice.gas.shallow <- plot(ice.gas_cann_shallow)+ggtitle("Cann_shallow")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()
plot.ice.gas.glm <- plot(ice.gas_glm)+ggtitle("Glm")+scale_y_continuous(name = "Réponse prédite")+theme(title = element_text(size=8))+theme_classic()


##Region

plot.region.cann <- plot(pdp.region_cann)+ggtitle("Cann_deep3")+scale_y_continuous(name = "Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(label=NULL,name=NULL)
plot.region.shallow <- plot(pdp.region_cann_shallow)+ggtitle("Cann_shallow")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(name=NULL)


##ice

plot.ice.region.cann <- plot(ice.region_cann)+ggtitle("Cann_deep3")+scale_y_continuous(name = "Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(label=NULL,name=NULL)
plot.ice.region.shallow <- plot(ice.region_cann_shallow)+ggtitle("Cann_shallow")+scale_y_continuous(name="Réponse prédite")+theme(title = element_text(size=8))+theme_classic()+coord_flip()+scale_x_discrete(name=NULL)


##Density


plot.density.cann <- plot(pdp.density_cann)


ggplot(data=pdp.density_cann$results,aes(x=Density,y=.y.hat))+
  geom_line(mapping=aes(color="Cann_deep3"))+
  geom_line(mapping=aes(color="Cann_shallow"),data=pdp.density_cann_shallow$results)+
  geom_line(mapping=aes(color="Glm"),dat=pdp.density_glm$results)+
  geom_rug(alpha=0.2,data=plot.density.cann$layers[[2]][["data"]],position="jitter",size=0.5,length = unit(0.03,"npc"),outside = F,na.rm = F,sides="b")+
  scale_y_continuous(limits=c(0.04,0.13),name="Réponse prédite")+
  labs(color="Modèle")+
  theme_classic()

##ice

plot.density.ice.cann <- plot(ice.density_cann)+scale_y_continuous(limits=c(0,0.45))+ggtitle("Cann_deep3")+ylab(NULL)+theme(title = element_text(size=8))+theme_classic()
plot.density.ice.shallow <- plot(ice.density_cann_shallow)+scale_y_continuous(limits=c(0,0.45))+ggtitle("Cann_shallow")+ylab("Réponse prédite")+theme(title = element_text(size=8))+theme_classic()
plot.density.ice.glm <- plot(ice.density_glm)+scale_y_continuous(limits=c(0,0.2))+ggtitle("Glm")+ylab("Réponse prédite")+theme(title = element_text(size=8))+theme_classic()
