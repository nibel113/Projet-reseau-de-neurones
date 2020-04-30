##Fichiier pour graphique d'analyse exploratoire

data("freMTPLfreq")

dat <- freMTPLfreq %>% 
  as_tibble() %>% 
  mutate_at(vars(Power, Gas,Brand,Region), factor) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))

ggplot(dat,aes(x=ClaimNb,y=..prop..))+
  geom_bar(fill="blue")+
  xlab("Nombre de réclamation")+
  ylab("Proportion")+
  theme_classic()


ggplot(dat,aes(x=Exposure))+
  geom_histogram(binwidth = 0.1,fill="blue")+
  xlab("Exposition en année")+
  ylab("Nombre de polices")+
  scale_y_continuous(breaks = c(0,50000,100000),labels=c("0","50000","100000"))+
  theme_classic()+
  theme(axis.text.y.left = element_text(angle=90,hjust=0.5))


dat %>% 
  group_by(Region) %>% 
  summarise(Expo_tot=sum(Exposure)) %>% 
  ggplot(aes(x=Region,y=Expo_tot))+
  geom_col(fill="blue")+
  coord_flip()+
  xlab("Région")+
  ylab("Exposition totale")+
  theme_classic()


dat %>% 
  group_by(Region) %>% 
  summarise(Obs_freq=sum(ClaimNb)/sum(Exposure)) %>% 
  ggplot(aes(x=Region,y=Obs_freq))+
  geom_col(fill="blue")+
  xlab("Région")+
  ylab("Fréquence moyenne observée")+
  theme_classic()+
  coord_flip()

dat %>% 
  group_by(Power) %>% 
  summarise(Expo_tot=sum(Exposure)) %>% 
  ggplot(aes(x=Power,y=Expo_tot))+
  geom_col(fill="blue")+
  xlab("Catégorie de véhicule selon la puissance")+
  ylab("Exposition totale")+
  theme_classic()

dat %>% 
  group_by(Power) %>% 
  summarise(Obs_freq=sum(ClaimNb)/sum(Exposure)) %>% 
  ggplot(aes(x=Power,y=Obs_freq))+
  geom_point(color="blue")+
  xlab("Catégorie de véhicule selon la puissance")+
  ylab("Fréquence moyenne observée")+
  theme_classic()

dat %>% 
  mutate_at(vars(CarAge),funs(pmin(.,20))) %>% 
  group_by(CarAge) %>% 
  summarise(Expo_tot=sum(Exposure)) %>% 
  ggplot(aes(x=CarAge,y=Expo_tot))+
  geom_col(fill="blue")+
  xlab("Âge du véhicule")+
  ylab("Exposition totale")+
  theme_classic()


dat %>% 
  mutate_at(vars(CarAge),funs(pmin(.,20))) %>% 
  group_by(CarAge) %>% 
  summarise(Obs_freq=sum(ClaimNb)/sum(Exposure)) %>% 
  ggplot(aes(x=CarAge,y=Obs_freq))+
  geom_point(color="blue")+
  xlab("Âge du véhicule")+
  ylab("Fréquence moyenne observée")+
  theme_classic()

ggplot(dat,aes(x=DriverAge))+
  geom_histogram(binwidth = 1,fill="blue")+
  xlab("Âge de l'assuré")+
  ylab("Exposition totale")+
  theme_classic()

dat %>% 
  group_by(DriverAge) %>% 
  summarise(Expo_tot=sum(Exposure)) %>% 
  ggplot(aes(x=DriverAge,y=Expo_tot))+
  geom_col(fill="blue")+
  xlab("Âge de l'assuré")+
  ylab("Exposition totale")+
  theme_classic()

dat %>% 
  group_by(DriverAge) %>% 
  summarise(Obs_freq=sum(ClaimNb)/sum(Exposure)) %>% 
  ggplot(aes(x=DriverAge,y=Obs_freq))+
  geom_point(color="blue")+
  xlab("Âge de l'assuré")+
  ylab("Fréquence moyenne observée")+
  theme_classic()

dat %>% 
  group_by(Brand) %>% 
  summarise(Expo_tot=sum(Exposure)) %>% 
  ggplot(aes(x=Brand,y=Expo_tot))+
  geom_col(fill="blue")+
  coord_flip()+
  xlab("Marque du véhicule")+
  ylab("Exposition totale")+
  theme_classic()+
  scale_y_continuous(breaks=c(0,50000,100000),labels=c("0","50000","100000"))
  
  
dat %>% 
  group_by(Brand) %>% 
  summarise(Obs_freq=sum(ClaimNb)/sum(Exposure)) %>% 
  ggplot(aes(x=Brand,y=Obs_freq))+
  geom_col(fill="blue")+
  coord_flip()+
  xlab("Marque du véhicule")+
  ylab("Fréquence moyenne observée")+
  theme_classic()

dat %>% 
  group_by(Gas) %>% 
  summarise(Expo_tot=sum(Exposure)) %>% 
  ggplot(aes(x=Gas,y=Expo_tot))+
  geom_col(fill="blue")+
  xlab("Type de carburant")+
  ylab("Exposition totale")+
  theme_classic()+
  scale_x_discrete(labels=c("Diésel","Régulier"))

dat %>% 
  group_by(Gas) %>% 
  summarise(Obs_freq=sum(ClaimNb)/sum(Exposure)) %>% 
  ggplot(aes(x=Gas,y=Obs_freq))+
  geom_col(fill="blue")+
  xlab("Type de carburant")+
  ylab("Fréquence moyenne observée")+
  theme_classic()+
  scale_x_discrete(labels=c("Diésel","Régulier"))


dat %>% 
  mutate(Density=ntile(log(Density),11)) %>% 
  group_by(Density) %>% 
  summarise(Expo_tot=sum(Exposure)) %>% 
  ggplot(aes(x=Density,y=Expo_tot))+
  geom_col()+
  xlab()+
  ylab()+
  theme_classic()

ggplot(dat,aes(x=log(Density)))+
  geom_histogram(binwidth = 1)

dat %>%  
  mutate(Density=ntile(log(Density),11)) %>% 
  group_by(Density) %>% 
  summarise(Obs_freq=sum(ClaimNb)/sum(Exposure)) %>% 
  ggplot(aes(x=Density,y=Obs_freq))+
  geom_point()+
  xlab()+
  ylab()+
  theme_classic()


tab <- data.frame(Échantillon=c("Entrainement","Test","Validation"),
            "ClaimNb (\\%)" = matrix(round(c(length(learnNN$ClaimNb[which(learnNN$ClaimNb==0)])/length(learnNN$ClaimNb),
                                    length(learnNN$ClaimNb[which(learnNN$ClaimNb==1)])/length(learnNN$ClaimNb),
                                    length(learnNN$ClaimNb[which(learnNN$ClaimNb==2)])/length(learnNN$ClaimNb),
                                    length(learnNN$ClaimNb[which(learnNN$ClaimNb==3)])/length(learnNN$ClaimNb),
                                    length(learnNN$ClaimNb[which(learnNN$ClaimNb==4)])/length(learnNN$ClaimNb),
                                    length(testNN$ClaimNb[which(testNN$ClaimNb==0)])/length(testNN$ClaimNb),
                                    length(testNN$ClaimNb[which(testNN$ClaimNb==1)])/length(testNN$ClaimNb),
                                    length(testNN$ClaimNb[which(testNN$ClaimNb==2)])/length(testNN$ClaimNb),
                                    length(testNN$ClaimNb[which(testNN$ClaimNb==3)])/length(testNN$ClaimNb),
                                    length(testNN$ClaimNb[which(testNN$ClaimNb==4)])/length(testNN$ClaimNb),
                                    length(valNN$ClaimNb[which(valNN$ClaimNb==0)])/length(valNN$ClaimNb),
                                    length(valNN$ClaimNb[which(valNN$ClaimNb==1)])/length(valNN$ClaimNb),
                                    length(valNN$ClaimNb[which(valNN$ClaimNb==2)])/length(valNN$ClaimNb),
                                    length(valNN$ClaimNb[which(valNN$ClaimNb==3)])/length(valNN$ClaimNb),
                                    length(valNN$ClaimNb[which(valNN$ClaimNb==4)])/length(valNN$ClaimNb)
                                    ),6)*100,ncol=5,nrow=3,byrow=T,dimnames = list(c("","",""),c("0","1","2","3","4"))
                                  )
            )

library(kableExtra)

kable(tab,format="latex",booktabs=T,escape=T)%>%
  kable_styling(latex_options=c("HOLD_position"),position = 'center')

