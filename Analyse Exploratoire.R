data("freMTPLfreq")

dat <- freMTPLfreq %>% 
  as_tibble() %>% 
  mutate_at(vars(Power, Gas,Brand,Region), factor) %>% 
  mutate(Exposure = if_else(Exposure > 1, 1, Exposure))

ggplot(dat,aes(x=ClaimNb,y=..density..))+
  geom_histogram()

dat$frequency <- log(dat$ClaimNb/dat$Exposure)

ggplot(dat,aes(x=frequency))+
  geom_histogram()

ggplot(dat,aes(x=Exposure))+
  geom_histogram()

ggplot(dat,aes(x=Power))+
  geom_bar()


ggplot(dat,aes(x=CarAge))+
  geom_histogram(binwidth=1)

ggplot(dat,aes(x=DriverAge))+
  geom_histogram(binwidth = 1)

ggplot(dat,aes(x=Brand))+
  geom_bar()+
  coord_flip()

ggplot(dat,aes(x=Gas))+
  geom_bar()

prop.table(table(dat$Gas))

ggplot(dat,aes(x=Region))+
  geom_bar()+
  coord_flip()

unique(factor(dat$Density))

ggplot(dat,aes(x=Density))+
  geom_density()

## Graphiques multivariés

ggplot(dat,aes(y=Exposure,group=ClaimNb))+
  geom_boxplot()

ggplot(dat,aes(x=Power))+
  geom_bar()+
  facet_wrap(~ClaimNb,scales = "free_y")

ggplot(dat,aes(x=Exposure,y=..density..))+
  geom_histogram(bins=25)+
  facet_wrap(~Power,scales="free_y")

ggplot(dat[which(dat$CarAge<26),],aes(y=CarAge))+
  geom_boxplot()+
  facet_wrap(~ClaimNb)

ggplot(dat,aes(x=CarAge,y=Exposure))+
  geom_point(alpha=0.1)+
  facet_wrap(~ClaimNb)

ggplot(dat,aes(y=DriverAge))+
  geom_boxplot()+
  facet_wrap(~ClaimNb,scales="free_y")

ggplot(dat,aes(x=DriverAge))+
  geom_histogram(alpha=0.5,binwidth = 1)+
  facet_wrap(~factor(ClaimNb),scales="free_y")

ggplot(dat,aes(x=Brand))+
  geom_bar(position = "dodge")+
  facet_wrap(~ClaimNb)

ggplot(dat,aes(x=ClaimNb,fill=Gas))+
  geom_

ggplot(dat,aes())+
  geom_
