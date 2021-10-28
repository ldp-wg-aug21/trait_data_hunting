library(tidyverse)
library(ggpubr)

wild.dat<-read.csv("data-clean/herps_canadian_sp.csv")
herps<-read.csv("data-clean/herp_traits_all.csv")

wild.dat[wild.dat==""]<-NA


####Start trait comparison to all canadian species



wild.dat$lpi<-"not_lpi"
wild.dat$lpi[wild.dat$Binomial%in%herps$Binomial]<-"lpi"

#write.csv(wild.dat,"data-clean/herps_canadian_sp.csv")

wild.dat[,c(6:8,10:12)]<-sapply(wild.dat[,c(6:8,10:12)],as.numeric)

ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set2")


ggplot(wild.dat,aes(x=log(body_mass_g),colour=lpi))+
  geom_density(size=2)+
  facet_grid(vars(Order))+
  theme_classic()

ggplot(wild.dat,aes(x=log(longevity_years),colour=lpi))+
  geom_density(size=2)+
  facet_grid(vars(Order))+
  theme_classic()

ggplot(wild.dat,aes(x=log(age_maturity_years),colour=lpi))+
  geom_density(size=2)+
  facet_grid(vars(Order))+
  theme_classic()

ggplot(wild.dat,aes(x=log(clutch_size_n),colour=lpi))+
  geom_density(size=2)+
  facet_grid(vars(Order))+
  theme_classic()

ggplot(wild.dat,aes(x=log(age_maturity_years),y=log(longevity_years),colour=lpi))+
  geom_smooth(size=2,method="lm",fill="grey90")+
  theme_classic()
ggplot(wild.dat,aes(x=log(body_mass_g),y=log(longevity_years),colour=lpi))+
  geom_smooth(size=2,method="lm",fill="grey90")+
  theme_classic()


A<-ggplot(wild.dat,aes(x=log(body_mass_g),colour=lpi))+
  geom_density(size=2)+
  theme_classic()

B<-ggplot(wild.dat,aes(x=log(longevity_years),colour=lpi))+
  geom_density(size=2)+
  theme_classic()

C<-ggplot(wild.dat,aes(x=log(age_maturity_years),colour=lpi))+
  geom_density(size=2)+
  theme_classic()

D<-ggplot(wild.dat,aes(x=log(clutch_size_n),colour=lpi))+
  geom_density(size=2)+
  theme_classic()

ggarrange(A,B,C,D,ncol=2,nrow=2,labels=c("A","B","C","D"))

library(ggmosaic)

ggplot(subset(wild.dat,!is.na(wild.dat$diet)))+
  geom_mosaic(aes(x=product(diet),fill=lpi))

#KS-tests for categorical tests
ks.test(wild.dat$body_mass_g[wild.dat$lpi=="lpi"],
        wild.dat$body_mass_g[wild.dat$lpi=="not_lpi"])
ks.test(wild.dat$longevity_years[wild.dat$lpi=="lpi"],
        wild.dat$longevity_years[wild.dat$lpi=="not_lpi"])
ks.test(wild.dat$age_maturity_years[wild.dat$lpi=="lpi"],
        wild.dat$age_maturity_years[wild.dat$lpi=="not_lpi"])
ks.test(wild.dat$clutch_size_n[wild.dat$lpi=="lpi"],
        wild.dat$clutch_size_n[wild.dat$lpi=="not_lpi"])

#chi-square for categorical test
chisq.test(table(wild.dat$diet,wild.dat$lpi)[2:4,])
