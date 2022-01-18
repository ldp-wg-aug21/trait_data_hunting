# Script to compare the distribution of biotic variables between the C-LPI and 
# C-vertebrate datasets. This script prepares the datasets, visualizes the
# distributions of variables (creating Figures 2, 3, 4, and 5 in the main text),
# and quantifies the difference between the distributions for each combination of
# vertebrate group and biotic variable.

# Load packages
library(tidyverse)
library(data.table)
library(ggpubr)
library(effectsize)
library(wesanderson)
library(traitdata)

################################################################################
# Data preparation
################################################################################

# Load data for Canadian + C-LPI species
herps<-fread("data-clean/herps_canadian_sp.csv")
fish<-fread("data-clean/fish/fishbase_traits-for-comparison.csv")
birds<-fread("data-clean/birds_traits_allcanadiansp.csv")
mammals<-fread("data-clean/mammals_traits_allcanadiansp.csv")

lpi.dat<-fread("data-raw/CIEE_LPI_dataset.csv")
wild.dat<-fread("data-raw/WildSpecies2015Data.csv")
lpi.dat2<-fread("data-raw/cLPI_data_resolved_species.csv")


## Fish data ----

# Not in Canada but currently in C-LPI (need to eliminate)
fish<-subset(fish, Binomial!="Bathyraja_aleutica" & Binomial!="Bathyraja_minispinosa")
fishlpi<-subset(fish, collection =="C-LPI")
fishwild<-subset(fish, collection == "Canadian Wild Species")
fishlpi<-fishlpi %>% select(c("Binomial","MaxLength_TLonly","LongevityWild",
                        "TrophCategorical","collection")) %>%
  rename(BodySize=MaxLength_TLonly,LifeSpan=LongevityWild,TrophicLevel=TrophCategorical,lpi=collection) %>%
  unique()
fishwild<-fishwild %>% select(c("Binomial","MaxLength_TLonly","LongevityWild",
                              "TrophCategorical","collection")) %>%
  rename(BodySize=MaxLength_TLonly,LifeSpan=LongevityWild,TrophicLevel=TrophCategorical,lpi=collection) %>%
  unique()
fishwild$lpi<- replace(fishwild$lpi, fishwild$lpi=="Canadian Wild Species", "C-vertebrates")
fish<-rbind(fishlpi, fishwild, fill=TRUE)
fishlpi$BodySize.log<-log(fishlpi$BodySize)
fishlpi$LifeSpan.log<-log(fishlpi$LifeSpan)
fishwild$BodySize.log<-log(fishwild$BodySize)
fishwild$LifeSpan.log<-log(fishwild$LifeSpan)

# Birds data ----

birds$lpi<-"Global"
birds$lpi[birds$Binomial%in%wild.dat$Binomial]<-"C-vertebrates"
birds$lpi[birds$Binomial%in%lpi.dat2$Binomial_resolved]<-"C-LPI"
birds<-birds %>% select(c("Binomial","mean_adult_body_mass_g","mean_max_longevity_y",
                          "diet","lpi")) %>%
  rename(BodySize=mean_adult_body_mass_g,
         LifeSpan=mean_max_longevity_y,TrophicLevel=diet) %>%
  filter(lpi!="Global")
temp1<-birds[birds$lpi=="C-LPI",]
temp1$lpi<-"C-vertebrates"
birds<-rbind(birds,temp1); rm(temp1)
#reassign diet categories
birds$TrophicLevel[birds$TrophicLevel%in%
                     c("fruit","nectar","plants","seeds")]<-"herbivore"
birds$TrophicLevel[birds$TrophicLevel%in%
                     c("invertebrates","vertebrates")]<-"carnivore"
birds$TrophicLevel[birds$TrophicLevel=="scav"]<-"carnivore" #turkey vulture
write.csv(birds, "birds.csv")
birds <- read.csv("birds.csv")
birds <- birds[c(2,3,4,5,6)]
birdslpi<-subset(birds, lpi =="C-LPI")
birdswild<-subset(birds, lpi == "C-vertebrates")
birdslpi$BodySize.log<-log(birdslpi$BodySize)
birdslpi$LifeSpan.log<-log(birdslpi$LifeSpan)
birdswild$BodySize.log<-log(birdswild$BodySize)
birdswild$LifeSpan.log<-log(birdswild$LifeSpan)

## Herps data ----

herps$lpi[herps$lpi=="lpi"]<-"C-LPI"
herps$lpi[herps$lpi=="not_lpi"]<-"C-vertebrates"
herps<-herps %>% select(c("Binomial","body_mass_g","longevity_years",
                          "diet","lpi")) %>%
  rename(BodySize=body_mass_g,LifeSpan=longevity_years,TrophicLevel=diet)
temp1<-herps[herps$lpi=="C-LPI",]
temp1$lpi<-"C-vertebrates"
herps<-rbind(herps,temp1); rm(temp1)
herps$TrophicLevel[herps$TrophicLevel=="Herbivore"]<-"herbivore"
herpslpi<-subset(herps, lpi =="C-LPI")
herpswild<-subset(herps, lpi == "C-vertebrates")
herpslpi$BodySize.log<-log(herpslpi$BodySize)
herpslpi$LifeSpan.log<-log(herpslpi$LifeSpan)
herpswild$BodySize.log<-log(herpswild$BodySize)
herpswild$LifeSpan.log<-log(herpswild$LifeSpan)

# Mammals data ----

mammals$lpi<-"Global"
mammals$lpi[mammals$Binomial%in%wild.dat$Binomial]<-"C-vertebrates"
mammals$lpi[mammals$Binomial%in%lpi.dat2$Binomial_resolved]<-"C-LPI"
mammals<-mammals %>% select(c("Binomial","elton_BodyMass.Value_g","Trophic_Level",
                          "amniota_maximum_longevity_y","lpi")) %>%
  rename(Binomial=Binomial,BodySize=elton_BodyMass.Value_g,
         LifeSpan=amniota_maximum_longevity_y,TrophicLevel=Trophic_Level) %>%
  filter(lpi!="Global")
mammals$TrophicLevel[mammals$TrophicLevel=="Herbivores"]<-"herbivore"
mammals$TrophicLevel[mammals$TrophicLevel=="Carnivores"]<-"carnivore"
mammals$TrophicLevel[mammals$TrophicLevel=="Omnivores"]<-"omnivore"
temp2<-mammals[mammals$lpi=="C-LPI",]
temp2$lpi<-"C-vertebrates"
mammals<-rbind(mammals,temp2); rm(temp2)
write.csv(mammals, "mammals.csv")
mammals <- read.csv("mammals.csv")
mammals <- mammals[c(2,3,4,5,6)]
mammalslpi<-subset(mammals, lpi =="C-LPI")
mammalswild<-subset(mammals, lpi == "C-vertebrates")
mammalslpi$BodySize.log<-log(mammalslpi$BodySize)
mammalslpi$LifeSpan.log<-log(mammalslpi$LifeSpan)
mammalswild$BodySize.log<-log(mammalswild$BodySize)
mammalswild$LifeSpan.log<-log(mammalswild$LifeSpan)


## Assembling datasets -----

birdslpi$Group<-"Birds"
fishlpi$Group<-"Fish"
herpslpi$Group<-"Herpetofauna"
mammalslpi$Group<-"Mammals"

#Species in LPI
vertslpi<-rbind(birdslpi,fishlpi,herpslpi,mammalslpi)
vertslpi %>% count(Group)

#Species in Canada
birdswild$Group<-"Birds"
fishwild$Group<-"Fish"
herpswild$Group<-"Herpetofauna"
mammalswild$Group<-"Mammals"

vertswild<-rbind(birdswild,fishwild,herpswild,mammalswild)
vertswild %>% count(Group)

#Species in Canada but not in LPI
vertslpi_sub <- vertslpi[c(1,2,3,4,6,7,8)]
vertswild_sub <- vertswild[c(1,2,3,4,6,7,8)]
vertswildonly<-setdiff(vertswild_sub, vertslpi_sub)
vertswildonly %>% count(Group)
vertswildonly$lpi <- "c wild only"

#Combined LPI and Canadian Verts (include duplicate species, but can filter by "lpi")
birds$Group<-"Birds"
fish$Group<-"Fish"
herps$Group<-"Herpetofauna"
mammals$Group<-"Mammals"
verts<-rbind(birds,fish,herps,mammals)
verts$BodySize.log<-log(verts$BodySize)
verts$LifeSpan.log<-log(verts$LifeSpan)


################################################################################
## Data checks
### Species in the LPI dataset but not the Canadian wild species list (should NOT be a thing)
### Species should only be in the LPI IF they are in Canadaian wild species list
### we need to remedy these species - likely an issue with binomial name (only an issue with fish and mammals)
################################################################################

vertslpionly<-setdiff(vertslpi_sub, vertswild_sub)

### Test of normality

#Canadian Vertebrates
ggqqplot(vertswild$BodySize.log)
shapiro.test(vertswild$BodySize.log)

ggqqplot(vertswild$LifeSpan.log)
shapiro.test(vertswild$LifeSpan.log)

#LPI
ggqqplot(vertslpi$BodySize.log)
shapiro.test(vertslpi$BodySize.log)

ggqqplot(vertslpi$LifeSpan.log)
shapiro.test(vertslpi$LifeSpan.log)

### Trait relationships
A1<-ggplot(verts,aes(x=BodySize.log,y=LifeSpan.log,colour=lpi))+
  geom_smooth(size=2,method="lm")+
  theme_classic()+
  xlab("Body Size (log)")+ylab("Lifespan (log)")+
  theme(legend.title=element_blank())

verts$tr.levels<-paste(verts$TrophicLevel,verts$lpi,sep="_")

trophic.life.summary<-
  data.frame(means=tapply(verts$LifeSpan.log,verts$tr.levels,mean,na.rm=T),
             sds=tapply(verts$LifeSpan.log,verts$tr.levels,sd,na.rm=T))[c(1:4,7,8),]
trophic.life.summary$sd.l<-trophic.life.summary$means-trophic.life.summary$sds
trophic.life.summary$sd.u<-trophic.life.summary$means+trophic.life.summary$sds
trophic.life.summary$TrophicLevel<-t(data.frame(strsplit(rownames(trophic.life.summary),"_")))[,1]
trophic.life.summary$lpi<-t(data.frame(strsplit(rownames(trophic.life.summary),"_")))[,2]

B1<-ggplot(trophic.life.summary,aes(x=TrophicLevel,y=means,colour=lpi,group=lpi))+
  geom_point(size=4,position=position_dodge(width=0.2))+
  geom_errorbar(aes(ymin=sd.l,ymax=sd.u),width=0.2,position=position_dodge(width=0.2))+
  theme_classic()+ylab("Lifespan (log)")+xlab("Trophic Level")+
  theme(legend.title=element_blank())

trophic.body.summary<-
  data.frame(means=tapply(verts$BodySize.log,verts$tr.levels,mean,na.rm=T),
             sds=tapply(verts$BodySize.log,verts$tr.levels,sd,na.rm=T))[c(1:4,7,8),]
trophic.body.summary$sd.l<-trophic.body.summary$means-trophic.body.summary$sds
trophic.body.summary$sd.u<-trophic.body.summary$means+trophic.body.summary$sds
trophic.body.summary$TrophicLevel<-t(data.frame(strsplit(rownames(trophic.body.summary),"_")))[,1]
trophic.body.summary$lpi<-t(data.frame(strsplit(rownames(trophic.body.summary),"_")))[,2]

C1<-ggplot(trophic.body.summary,aes(x=TrophicLevel,y=means,colour=lpi,group=lpi))+
  geom_point(size=4,position=position_dodge(width=0.2))+
  geom_errorbar(aes(ymin=sd.l,ymax=sd.u),width=0.2,position=position_dodge(width=0.2))+
  theme_classic()+ylab("Body Size (log)")+xlab("Trophic Level")+
  theme(legend.title=element_blank())

ggarrange(A1,B1,C1,nrow=1,ncol=3)


### Data coverage for species in the LPI dataset
vertslpi$BS.pa<-if_else(is.na(vertslpi$BodySize.log),0,1)
vertslpi$LS.pa<-if_else(is.na(vertslpi$LifeSpan.log),0,1)
vertslpi$TL.pa<-if_else(is.na(vertslpi$TrophicLevel),0,1)

sum(vertslpi$BS.pa)
sum(vertslpi$LS.pa)
sum(vertslpi$TL.pa)
aggregate(vertslpi$BS.pa, by=list(Group=vertslpi$Group), FUN=sum)
aggregate(vertslpi$LS.pa, by=list(Group=vertslpi$Group), FUN=sum)
aggregate(vertslpi$TL.pa, by=list(Group=vertslpi$Group), FUN=sum)


##Not sure this is needed? What's it really saying?
# v.BS<-vertslpi %>% group_by(Group,BS.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(BS.pa==1) %>% select(c("Group","n","freq"))
# v.LS<-vertslpi %>% group_by(Group,LS.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(LS.pa==1) %>% select(c("Group","n","freq"))
# v.TL<-vertslpi %>% group_by(Group,TL.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(TL.pa==1) %>% select(c("Group","n","freq"))
# v.BS$Trait<-"BodySize"; v.LS$Trait<-"LifeSpan"; v.TL$Trait<-"TrophicLevel"
# v.trait<-rbind(v.BS,v.LS,v.TL)
# ggplot(data=v.trait,aes(x=Trait,y=freq,fill=Group))+
#  geom_col(position="dodge")+theme_classic()+ylab("Frequency")+
#  scale_fill_manual(values=wes_palette("Moonrise2",4))



### Data coverage for Canadian species
vertswild$BS.pa<-if_else(is.na(vertswild$BodySize.log),0,1)
vertswild$LS.pa<-if_else(is.na(vertswild$LifeSpan.log),0,1)
vertswild$TL.pa<-if_else(is.na(vertswild$TrophicLevel),0,1)

sum(vertswild$BS.pa)
sum(vertswild$LS.pa)
sum(vertswild$TL.pa)
aggregate(vertswild$BS.pa, by=list(Group=vertswild$Group), FUN=sum)
aggregate(vertswild$LS.pa, by=list(Group=vertswild$Group), FUN=sum)
aggregate(vertswild$TL.pa, by=list(Group=vertswild$Group), FUN=sum)

##Not sure this is needed? What's it really saying?
# v.BS<-vertswild %>% group_by(Group,BS.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(BS.pa==1) %>% select(c("Group","n","freq"))
# v.LS<-vertswild %>% group_by(Group,LS.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(LS.pa==1) %>% select(c("Group","n","freq"))
# v.TL<-vertswild %>% group_by(Group,TL.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(TL.pa==1) %>% select(c("Group","n","freq"))
# v.BS$Trait<-"BodySize"; v.LS$Trait<-"LifeSpan"; v.TL$Trait<-"TrophicLevel"
# v.trait<-rbind(v.BS,v.LS,v.TL)
# 
# ggplot(data=v.trait,aes(x=Trait,y=freq,fill=Group))+
#  geom_col(position="dodge")+theme_classic()+ylab("Frequency")+
#  scale_fill_manual(values=wes_palette("Moonrise2",4))


### Data coverage for Canadian species not in the LPI
vertswildonly$BS.pa<-if_else(is.na(vertswildonly$BodySize.log),0,1)
vertswildonly$LS.pa<-if_else(is.na(vertswildonly$LifeSpan.log),0,1)
vertswildonly$TL.pa<-if_else(is.na(vertswildonly$TrophicLevel),0,1)

sum(vertswildonly$BS.pa)
sum(vertswildonly$LS.pa)
sum(vertswildonly$TL.pa)
aggregate(vertswildonly$BS.pa, by=list(Group=vertswildonly$Group), FUN=sum)
aggregate(vertswildonly$LS.pa, by=list(Group=vertswildonly$Group), FUN=sum)
aggregate(vertswildonly$TL.pa, by=list(Group=vertswildonly$Group), FUN=sum)

# v.BS<-vertswildonly %>% group_by(Group,BS.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(BS.pa==1) %>% select(c("Group","n","freq"))
# v.LS<-vertswildonly %>% group_by(Group,LS.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(LS.pa==1) %>% select(c("Group","n","freq"))
# v.TL<-vertswildonly %>% group_by(Group,TL.pa) %>%  summarise(n=n()) %>%  mutate(freq=n/sum(n)) %>%
#  subset(TL.pa==1) %>% select(c("Group","n","freq"))
# v.BS$Trait<-"BodySize"; v.LS$Trait<-"LifeSpan"; v.TL$Trait<-"TrophicLevel"
# v.trait<-rbind(v.BS,v.LS,v.TL)
# 
# ggplot(data=v.trait,aes(x=Trait,y=freq,fill=Group))+
#  geom_col(position="dodge")+theme_classic()+ylab("Frequency")+
#  scale_fill_manual(values=wes_palette("Moonrise2",4))


################################################################################
# Visualising trait distributions for comparing C-LPI to C-vertebrates
## Trait comparisons
# Canadian wild (including LPI species) compared to LPI species
################################################################################

#### FIG 1 ####


#### FIGURE 2A ####

#All taxa
A2<-ggplot(verts,aes(x=BodySize.log,colour=lpi))+
  geom_density(size=1)+
  theme_classic()+
  scale_color_manual(values=c("red3", "gray0")) +
  labs(x = "Body Size (log)", y = "Density", col = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
A2
ks.test(vertslpi$BodySize.log, vertswild$BodySize.log)

#### FIGURE 2B ####

#All taxa
B2<-ggplot(verts,aes(x=LifeSpan.log,colour=lpi))+
  geom_density(size=1)+
  theme_classic()+
  scale_color_manual(values=c("red3", "gray0")) +
  labs(x = "Lifespan (log)", y = "Density", col = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
B2
ks.test(vertslpi$LifeSpan.log, vertswild$LifeSpan.log)

#### FIG 2C ####
#All taxa
C2<-ggplot(troph.plot,aes(x=lpi,y=proportion,fill=TrophicLevel))+
  geom_col(position="fill", width = .7)+
  scale_fill_manual(values=c("gray80","gray45","grey0"))+
  labs(x = "Dataset", y = "Proportion", fill = "Trophic level") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
C2
chisq.test(x=verts$TrophicLevel,y=verts$lpi)

#### FIGURE 2: COMBINED ####
ggarrange(A2,B2,C2,nrow=1,ncol=3,labels=c("A","B","C"))
ggsave("figures/fig2_alltaxa.png", width = 11.9, height = 2.76)


#### FIGURE 3 ####

### Body size
#By taxonomic group
ggplot(verts, aes(x=BodySize.log,colour=lpi))+
  geom_density(size=1)+
  facet_wrap(vars(Group),nrow=2)+
  theme_classic()+
  scale_color_manual(values=c("red3", "gray0","red3", "gray0","red3", "gray0","red3", "gray0")) +
  labs(x = "Body Size (log)", y = "Density", col = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/fig3_taxa_bodysize.png", width = 7.29, height = 4.51)

ks.test(birdslpi$BodySize.log, birdswild$BodySize.log)
ks.test(fishlpi$BodySize.log, fishwild$BodySize.log)
ks.test(mammalslpi$BodySize.log, mammalswild$BodySize.log)
ks.test(herpslpi$BodySize.log, herpswild$BodySize.log)


#### FIGURE 4 ####

### Lifespan
#By taxonomic group
ggplot(verts,aes(x=LifeSpan.log,colour=lpi))+
  geom_density(size=1)+
  facet_wrap(vars(Group),nrow=2)+
  theme_classic()+
  scale_color_manual(values=c("red3", "gray0")) +
  labs(x = "Lifespan (log)", y = "Density", col = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/fig4_taxa_lifespan.png", width = 7.29, height = 4.51)

ks.test(birdslpi$LifeSpan.log, birdswild$LifeSpan.log)
ks.test(fishlpi$LifeSpan.log, fishwild$LifeSpan.log)
ks.test(mammalslpi$LifeSpan.log, mammalswild$LifeSpan.log)
ks.test(herpslpi$LifeSpan.log, herpswild$LifeSpan.log)


#### FIGURE 5 ####

### Trophic level
#By taxonomic group
troph.plot<-data.frame(table(verts$TrophicLevel,verts$Group,verts$lpi))
colnames(troph.plot)<-c("TrophicLevel","Group","lpi","proportion")
troph.plot$TrophicLevel<-factor(troph.plot$TrophicLevel,levels=c("herbivore","omnivore","carnivore"))

ggplot(troph.plot,aes(x=lpi,y=proportion,fill=TrophicLevel))+
  geom_col(position="fill")+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_manual(values=c("gray80","gray45","grey0"))+
  labs(x = "Dataset", y = "Proportion", fill = "Trophic level") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/fig5_taxa_trophic.png", width = 7.29, height = 4.51)


chisq.test(x=birds$TrophicLevel,y=birds$lpi)
chisq.test(x=fish$TrophicLevel,y=fish$lpi)
chisq.test(x=mammals$TrophicLevel,y=mammals$lpi)
chisq.test(x=herps$TrophicLevel,y=herps$lpi)



################################################################################
## Trait comparisons - Canadian LPI to not LPI
#Canadian wild (NOT including LPI species) compared to LPI species
################################################################################

verts2<-rbind(vertslpi, vertswildonly)
birdswildonly<-subset(vertswildonly, Group =="Birds")
fishwildonly<-subset(vertswildonly, Group =="Fish")
herpswildonly<-subset(vertswildonly, Group =="Herpetofauna")
mammalswildonly<-subset(vertswildonly, Group =="Mammals")
birdswildonly <- birdswildonly[c(1,2,3,4,5,6,7,8)]
fishwildonly <- fishwildonly[c(1,2,3,4,5,6,7,8)]
herpswildonly <- herpswildonly[c(1,2,3,4,5,6,7,8)]
mammalswildonly <- mammalswildonly[c(1,2,3,4,5,6,7,8)]
birds2<-rbind(birdslpi, birdswildonly)
fish2<-rbind(fishlpi, fishwildonly)
herps2<-rbind(herpslpi, herpswildonly)
mammals2<-rbind(mammalslpi, mammalswildonly)

### Body size
#By taxonomic group
ggplot(verts2,aes(x=BodySize.log,colour=lpi))+
  geom_density(size=2)+
  facet_wrap(vars(Group),nrow=2)+
  theme_classic()+
  xlab("Body Size (log)")+
  theme(legend.title=element_blank())
ks.test(birdslpi$BodySize.log, birdswildonly$BodySize.log)
ks.test(fishlpi$BodySize.log, fishwildonly$BodySize.log)
ks.test(mammalslpi$BodySize.log, mammalswildonly$BodySize.log)
ks.test(herpslpi$BodySize.log, herpswildonly$BodySize.log)

#All taxa
A2<-ggplot(verts2,aes(x=BodySize.log,colour=lpi))+
  geom_density(size=2)+
  theme_classic()+
  xlab("Body Size (log)")+
  theme(legend.title=element_blank())
A2
ks.test(vertslpi$BodySize.log, vertswildonly$BodySize.log)


### Lifespan
#By taxonomic group
ggplot(verts2,aes(x=LifeSpan.log,colour=lpi))+
  geom_density(size=2)+
  facet_wrap(vars(Group),nrow=2)+
  theme_classic()+
  xlab("Lifespan (log)")+
  theme(legend.title=element_blank())
ks.test(birdslpi$LifeSpan.log, birdswildonly$LifeSpan.log)
ks.test(fishlpi$LifeSpan.log, fishwildonly$LifeSpan.log)
ks.test(mammalslpi$LifeSpan.log, mammalswildonly$LifeSpan.log)
ks.test(herpslpi$LifeSpan.log, herpswildonly$LifeSpan.log)

#All taxa
B2<-ggplot(verts2,aes(x=LifeSpan.log,colour=lpi))+
  geom_density(size=2)+
  theme_classic()+
  xlab("Lifespan (log)")+
  theme(legend.title=element_blank())
B2

### Trophic level
#By taxonomic group
troph.plot<-data.frame(table(verts2$TrophicLevel,verts2$Group,verts2$lpi))
colnames(troph.plot)<-c("TrophicLevel","Group","lpi","proportion")
troph.plot$TrophicLevel<-factor(troph.plot$TrophicLevel,levels=c("herbivore","omnivore","carnivore"))
ggplot(troph.plot,aes(x=lpi,y=proportion,fill=TrophicLevel))+
  geom_col(position="fill")+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_manual(values=c("gray80","gray45","grey0"))+
  theme_classic()+
  xlab("Dataset")+
  theme(legend.title=element_blank())
chisq.test(x=birds2$TrophicLevel,y=birds2$lpi)
chisq.test(x=fish2$TrophicLevel,y=fish2$lpi)
chisq.test(x=mammals2$TrophicLevel,y=mammals2$lpi)
chisq.test(x=herps2$TrophicLevel,y=herps2$lpi)

#All taxa
C2<-ggplot(troph.plot,aes(x=lpi,y=proportion,fill=TrophicLevel))+
  geom_col(position="fill")+
  scale_fill_manual(values=c("gray80","gray45","grey0"))+
  xlab("Dataset")+
  theme_classic()+
  theme(legend.title=element_blank())
C2
chisq.test(x=verts2$TrophicLevel,y=verts2$lpi)

### Combined figure
ggarrange(A2,B2,C2,nrow=1,ncol=3,labels=c("A","B","C"))