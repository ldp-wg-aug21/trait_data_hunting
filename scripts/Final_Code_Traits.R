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
library(traitdata)
library(patchwork)

################################################################################
# Data preparation
################################################################################

# Load data for Canadian + C-LPI species
herps<-fread("data-clean/herps_traits_allcanadiansp.csv")
fish<-fread("data-clean/fish_traits_CLPI_canadiansp_global.csv")
birds<-fread("data-clean/birds_traits_globalLPI.csv")
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
fishwild$lpi<- replace(fishwild$lpi, fishwild$lpi=="Canadian Wild Species", "C-Vertebrates")
fish<-rbind(fishlpi, fishwild, fill=TRUE)
fishlpi$BodySize.log<-log(fishlpi$BodySize)
fishlpi$LifeSpan.log<-log(fishlpi$LifeSpan)
fishwild$BodySize.log<-log(fishwild$BodySize)
fishwild$LifeSpan.log<-log(fishwild$LifeSpan)
fishlpi$countNA <-apply(fishlpi, MARGIN = 1, function(x) sum(is.na(x)))
fishlpi <- fishlpi[ !(fishlpi$countNA %in% c("5")), ]
fishwild$countNA <-apply(fishwild, MARGIN = 1, function(x) sum(is.na(x)))
fishwild <- fishwild[ !(fishwild$countNA %in% c("5")), ]


# Birds data ----

birds$lpi<-"Global"
birds$lpi[birds$binomial%in%wild.dat$Binomial]<-"C-Vertebrates"
birds$lpi[birds$binomial%in%lpi.dat2$Binomial_resolved]<-"C-LPI"
birds<-birds %>% select(c("binomial","mean_adult_body_mass_g","mean_max_longevity_y",
                          "diet","lpi")) %>%
  rename(Binomial=binomial,BodySize=mean_adult_body_mass_g,
         LifeSpan=mean_max_longevity_y,TrophicLevel=diet) %>%
  filter(lpi!="Global")
temp1<-birds[birds$lpi=="C-LPI",]
temp1$lpi<-"C-Vertebrates"
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
birdswild<-subset(birds, lpi == "C-Vertebrates")
birdslpi$BodySize.log<-log(birdslpi$BodySize)
birdslpi$LifeSpan.log<-log(birdslpi$LifeSpan)
birdswild$BodySize.log<-log(birdswild$BodySize)
birdswild$LifeSpan.log<-log(birdswild$LifeSpan)
birdslpi$countNA <-apply(birdslpi, MARGIN = 1, function(x) sum(is.na(x)))
birdslpi <- birdslpi[ !(birdslpi$countNA %in% c("5")), ]
birdswild$countNA <-apply(birdswild, MARGIN = 1, function(x) sum(is.na(x)))
birdswild <- birdswild[ !(birdswild$countNA %in% c("5")), ]

## Herps data ----

# start a blank column
herps$lpi<-NA
# select columns with traits of interest
herps <-herps %>% select(c("Binomial","body_mass_g","longevity_years",
                          "diet","lpi")) %>%
  rename(BodySize=body_mass_g,LifeSpan=longevity_years,TrophicLevel=diet)
# correct some data cells
herps$TrophicLevel[herps$TrophicLevel=="Herbivore"]<-"herbivore"
herps$TrophicLevel[herps$TrophicLevel == ""] <- NA
# log-transform quantitative traits
herps$BodySize.log<-log(herps$BodySize)
herps$LifeSpan.log<-log(herps$LifeSpan)

# label all species as Canadian Wild Species
herps$lpi[herps$Binomial%in%wild.dat$Binomial]<-"C-Vertebrates"
# label the subset which are found in the C-LPI
herps$lpi[herps$Binomial%in%lpi.dat2$Binomial_resolved]<-"C-LPI"
# duplicate this subset C-LPI species
temp2<-herps[herps$lpi=="C-LPI",]
temp2$lpi<-"C-Vertebrates" # assign them back to Wild Species, with their original label
# bind together to have 1 row per species in C-LPI, and one row per species in C-vertebrates
herps<-rbind(herps,temp2); rm(temp2)

write.csv(herps, "herps.csv", row.names = FALSE)
herps <- read.csv("herps.csv")
herpslpi<-subset(herps, lpi =="C-LPI")
herpswild<-subset(herps, lpi == "C-Vertebrates")

herpslpi$countNA <-apply(herpslpi, MARGIN = 1, function(x) sum(is.na(x)))
herpslpi <- herpslpi[ !(herpslpi$countNA %in% c("5")), ]
herpswild$countNA <-apply(herpswild, MARGIN = 1, function(x) sum(is.na(x)))
herpswild <- herpswild[ !(herpswild$countNA %in% c("5")), ]

# Mammals data ----

mammals$lpi<-"Global"
mammals$lpi[mammals$Binomial%in%wild.dat$Binomial]<-"C-Vertebrates"
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
temp2$lpi<-"C-wild"
mammals<-rbind(mammals,temp2); rm(temp2)
write.csv(mammals, "mammals.csv")
mammals <- read.csv("mammals.csv")
mammals <- mammals[c(2,3,4,5,6)]
mammalslpi<-subset(mammals, lpi =="C-LPI")
mammalswild<-subset(mammals, lpi == "C-Vertebrates")
mammalslpi$BodySize.log<-log(mammalslpi$BodySize)
mammalslpi$LifeSpan.log<-log(mammalslpi$LifeSpan)
mammalswild$BodySize.log<-log(mammalswild$BodySize)
mammalswild$LifeSpan.log<-log(mammalswild$LifeSpan)
mammalslpi$countNA <-apply(mammalslpi, MARGIN = 1, function(x) sum(is.na(x)))
mammalslpi <- mammalslpi[ !(mammalslpi$countNA %in% c("5")), ]
mammalswild$countNA <-apply(mammalswild, MARGIN = 1, function(x) sum(is.na(x)))
mammalswild <- mammalswild[ !(mammalswild$countNA %in% c("5")), ]


## Assembling datasets -----

birdslpi$Group<-"Birds"
fishlpi$Group<-"Fish"
herpslpi$Group<-"Herpetofauna"
mammalslpi$Group<-"Mammals"

#Species in LPI
vertslpi<-rbind(birdslpi,fishlpi,herpslpi,mammalslpi)

#Species in Canada
birdswild$Group<-"Birds"
fishwild$Group<-"Fish"
herpswild$Group<-"Herpetofauna"
mammalswild$Group<-"Mammals"

vertswild<-rbind(birdswild,fishwild,herpswild,mammalswild)

#Species in Canada but not in LPI
vertslpi_sub <- vertslpi[c(1,2,3,4,6,7,9)]
vertswild_sub <- vertswild[c(1,2,3,4,6,7,9)]
vertswildonly<-setdiff(vertswild_sub, vertslpi_sub)
vertswildonly %>% count(Group)
vertswildonly$lpi <- "C-Vertebrates Only"

#combine into 1 dataset that can be filtered by "lpi"
verts<-rbind(vertslpi, vertswild)
verts <- verts[c(1,2,3,4,5,6,7,9)]

################################################################################
## Data checks
### Species in the LPI dataset but not the Canadian wild species list (should NOT be a thing)
### Species should only be in the LPI IF they are in Canadaian wild species list
################################################################################

vertslpionly<-setdiff(vertslpi_sub, vertswild_sub)

#### Test of normality
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

#Wildonly
ggqqplot(vertslpi$BodySize.log)
shapiro.test(vertswildonly$BodySize.log)

ggqqplot(vertslpi$LifeSpan.log)
shapiro.test(vertswildonly$LifeSpan.log)




#### Trait relationships
A1<-ggplot(verts,aes(x=BodySize.log,y=LifeSpan.log,colour=lpi))+
  geom_smooth(size=1,method="lm")+
  theme_classic()+
  scale_color_manual(values=c("red3", "gray0"))+
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
  geom_point(size=4,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=sd.l,ymax=sd.u),width=0.2,position=position_dodge(width=0.4))+
  theme_classic()+scale_color_manual(values=c("red3", "gray0"))+ylab("Lifespan (log)")+xlab("Trophic Level")+
  theme(legend.title=element_blank())

trophic.body.summary<-
  data.frame(means=tapply(verts$BodySize.log,verts$tr.levels,mean,na.rm=T),
             sds=tapply(verts$BodySize.log,verts$tr.levels,sd,na.rm=T))[c(1:4,7,8),]
trophic.body.summary$sd.l<-trophic.body.summary$means-trophic.body.summary$sds
trophic.body.summary$sd.u<-trophic.body.summary$means+trophic.body.summary$sds
trophic.body.summary$TrophicLevel<-t(data.frame(strsplit(rownames(trophic.body.summary),"_")))[,1]
trophic.body.summary$lpi<-t(data.frame(strsplit(rownames(trophic.body.summary),"_")))[,2]

C1<-ggplot(trophic.body.summary,aes(x=TrophicLevel,y=means,colour=lpi,group=lpi))+
  geom_point(size=4,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=sd.l,ymax=sd.u),width=0.2,position=position_dodge(width=0.4))+
  theme_classic()+scale_color_manual(values=c("red3", "gray0"))+ylab("Body Size (log)")+xlab("Trophic Level")+
  theme(legend.title=element_blank())

ggarrange(A1,B1,C1,nrow=1,ncol=3,labels=c("A","B","C"))
ggsave("figures/figS2_compare_traits.png", width = 11.9, height = 2.76)


#### Data coverage for species in the LPI dataset
vertslpi$BS.pa<-if_else(is.na(vertslpi$BodySize.log),0,1)
vertslpi$LS.pa<-if_else(is.na(vertslpi$LifeSpan.log),0,1)
vertslpi$TL.pa<-if_else(is.na(vertslpi$TrophicLevel),0,1)

sum(vertslpi$BS.pa)
sum(vertslpi$LS.pa)
sum(vertslpi$TL.pa)
aggregate(vertslpi$BS.pa, by=list(Group=vertslpi$Group), FUN=sum)
aggregate(vertslpi$LS.pa, by=list(Group=vertslpi$Group), FUN=sum)
aggregate(vertslpi$TL.pa, by=list(Group=vertslpi$Group), FUN=sum)




#### Data coverage for Canadian species
vertswild$BS.pa<-if_else(is.na(vertswild$BodySize.log),0,1)
vertswild$LS.pa<-if_else(is.na(vertswild$LifeSpan.log),0,1)
vertswild$TL.pa<-if_else(is.na(vertswild$TrophicLevel),0,1)

sum(vertswild$BS.pa)
sum(vertswild$LS.pa)
sum(vertswild$TL.pa)
aggregate(vertswild$BS.pa, by=list(Group=vertswild$Group), FUN=sum)
aggregate(vertswild$LS.pa, by=list(Group=vertswild$Group), FUN=sum)
aggregate(vertswild$TL.pa, by=list(Group=vertswild$Group), FUN=sum)


#### Data coverage for Canadian species not in the LPI
vertswildonly$BS.pa<-if_else(is.na(vertswildonly$BodySize.log),0,1)
vertswildonly$LS.pa<-if_else(is.na(vertswildonly$LifeSpan.log),0,1)
vertswildonly$TL.pa<-if_else(is.na(vertswildonly$TrophicLevel),0,1)

sum(vertswildonly$BS.pa)
sum(vertswildonly$LS.pa)
sum(vertswildonly$TL.pa)
aggregate(vertswildonly$BS.pa, by=list(Group=vertswildonly$Group), FUN=sum)
aggregate(vertswildonly$LS.pa, by=list(Group=vertswildonly$Group), FUN=sum)
aggregate(vertswildonly$TL.pa, by=list(Group=vertswildonly$Group), FUN=sum)


################################################################################
# Visualizing trait distributions for comparing C-LPI to C-vertebrates
## Trait comparisons
# Canadian wild (including LPI species) compared to C-LPI species
################################################################################

#### FIGURE 2A ####

# All taxa
A2<-ggplot(verts,aes(x=BodySize.log,colour=lpi))+
  geom_density(size=1)+
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
troph.plot<-data.frame(table(verts$TrophicLevel,verts$Group,verts$lpi))
colnames(troph.plot)<-c("TrophicLevel","Group","lpi","proportion")
troph.plot$TrophicLevel <- as.character(troph.plot$TrophicLevel)
troph.plot$TrophicLevel[troph.plot$TrophicLevel == ""] <- "no data"
troph.plot$TrophicLevel<-factor(troph.plot$TrophicLevel,
                                levels=c("no data", "herbivore","omnivore","carnivore"))

C2<-ggplot(troph.plot,
           aes(x=lpi,y=proportion,fill=TrophicLevel))+
  geom_col(position="fill", width = .7)+
  scale_fill_grey(start = .9, end = .1) +
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

ggplot(troph.plot,aes(x=lpi,y=proportion,fill=TrophicLevel))+
  geom_col(position="fill")+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_grey(start = .9, end = .1) +
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
# herps$TrophicLevel[herps$TrophicLevel == ""] <- NA
# herps <- na.omit(herps)
chisq.test(x=herps$TrophicLevel,y=herps$lpi)

################################################################################
#### Trait comparisons - Canadian LPI to not LPI
# Canadian wild (NOT including LPI species) compared to LPI species
################################################################################

temp<-vertslpi[c(1,2,3,4,5,6,7,9)]
temp2<-vertswildonly[c(1,2,3,4,5,6,7,8)]
verts2<-rbind(temp, temp2)

birdswildonly<-subset(vertswildonly, Group =="Birds")
fishwildonly<-subset(vertswildonly, Group =="Fish")
herpswildonly<-subset(vertswildonly, Group =="Herpetofauna")
mammalswildonly<-subset(vertswildonly, Group =="Mammals")
birdswildonly <- birdswildonly[c(1,2,3,4,5,6,7,8)]
fishwildonly <- fishwildonly[c(1,2,3,4,5,6,7,8)]
herpswildonly <- herpswildonly[c(1,2,3,4,5,6,7,8)]
mammalswildonly <- mammalswildonly[c(1,2,3,4,5,6,7,8)]

write.csv(birdslpi, "birdslpi.csv")
birdslpi <- read.csv("birdslpi.csv")
birdslpi <- birdslpi[c(2,3,4,5,6,7,8,10)]

write.csv(fishlpi, "fishlpi.csv")
fishlpi <- read.csv("fishlpi.csv")
fishlpi <- fishlpi[c(2,3,4,5,6,7,8,10)]

write.csv(herpslpi, "herpslpi.csv")
herpslpi <- read.csv("herpslpi.csv")
herpslpi <- herpslpi[c(2,3,4,5,6,7,8,10)]

write.csv(mammalslpi, "mammalslpi.csv")
mammalslpi <- read.csv("mammalslpi.csv")
mammalslpi <- mammalslpi[c(2,3,4,5,6,7,8,10)]

birds2<-rbind(birdslpi, birdswildonly)
fish2<-rbind(fishlpi, fishwildonly)
herps2<-rbind(herpslpi, herpswildonly)
mammals2<-rbind(mammalslpi, mammalswildonly)

#### Figure S3 ####

#### S3A ####
# All taxa
A2<-ggplot(verts2,aes(x=BodySize.log,colour=lpi))+
  geom_density(size=1)+
  scale_color_manual(values=c("red3", "gray0"))+
  labs(x = "Body Size (log)", y = "Density", col = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
A2
ks.test(vertslpi$BodySize.log, vertswildonly$BodySize.log)

#### S3B ####
#All taxa

B2<-ggplot(verts2,aes(x=LifeSpan.log,colour=lpi))+
  geom_density(size=1)+
  scale_color_manual(values=c("red3", "gray0"))+
  labs(x = "Lifespan (log)", y = "Density", col = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
B2
ks.test(vertslpi$LifeSpan.log, vertswildonly$LifeSpan.log)

#### S3C ####

troph.plot<-data.frame(table(verts2$TrophicLevel,verts2$Group,verts2$lpi))
colnames(troph.plot)<-c("TrophicLevel","Group","lpi","proportion")
troph.plot$TrophicLevel <- as.character(troph.plot$TrophicLevel)
troph.plot$TrophicLevel[troph.plot$TrophicLevel == ""] <- "no data"
troph.plot$TrophicLevel<-factor(troph.plot$TrophicLevel,
                                levels=c("no data", "herbivore","omnivore","carnivore"))

#All taxa
C2<-ggplot(troph.plot,aes(x=lpi,y=proportion,fill=TrophicLevel))+
  geom_col(position="fill", width = .7)+
  scale_fill_grey(start = .9, end = .1) +
  labs(x = "Dataset", y = "Proportion", fill = "Trophic level") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
C2
chisq.test(x=verts2$TrophicLevel,y=verts2$lpi)

#### FIGURE S3: COMBINED ####
ggarrange(A2,B2,C2,nrow=1,ncol=3,labels=c("A","B","C"))
ggsave("figures/figS3_alltaxa.png", width = 11.9, height = 2.76)


#### FIGURE S4 ####

### Body size
#By taxonomic group
ggplot(verts2,aes(x=BodySize.log,colour=lpi))+
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
ggsave("figures/figS4_taxa_bodysize.png", width = 7.29, height = 4.51)

ks.test(birdslpi$BodySize.log, birdswildonly$BodySize.log)
ks.test(fishlpi$BodySize.log, fishwildonly$BodySize.log)
ks.test(mammalslpi$BodySize.log, mammalswildonly$BodySize.log)
ks.test(herpslpi$BodySize.log, herpswildonly$BodySize.log)

#### FIGURE S5 ####
### Lifespan
#By taxonomic group
ggplot(verts2,aes(x=LifeSpan.log,colour=lpi))+
  geom_density(size=1)+
  facet_wrap(vars(Group),nrow=2)+
  scale_color_manual(values=c("red3", "gray0")) +
  labs(x = "Lifespan (log)", y = "Density", col = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/figS5_taxa_lifespan.png", width = 7.29, height = 4.51)

ks.test(birdslpi$LifeSpan.log, birdswildonly$LifeSpan.log)
ks.test(fishlpi$LifeSpan.log, fishwildonly$LifeSpan.log)
ks.test(mammalslpi$LifeSpan.log, mammalswildonly$LifeSpan.log)
ks.test(herpslpi$LifeSpan.log, herpswildonly$LifeSpan.log)

#### FIGURE S6 ####

### Trophic level
#By taxonomic group

ggplot(troph.plot,aes(x=lpi,y=proportion,fill=TrophicLevel))+
  geom_col(position="fill")+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_grey(start = .9, end = .1) +
  labs(x = "Dataset", y = "Proportion", fill = "Trophic level") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/figS6_taxa_trophic.png", width = 7.29, height = 4.51)
chisq.test(x=birds2$TrophicLevel,y=birds2$lpi)
chisq.test(x=fish2$TrophicLevel,y=fish2$lpi)
chisq.test(x=mammals2$TrophicLevel,y=mammals2$lpi)
chisq.test(x=herps2$TrophicLevel,y=herps2$lpi)