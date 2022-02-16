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
library(overlapping)


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
write.csv(mammals, "mammals.csv")
mammals <- read.csv("mammals.csv")
mammals <- mammals[c(2,3,4,5,6)]
mammalslpi<-subset(mammals, lpi =="C-LPI")
mammalswild<-mammals
mammalslpi$BodySize.log<-log(mammalslpi$BodySize)
mammalslpi$LifeSpan.log<-log(mammalslpi$LifeSpan)
mammalswild$BodySize.log<-log(mammalswild$BodySize)
mammalswild$LifeSpan.log<-log(mammalswild$LifeSpan)
mammalslpi$countNA <-apply(mammalslpi, MARGIN = 1, function(x) sum(is.na(x)))
mammalslpi <- mammalslpi[ !(mammalslpi$countNA %in% c("5")), ]
mammalswild$countNA <-apply(mammalswild, MARGIN = 1, function(x) sum(is.na(x)))
mammalswild <- mammalswild[ !(mammalswild$countNA %in% c("5")), ]
mammalswild$lpi<-"C-Vertebrates"


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
vertswildonly$lpi <- "C-Vertebrates (Only)"

#combine into 1 dataset that can be filtered by "lpi" - lpi and all verts
verts<-rbind(vertslpi, vertswild)
verts <- verts[c(1,2,3,4,5,6,7,9)]

#combine into 1 dataset that can be filtered by "lpi" - lpi and non-lpi
vertslpi2 <- vertslpi[c(1,2,3,4,5,6,7,9)]
verts2 <-rbind(vertslpi2, vertswildonly)

#combine into 1 dataset that can be filtered by "lpi" - non-lpi and all verts
vertswildtemp<- vertswild[c(1,2,3,4,5,6,7,9)]
verts3 <- rbind(vertswildtemp, vertswildonly)

################################################################################
## Data checks
### Species in the LPI dataset but not the Canadian wild species list (should NOT be a thing)
### Species should only be in the LPI IF they are in Canadaian wild species list
################################################################################

vertslpionly<-setdiff(vertslpi_sub, vertswild_sub)

#### Test of normality

#LPI
ggqqplot(vertslpi$BodySize.log)
shapiro.test(vertslpi$BodySize.log)

ggqqplot(vertslpi$LifeSpan.log)
shapiro.test(vertslpi$LifeSpan.log)

#Non-LPI
ggqqplot(vertslpi$BodySize.log)
shapiro.test(vertswildonly$BodySize.log)

ggqqplot(vertslpi$LifeSpan.log)
shapiro.test(vertswildonly$LifeSpan.log)




#### Trait relationships - LPI compared to non-LPI
A1<-ggplot(verts2,aes(x=BodySize.log,y=LifeSpan.log))+
  geom_point(aes(colour=lpi))+
  geom_smooth(method="lm", aes(colour=lpi))+
  theme_classic()+
  scale_color_manual(values=c("olivedrab", "gray0"))+
  xlab("Body Size (log)")+ylab("Lifespan (log)")+
  theme(legend.title=element_blank())

verts2$tr.levels<-paste(verts2$TrophicLevel,verts2$lpi,sep="_")

trophic.life.summary<-
  data.frame(means=tapply(verts2$LifeSpan.log,verts2$tr.levels,mean,na.rm=T),
             sds=tapply(verts2$LifeSpan.log,verts2$tr.levels,sd,na.rm=T))[c(1:4,7,8),]
trophic.life.summary$sd.l<-trophic.life.summary$means-trophic.life.summary$sds
trophic.life.summary$sd.u<-trophic.life.summary$means+trophic.life.summary$sds
trophic.life.summary$TrophicLevel<-t(data.frame(strsplit(rownames(trophic.life.summary),"_")))[,1]
trophic.life.summary$lpi<-t(data.frame(strsplit(rownames(trophic.life.summary),"_")))[,2]

B1<-ggplot(trophic.life.summary,aes(x=TrophicLevel,y=means,colour=lpi,group=lpi))+
  geom_point(size=4,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=sd.l,ymax=sd.u),width=0.2,position=position_dodge(width=0.4))+
  theme_classic()+scale_color_manual(values=c("olivedrab", "gray0"))+ylab("Lifespan (log)")+xlab("Trophic Level")+
  theme(legend.title=element_blank())

trophic.body.summary<-
  data.frame(means=tapply(verts2$BodySize.log,verts2$tr.levels,mean,na.rm=T),
             sds=tapply(verts2$BodySize.log,verts2$tr.levels,sd,na.rm=T))[c(1:4,7,8),]
trophic.body.summary$sd.l<-trophic.body.summary$means-trophic.body.summary$sds
trophic.body.summary$sd.u<-trophic.body.summary$means+trophic.body.summary$sds
trophic.body.summary$TrophicLevel<-t(data.frame(strsplit(rownames(trophic.body.summary),"_")))[,1]
trophic.body.summary$lpi<-t(data.frame(strsplit(rownames(trophic.body.summary),"_")))[,2]

C1<-ggplot(trophic.body.summary,aes(x=TrophicLevel,y=means,colour=lpi,group=lpi))+
  geom_point(size=4,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=sd.l,ymax=sd.u),width=0.2,position=position_dodge(width=0.4))+
  theme_classic()+scale_color_manual(values=c("olivedrab", "gray0"))+ylab("Body Size (log)")+xlab("Trophic Level")+
  theme(legend.title=element_blank())

A1
B1
C1

ggarrange(A1,B1,C1,nrow=1,ncol=3,labels=c("a","b","c"))
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
################################################################################

#### FIGURE S3A ####

# All taxa
A2<-ggplot(verts,aes(x=BodySize.log,fill=lpi))+
  geom_density(size=1, alpha=.5)+
  scale_fill_manual(values = c("olivedrab", "azure"))+
  labs(x = "Body Size (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
A2

bodysizevertslpi<-vertslpi[complete.cases(vertslpi[ , 6]),]
bodysizevertswild<-vertswild[complete.cases(vertswild[ , 6]),]
OverlapBodySize1<-list(LPI = bodysizevertslpi$BodySize.log, All = bodysizevertswild$BodySize.log)
overlap(OverlapBodySize1)

#### FIGURE S3B ####

#All taxa
B2<-ggplot(verts,aes(x=LifeSpan.log,fill=lpi))+
  geom_density(size=1, alpha=.5)+
  scale_fill_manual(values = c("olivedrab", "azure"))+
  labs(x = "Lifespan (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
B2

LifeSpanvertslpi<-vertslpi[complete.cases(vertslpi[ , 7]),]
LifeSpanvertswild<-vertswild[complete.cases(vertswild[ , 7]),]
OverlapLifeSpan1<-list(LPI = LifeSpanvertslpi$LifeSpan.log, All = LifeSpanvertswild$LifeSpan.log)
overlap(OverlapLifeSpan1)

#### FIG S3C ####

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


#### FIGURE S3: COMBINED ####
ggarrange(A2,B2,C2,nrow=1,ncol=3,labels=c("a","b","c"))
ggsave("figures/fig2_alltaxa.png", width = 11.9, height = 2.76)

#### FIGURE S4 ####

### Body size
#By taxonomic group
ggplot(verts, aes(x=BodySize.log,fill=lpi))+
  geom_density(size=1, alpha=.5)+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_manual(values=c("olivedrab", "azure","olivedrab", "azure","olivedrab", "azure","olivedrab", "azure")) +
  labs(x = "Body Size (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/fig3_taxa_bodysize.png", width = 7.29, height = 4.51)

bodysizebirdslpi<-birdslpi[complete.cases(birdslpi[ , 6]),]
bodysizebirdswild<-birdswild[complete.cases(birdswild[ , 6]),]
OverlapBodySizebirds<-list(LPI = bodysizebirdslpi$BodySize.log, All = bodysizebirdswild$BodySize.log)
overlap(OverlapBodySizebirds)

bodysizefishlpi<-fishlpi[complete.cases(fishlpi[ , 6]),]
bodysizefishwild<-fishwild[complete.cases(fishwild[ , 6]),]
OverlapBodySizefish<-list(LPI = bodysizefishlpi$BodySize.log, All = bodysizefishwild$BodySize.log)
overlap(OverlapBodySizefish)

bodysizemammalslpi<-mammalslpi[complete.cases(mammalslpi[ , 6]),]
bodysizemammalswild<-mammalswild[complete.cases(mammalswild[ , 6]),]
OverlapBodySizemammals<-list(LPI = bodysizemammalslpi$BodySize.log, All = bodysizemammalswild$BodySize.log)
overlap(OverlapBodySizemammals)

bodysizeherpslpi<-herpslpi[complete.cases(herpslpi[ , 6]),]
bodysizeherpswild<-herpswild[complete.cases(herpswild[ , 6]),]
OverlapBodySizeherps<-list(LPI = bodysizeherpslpi$BodySize.log, All = bodysizeherpswild$BodySize.log)
overlap(OverlapBodySizeherps)

#### FIGURE S5 ####

### Lifespan
#By taxonomic group
ggplot(verts,aes(x=LifeSpan.log,fill=lpi))+
  geom_density(size=1, alpha=.5)+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_manual(values=c("olivedrab", "azure")) +
  labs(x = "Lifespan (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/fig4_taxa_lifespan.png", width = 7.29, height = 4.51)

lifespanbirdslpi<-birdslpi[complete.cases(birdslpi[ , 7]),]
lifespanbirdswild<-birdswild[complete.cases(birdswild[ , 7]),]
OverlapLifeSpanbirds<-list(LPI = lifespanbirdslpi$LifeSpan.log, All = lifespanbirdswild$LifeSpan.log)
overlap(OverlapLifeSpanbirds)

lifespanfishlpi<-fishlpi[complete.cases(fishlpi[ , 7]),]
lifespanfishwild<-fishwild[complete.cases(fishwild[ , 7]),]
OverlapLifeSpanfish<-list(LPI = lifespanfishlpi$LifeSpan.log, All = lifespanfishwild$LifeSpan.log)
overlap(OverlapLifeSpanfish)

lifespanmammalslpi<-mammalslpi[complete.cases(mammalslpi[ , 7]),]
lifespanmammalswild<-mammalswild[complete.cases(mammalswild[ , 7]),]
OverlapLifeSpanmammals<-list(LPI = lifespanmammalslpi$LifeSpan.log, All = lifespanmammalswild$LifeSpan.log)
overlap(OverlapLifeSpanmammals)

lifespanherpslpi<-herpslpi[complete.cases(herpslpi[ , 7]),]
lifespanherpswild<-herpswild[complete.cases(herpswild[ , 7]),]
OverlapLifeSpanherps<-list(LPI = lifespanherpslpi$LifeSpan.log, All = lifespanherpswild$LifeSpan.log)
overlap(OverlapLifeSpanherps)


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
ggsave("figures/fig5_taxa_trophic.png", width = 7.29, height = 4.51)



################################################################################
#### Trait comparisons - Canadian LPI to not LPI
# Canadian wild (NOT including LPI species) compared to LPI species
################################################################################

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

#### Figure 2 ####

#### 2A ####
# All taxa
A2<-ggplot(verts2,aes(x=BodySize.log, fill=lpi))+
  geom_density(size=1, alpha=.5)+
  scale_fill_manual(values = c("olivedrab", "azure"))+
  labs(x = "Body Size (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
A2

bodysizelpi<-vertslpi[complete.cases(vertslpi[ , 6]),]
bodysizewildonly<-vertswildonly[complete.cases(vertswildonly[ , 5]),]
set.seed(1000)
bodysizelpi_ks<-jitter(bodysizelpi$BodySize.log, factor=0.001)
bodysizewildonly_ks<-jitter(bodysizewildonly$BodySize.log, factor=0.001)
ks.test(bodysizelpi_ks, bodysizewildonly_ks)

OverlapBodySize<-list(LPI = bodysizelpi$BodySize.log, Only = bodysizewildonly$BodySize.log)
overlap(OverlapBodySize)


#### 2B ####
#All taxa

B2<-ggplot(verts2,aes(x=LifeSpan.log,fill=lpi))+
  geom_density(size=1, alpha=.5)+
  scale_fill_manual(values = c("olivedrab", "azure"))+
  labs(x = "Lifespan (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
B2

lifespanlpi<-vertslpi[complete.cases(vertslpi[ , 7]),]
lifespanwildonly<-vertswildonly[complete.cases(vertswildonly[ , 6]),]
set.seed(1000)
lifespanlpi_ks<-jitter(lifespanlpi$LifeSpan.log, factor=0.001)
lifespanwildonly_ks<-jitter(lifespanwildonly$LifeSpan.log, factor=0.001)
ks.test(lifespanlpi_ks, lifespanwildonly_ks)

OverlapLifeSpan<-list(LPI = lifespanlpi$LifeSpan.log, Only = lifespanwildonly$LifeSpan.log)
overlap(OverlapLifeSpan)


#### 2C ####

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
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
C2
chisq.test(x=verts2$TrophicLevel,y=verts2$lpi)



#### FIGURE 2: COMBINED ####
ggarrange(A2,B2,C2,nrow=1,ncol=3,labels=c("a","b","c"))
ggsave("figures/figS3_alltaxa.png", width = 11.9, height = 2.76)


#### FIGURE 3 ####

### Body size
#By taxonomic group
ggplot(verts2,aes(x=BodySize.log,fill=lpi))+
  geom_density(size=1, alpha=.5)+
  facet_wrap(vars(Group),nrow=2)+
  theme_classic()+
  scale_fill_manual(values=c("olivedrab", "azure","olivedrab", "azure","olivedrab", "azure","olivedrab", "azure")) +
  labs(x = "Body Size (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/figS4_taxa_bodysize.png", width = 7.29, height = 4.51)


bodysizebirdslpi<-birdslpi[complete.cases(birdslpi[ , 6]),]
bodysizebirdswildonly<-birdswildonly[complete.cases(birdswildonly[ , 5]),]
set.seed(1000)
bodysizebirdslpi_ks<-jitter(bodysizebirdslpi$BodySize.log, factor=0.001)
bodysizebirdswildonly_ks<-jitter(bodysizebirdswildonly$BodySize.log, factor=0.001)
ks.test(bodysizebirdslpi_ks, bodysizebirdswildonly_ks)

OverlapBodySizebirds<-list(LPI = bodysizebirdslpi$BodySize.log, Only = bodysizebirdswildonly$BodySize.log)
overlap(OverlapBodySizebirds)

bodysizefishlpi<-fishlpi[complete.cases(fishlpi[ , 6]),]
bodysizefishwildonly<-fishwildonly[complete.cases(fishwildonly[ , 5]),]
set.seed(1000)
bodysizefishlpi_ks<-jitter(bodysizefishlpi$BodySize.log, factor=0.001)
bodysizefishwildonly_ks<-jitter(bodysizefishwildonly$BodySize.log, factor=0.001)
ks.test(bodysizefishlpi_ks, bodysizefishwildonly_ks)

OverlapBodySizefish<-list(LPI = bodysizefishlpi$BodySize.log, Only = bodysizefishwildonly$BodySize.log)
overlap(OverlapBodySizefish)

bodysizemammalslpi<-mammalslpi[complete.cases(mammalslpi[ , 6]),]
bodysizemammalswildonly<-mammalswildonly[complete.cases(mammalswildonly[ , 5]),]
set.seed(1000)
bodysizemammalslpi_ks<-jitter(bodysizemammalslpi$BodySize.log, factor=0.001)
bodysizemammalswildonly_ks<-jitter(bodysizemammalswildonly$BodySize.log, factor=0.001)
ks.test(bodysizemammalslpi_ks, bodysizemammalswildonly_ks)

OverlapBodySizemammals<-list(LPI = bodysizemammalslpi$BodySize.log, Only = bodysizemammalswildonly$BodySize.log)
overlap(OverlapBodySizemammals)

bodysizeherpslpi<-herpslpi[complete.cases(herpslpi[ , 6]),]
bodysizeherpswildonly<-herpswildonly[complete.cases(herpswildonly[ , 5]),]
set.seed(1000)
bodysizeherpslpi_ks<-jitter(bodysizeherpslpi$BodySize.log, factor=0.001)
bodysizeherpswildonly_ks<-jitter(bodysizeherpswildonly$BodySize.log, factor=0.001)
ks.test(bodysizeherpslpi_ks, bodysizeherpswildonly_ks)

OverlapBodySizeherps<-list(LPI = bodysizeherpslpi$BodySize.log, Only = bodysizeherpswildonly$BodySize.log)
overlap(OverlapBodySizeherps)


#### FIGURE 4 ####
### Lifespan
#By taxonomic group
ggplot(verts2,aes(x=LifeSpan.log,fill=lpi))+
  geom_density(size=1, alpha=.5)+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_manual(values=c("olivedrab", "azure")) +
  labs(x = "Lifespan (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"))
ggsave("figures/figS5_taxa_lifespan.png", width = 7.29, height = 4.51)

lifespanbirdslpi<-birdslpi[complete.cases(birdslpi[ , 7]),]
lifespanbirdswildonly<-birdswildonly[complete.cases(birdswildonly[ , 6]),]
set.seed(1000)
lifespanbirdslpi_ks<-jitter(lifespanbirdslpi$LifeSpan.log, factor=0.001)
lifespanbirdswildonly_ks<-jitter(lifespanbirdswildonly$LifeSpan.log, factor=0.001)
ks.test(lifespanbirdslpi_ks, lifespanbirdswildonly_ks)

OverlapLifeSpanbirds<-list(LPI = lifespanbirdslpi$LifeSpan.log, Only = lifespanbirdswildonly$LifeSpan.log)
overlap(OverlapLifeSpanbirds)

lifespanfishlpi<-fishlpi[complete.cases(fishlpi[ , 7]),]
lifespanfishwildonly<-fishwildonly[complete.cases(fishwildonly[ , 6]),]
set.seed(1000)
lifespanfishlpi_ks<-jitter(lifespanfishlpi$LifeSpan.log, factor=0.001)
lifespanfishwildonly_ks<-jitter(lifespanfishwildonly$LifeSpan.log, factor=0.001)
ks.test(lifespanfishlpi_ks, lifespanfishwildonly_ks)

OverlapLifeSpanfish<-list(LPI = lifespanfishlpi$LifeSpan.log, Only = lifespanfishwildonly$LifeSpan.log)
overlap(OverlapLifeSpanfish)

lifespanmammalslpi<-mammalslpi[complete.cases(mammalslpi[ , 7]),]
lifespanmammalswildonly<-mammalswildonly[complete.cases(mammalswildonly[ , 6]),]
set.seed(1000)
lifespanmammalslpi_ks<-jitter(lifespanmammalslpi$LifeSpan.log, factor=0.001)
lifespanmammalswildonly_ks<-jitter(lifespanmammalswildonly$LifeSpan.log, factor=0.001)
ks.test(lifespanmammalslpi_ks, lifespanmammalswildonly_ks)

OverlapLifeSpanmammals<-list(LPI = lifespanmammalslpi$LifeSpan.log, Only = lifespanmammalswildonly$LifeSpan.log)
overlap(OverlapLifeSpanmammals)

lifespanherpslpi<-herpslpi[complete.cases(herpslpi[ , 7]),]
lifespanherpswildonly<-herpswildonly[complete.cases(herpswildonly[ , 6]),]
set.seed(1000)
lifespanherpslpi_ks<-jitter(lifespanherpslpi$LifeSpan.log, factor=0.001)
lifespanherpswildonly_ks<-jitter(lifespanherpswildonly$LifeSpan.log, factor=0.001)
ks.test(lifespanherpslpi_ks, lifespanherpswildonly_ks)

OverlapLifeSpanherps<-list(LPI = lifespanherpslpi$LifeSpan.log, Only = lifespanherpswildonly$LifeSpan.log)
overlap(OverlapLifeSpanherps)


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
ggsave("figures/figS6_taxa_trophic.png", width = 7.29, height = 4.51)

chisq.test(x=birds2$TrophicLevel,y=birds2$lpi)

chisq.test(x=fish2$TrophicLevel,y=fish2$lpi)
fisher.test(x=fish2$TrophicLevel,y=fish2$lpi)

chisq.test(x=herps2$TrophicLevel,y=herps2$lpi)
fisher.test(x=herps2$TrophicLevel,y=herps2$lpi)

chisq.test(x=mammals2$TrophicLevel,y=mammals2$lpi)



