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
fishlpi$Group<-"Fishes"
herpslpi$Group<-"Amphibians & Reptiles"
mammalslpi$Group<-"Mammals"

#Species in LPI
vertslpi<-rbind(birdslpi,fishlpi,herpslpi,mammalslpi)

#Species in Canada
birdswild$Group<-"Birds"
fishwild$Group<-"Fishes"
herpswild$Group<-"Amphibians & Reptiles"
mammalswild$Group<-"Mammals"

vertswild<-rbind(birdswild,fishwild,herpswild,mammalswild)

#Species in Canada but not in LPI
vertslpi_sub <- vertslpi[c(1,2,3,4,6,7,9)]
vertswild_sub <- vertswild[c(1,2,3,4,6,7,9)]
vertswildonly<-setdiff(vertswild_sub, vertslpi_sub)
vertswildonly %>% count(Group)
vertswildonly$lpi <- "C-Vertebrates (Only)"

#combine into 1 dataset that can be filtered by "lpi" - lpi and all verts
vertswild$clpi<-"no"
vertslpi$clpi<-"yes"
verts<-rbind(vertslpi, vertswild)
verts <- verts[c(1,2,3,4,5,6,7,9,10)]
verts <- verts %>% mutate(clpi = factor(clpi, levels = c("yes", "no")))

#combine into 1 dataset that can be filtered by "lpi" - lpi and non-lpi
vertslpi2 <- vertslpi[c(1,2,3,4,5,6,7,9)]
vertswildonly$clpi<-"no"
vertslpi2$clpi<-"yes"
verts2 <-rbind(vertslpi2, vertswildonly)
verts2 <- verts2 %>% mutate(clpi = factor(clpi, levels = c("yes", "no")))



#Figure 1
#Taxon-specific representation for the different traits
pal <- as.vector(RColorBrewer::brewer.pal(name = "Dark2", n = 6)[3:6])
F1 <- verts2 %>% 
  select(Binomial, Group, clpi, 
         TrophicLevel, BodySize.log, LifeSpan.log) %>% 
  mutate(across(.cols = everything(), ~ as.character(.))) %>% 
  mutate(Group = factor(Group, 
                        levels = c("Birds", "Fishes", 
                                   "Amphibians & Reptiles", "Mammals"), 
                        labels = c("Birds", "Fishes", 
                                   "Amphibians & Reptiles", "Mammals"))) %>%
  pivot_longer(cols = TrophicLevel:LifeSpan.log, 
               names_to = "measure", values_to = "value") %>% 
  drop_na(value) %>% 
  group_by(clpi, Group, measure) %>% 
  count(.) %>% 
  pivot_wider(names_from = "clpi", values_from = "n") %>% 
  mutate(prop = yes / (yes + no)) %>% 
  ggplot(data = ., 
         mapping = aes(x = measure, y = prop, 
                       group = Group, fill = Group)) + 
  geom_col(colour = "grey20", 
           position = position_dodge(0.9), width = 0.8, alpha = 1) + 
  scale_fill_manual(values = pal, name = "Taxonomic\ngroup") + 
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) + 
  scale_x_discrete(labels = c("Body size (log)", "Lifespan (log)", 
                              "Trophic level")) + 
  labs(x = "Trait", y = "Representation\n(C-LPI / C-Vertebrates)") + 
  theme_classic()+
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))
F1

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




#### Figure S2. Trait relationships - LPI compared to non-LPI
FS2a<-ggplot(verts2,aes(x=BodySize.log,y=LifeSpan.log))+
  geom_point(aes(colour=lpi))+
  geom_smooth(method="lm", aes(colour=lpi))+
  theme_classic()+
  scale_color_manual(values=c("#1B9E77", "#D95F02"))+
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

FS2b<-ggplot(trophic.life.summary,aes(x=TrophicLevel,y=means,colour=lpi,group=lpi))+
  geom_point(size=4,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=sd.l,ymax=sd.u),width=0.2,position=position_dodge(width=0.4))+
  theme_classic()+scale_color_manual(values=c("#1B9E77", "#D95F02"))+ylab("Lifespan (log)")+xlab("Trophic Level")+
  theme(legend.title=element_blank())

trophic.body.summary<-
  data.frame(means=tapply(verts2$BodySize.log,verts2$tr.levels,mean,na.rm=T),
             sds=tapply(verts2$BodySize.log,verts2$tr.levels,sd,na.rm=T))[c(1:4,7,8),]
trophic.body.summary$sd.l<-trophic.body.summary$means-trophic.body.summary$sds
trophic.body.summary$sd.u<-trophic.body.summary$means+trophic.body.summary$sds
trophic.body.summary$TrophicLevel<-t(data.frame(strsplit(rownames(trophic.body.summary),"_")))[,1]
trophic.body.summary$lpi<-t(data.frame(strsplit(rownames(trophic.body.summary),"_")))[,2]

FS2c<-ggplot(trophic.body.summary,aes(x=TrophicLevel,y=means,colour=lpi,group=lpi))+
  geom_point(size=4,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=sd.l,ymax=sd.u),width=0.2,position=position_dodge(width=0.4))+
  theme_classic()+scale_color_manual(values=c("#1B9E77", "#D95F02"))+ylab("Body Size (log)")+xlab("Trophic Level")+
  theme(legend.title=element_blank())

ggarrange(FS2a,FS2b,FS2c,nrow=1,ncol=3,labels=c("a","b","c"))



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
# Supplementary Figures
###Visualizing trait distributions for comparing C-LPI to C-vertebrates (ALL)
## Trait comparisons
################################################################################

#### FIGURE S7A ####

# All taxa
FS7a<-ggplot(verts,aes(x=BodySize.log,fill=lpi))+
  geom_density(size=0.4, alpha=0.4)+
  scale_fill_manual(values = c("#1B9E77", "#D95F02"))+
  labs(x = "Body Size (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))
FS7a

bodysizevertslpi<-vertslpi[complete.cases(vertslpi[ , 6]),]
bodysizevertswild<-vertswild[complete.cases(vertswild[ , 6]),]
OverlapBodySize1<-list(LPI = bodysizevertslpi$BodySize.log, All = bodysizevertswild$BodySize.log)
overlap(OverlapBodySize1)

#### FIGURE S7B ####

#All taxa
FS7b<-ggplot(verts,aes(x=LifeSpan.log,fill=lpi))+
  geom_density(size=0.4, alpha=.4)+
  scale_fill_manual(values = c("#1B9E77", "#D95F02"))+
  labs(x = "Lifespan (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))
FS7b

LifeSpanvertslpi<-vertslpi[complete.cases(vertslpi[ , 7]),]
LifeSpanvertswild<-vertswild[complete.cases(vertswild[ , 7]),]
OverlapLifeSpan1<-list(LPI = LifeSpanvertslpi$LifeSpan.log, All = LifeSpanvertswild$LifeSpan.log)
overlap(OverlapLifeSpan1)

#### FIG S7C ####

#All taxa
troph.plot<-data.frame(table(verts$TrophicLevel,verts$Group,verts$lpi))
colnames(troph.plot)<-c("TrophicLevel","Group","lpi","proportion")
troph.plot$TrophicLevel <- as.character(troph.plot$TrophicLevel)
troph.plot$TrophicLevel[troph.plot$TrophicLevel == ""] <- "no data"
troph.plot$TrophicLevel<-factor(troph.plot$TrophicLevel,
                                levels=c("no data", "herbivore","omnivore","carnivore"))

FS7c <- verts %>% 
  drop_na(TrophicLevel) %>% 
  mutate(TrophicLevel = factor(TrophicLevel,
                               levels = c("herbivore", "omnivore", "carnivore"),
                               labels = c("Herbivore", "Omnivore", "Carnivore"))) %>% 
  group_by(lpi, TrophicLevel) %>% 
  count(.) %>% 
  group_by(lpi) %>% 
  mutate(total = sum(n), 
         prop = n / total) %>% 
  ggplot(data = ., 
         mapping = aes(x = lpi, y = prop, fill = lpi, alpha = TrophicLevel)) + 
  geom_col(colour = "grey20", width = 1) + 
  scale_fill_brewer(palette = "Dark2", name = "Dataset", 
                    labels = c("C-LPI", "C-Vertebrates"), 
                    guide = NULL) + 
  scale_alpha_manual(values = c(0.5, 0.75, 1), name = "Trophic level") + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) + 
  scale_x_discrete(labels = c("C-LPI", "C-Vertebrates")) + 
  labs(x = "Dataset", y = "Proportion") + 
  theme_classic()+
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))
FS7c



#### FIGURE S3: COMBINED ####
ggarrange(FS7a, FS7b, FS7c,nrow=1,ncol=3,labels=c("a","b","c"))


#### FIGURE S8 ####

### Body size
#By taxonomic group
ggplot(verts, aes(x=BodySize.log,fill=lpi))+
  geom_density(size=0.4, alpha=.4)+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_manual(values=c("#1B9E77", "#D95F02","#1B9E77", "#D95F02","#1B9E77", "#D95F02","#1B9E77", "#D95F02")) +
  labs(x = "Body Size (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))


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

#### FIGURE S9 ####

### Lifespan
#By taxonomic group
ggplot(verts,aes(x=LifeSpan.log,fill=lpi))+
  geom_density(size=0.4, alpha=.4)+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  labs(x = "Lifespan (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))

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


#### FIGURE S10 ####

### Trophic level
#By taxonomic group

FS10 <- verts %>% 
  drop_na(TrophicLevel) %>% 
  mutate(TrophicLevel = factor(TrophicLevel,
                               levels = c("herbivore", "omnivore", 
                                          "carnivore"),
                               labels = c("Herbivore", "Omnivore", 
                                          "Carnivore"))) %>% 
  mutate(Group = factor(Group, 
                        levels = c("Amphibians & Reptiles", "Birds", "Fishes", "Mammals"), 
                        labels = c("Amphibians & Reptiles", "Birds", "Fishes", "Mammals"))) %>% 
  group_by(clpi, Group, TrophicLevel) %>% 
  count(.) %>% 
  group_by(clpi, Group) %>% 
  mutate(total = sum(n), 
         prop = n / total) %>% 
  ggplot(data = ., 
         mapping = aes(x = clpi, y = prop, fill = clpi, alpha = TrophicLevel)) + 
  geom_col(colour = "grey20", width = 0.8) + 
  scale_fill_brewer(palette = "Dark2", name = "Dataset", 
                    labels = c("C-LPI", "C-Vertebrates"), 
                    guide = NULL) + 
  scale_alpha_manual(values = c(0.5, 0.75, 1), name = "Trophic level") + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) + 
  scale_x_discrete(labels = c("C-LPI", "C-Vertebrates")) + 
  facet_wrap(~ Group) + 
  labs(x = "Dataset", y = "Proportion") + 
  theme_classic()+
  theme(legend.position = "right",
        strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold"))
FS10


       

################################################################################
#### Trait comparisons - Canadian LPI to not LPI
# Canadian wild (NOT including LPI species) compared to LPI species
################################################################################

birdswildonly<-subset(vertswildonly, Group =="Birds")
fishwildonly<-subset(vertswildonly, Group =="Fishes")
herpswildonly<-subset(vertswildonly, Group =="Amphibians & Reptiles")
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
F2a<-ggplot(verts2,aes(x=BodySize.log, fill=lpi))+
  geom_density(size=.4, alpha=.4)+
  scale_fill_manual(values = c("#1B9E77", "#D95F02"))+
  labs(x = "Body Size (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))
F2a

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

F2b<-ggplot(verts2,aes(x=LifeSpan.log,fill=lpi))+
  geom_density(size=.4, alpha=.4)+
  scale_fill_manual(values = c("#1B9E77", "#D95F02"))+
  labs(x = "Lifespan (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))
F2b

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
F2c <- verts2 %>% 
  drop_na(TrophicLevel) %>% 
  mutate(TrophicLevel = factor(TrophicLevel,
                               levels = c("herbivore", "omnivore", "carnivore"),
                               labels = c("Herbivore", "Omnivore", "Carnivore"))) %>% 
  group_by(lpi, TrophicLevel) %>% 
  count(.) %>% 
  group_by(lpi) %>% 
  mutate(total = sum(n), 
         prop = n / total) %>% 
  ggplot(data = ., 
         mapping = aes(x = lpi, y = prop, fill = lpi, alpha = TrophicLevel)) + 
  geom_col(colour = "grey20", width = 0.8) + 
  scale_fill_brewer(palette = "Dark2", name = "Dataset", 
                    labels = c("C-LPI", "C-Vertebrates (Only)"), 
                    guide = NULL) + 
  scale_alpha_manual(values = c(0.5, 0.75, 1), name = "Trophic level") + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) + 
  scale_x_discrete(labels = c("C-LPI", "C-Vertebrates (Only)")) + 
  labs(x = "Dataset", y = "Proportion") + 
  theme_classic()+
  ggpubr::theme_pubr() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))
F2c


chisq.test(x=verts2$TrophicLevel,y=verts2$lpi)



#### FIGURE 2: COMBINED ####
ggarrange(F2a,F2b,F2c,nrow=1,ncol=3,labels=c("a","b","c"))



#### FIGURE 3 ####

### Body size
#By taxonomic group
ggplot(verts2,aes(x=BodySize.log,fill=lpi))+
  geom_density(size=.4, alpha=.4)+
  facet_wrap(vars(Group),nrow=2)+
  labs(x = "Body size (log)", y = "Density", fill = "Dataset") +
  theme_classic()+
  scale_fill_manual(values=c("#1B9E77", "#D95F02","#1B9E77", "#D95F02","#1B9E77", "#D95F02","#1B9E77", "#D95F02")) +
 ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))



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
  geom_density(size=.4, alpha=.4)+
  facet_wrap(vars(Group),nrow=2)+
  scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  labs(x = "Lifespan (log)", y = "Density", fill = "Dataset") +
  ggpubr::theme_pubr() +
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))


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
F5 <- verts2 %>% 
  drop_na(TrophicLevel) %>% 
  mutate(TrophicLevel = factor(TrophicLevel,
                               levels = c("herbivore", "omnivore", 
                                          "carnivore"),
                               labels = c("Herbivore", "Omnivore", 
                                          "Carnivore"))) %>% 
  mutate(Group = factor(Group, 
                        levels = c("Amphibians & Reptiles", "Birds", "Fishes", "Mammals"), 
                        labels = c("Amphibians & Reptiles", "Birds", "Fishes", "Mammals"))) %>% 
  group_by(lpi, Group, TrophicLevel) %>% 
  count(.) %>% 
  group_by(lpi, Group) %>% 
  mutate(total = sum(n), 
         prop = n / total) %>% 
  ggplot(data = ., 
         mapping = aes(x = lpi, y = prop, fill = lpi, alpha = TrophicLevel)) + 
  geom_col(colour = "grey20", width = 0.8) + 
  scale_fill_brewer(palette = "Dark2", name = "Dataset", 
                    labels = c("C-LPI", "C-Vertebrates (Only)"), 
                    guide = NULL) + 
  scale_alpha_manual(values = c(0.5, 0.75, 1), name = "Trophic level") + 
  scale_y_continuous(limits = c(0, 1.05), expand = c(0,0)) + 
  scale_x_discrete(labels = c("C-LPI", "C-Vertebrates (Only)")) + 
  facet_wrap(~ Group) + 
  labs(x = "Dataset", y = "Proportion") + 
  theme_classic()+
  theme(legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold"))
F5

chisq.test(x=birds2$TrophicLevel,y=birds2$lpi)

chisq.test(x=fish2$TrophicLevel,y=fish2$lpi)
fisher.test(x=fish2$TrophicLevel,y=fish2$lpi)

chisq.test(x=herps2$TrophicLevel,y=herps2$lpi)
fisher.test(x=herps2$TrophicLevel,y=herps2$lpi)

chisq.test(x=mammals2$TrophicLevel,y=mammals2$lpi)





#BOOTSTRAPPING: Supplementary Material
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(dabestr)
library(ggthemes)

## set a plotting theme
theme_set(theme_few() + 
            theme(legend.position = "top"))

## set a random seed (to make the bootstrapping reproducible)
set.seed(444)

## read in data
(verts <- as_tibble(
  readRDS("../data-clean/lpi_and_wild_trait_data.RDS")) %>% 
    relocate(lpi, .before = Binomial) %>% 
    relocate(Group, TrophicLevel, .after = Binomial))

table(verts$lpi)

## C-LPI species
vertslpi <- verts %>% 
  filter(lpi == "C-LPI") %>% 
  arrange(desc(Group), Binomial) %>% 
  select(-lpi)
# unique(vertslpi$Binomial) ## [1] 845

## all Canadian vertebrates (including C-LPI species)
vertswild <- verts %>% 
  filter(lpi == "C-Vertebrates") %>% 
  arrange(desc(Group), Binomial) %>% 
  select(-lpi)

## vector of species in C-Vertebrates but not C-LPI
wild_only <- setdiff(vertswild$Binomial, vertslpi$Binomial)

## check if there are any species in C-LPI not in C-Verts 
## (this shouldn't be possible)
# setdiff(vertslpi$Binomial,vertswild$Binomial) ## character(0) 

## create a new version of vert2, with only a single row per species
## (rather than duplicating species found in both lpi and wild), and 
## adding a column indicating if the spcies is found in C-LPI
verts2 <- vertswild %>% 
  mutate(clpi = ifelse(Binomial %in% unique(vertslpi$Binomial), "C-LPI", "C-Vertebrates (Only)"), 
         clpi = factor(clpi, levels = c("C-LPI", "C-Vertebrates (Only)")))

#bodaysize
bs_est <- verts2 %>% 
  select(Binomial, BodySize.log, clpi) %>% 
  drop_na() %>% 
  dabest(clpi, BodySize.log, 
         idx = c("C-LPI", "C-Vertebrates (Only)"))

bs_est.md <- median_diff(bs_est)
# mean_diff(bs_est)
# cohens_d(tt)
# hedges_g(tt)
# cliffs_delta(bs_est)

FS3<-plot(bs_est.md, 
         rawplot.ylabel = "Body size (log)", 
         effsize.ylabel = "Median difference", 
         tick.fontsize = "9", 
         axes.title.fontsize = "12", 
         palette = c("#1B9E77", "#D95F02"))
FS3

#lifespan
ls_est <- verts2 %>% 
  select(Binomial, LifeSpan.log, clpi) %>% 
  drop_na() %>% 
  dabest(clpi, LifeSpan.log, 
         idx = c("C-LPI", "C-Vertebrates (Only)"))

ls_est.md <- median_diff(ls_est)
# mean_diff(bs_est)
# cohens_d(tt)
# hedges_g(tt)
# cliffs_delta(bs_est)

FS4<-plot(median_diff(ls_est), 
         rawplot.ylabel = "Lifespan (log)", 
         effsize.ylabel = "Median difference", 
         tick.fontsize = "9", 
         axes.title.fontsize = "12", 
         palette = c("#1B9E77", "#D95F02"))
FS4
## try doing it with interaction between clpi and group
## body size
bs_est2 <- verts2 %>% 
  mutate(Group = factor(Group, 
                        levels = c("Amphibians & Reptiles", "Birds", "Fishes", "Mammals"), 
                        labels = c("Amphibians & Reptiles", "Birds", "Fishes", "Mammals")), 
         clpi = factor(clpi, 
                       levels = c("C-LPI", "C-Vertebrates (Only)"), 
                       labels = c("LPI", "Only"))) %>% 
  unite(col = "Group2", Group, clpi, remove = FALSE) %>% 
  select(Binomial, BodySize.log, Group, clpi, Group2) %>% 
  drop_na() %>% 
  dabest(Group2, BodySize.log, 
         idx = list(c("Amphibians & Reptiles_LPI", "Amphibians & Reptiles_Only"),
                    c("Birds_LPI", "Birds_Only"), 
                    c("Fishes_LPI", "Fishes_Only"),
                    c("Mammals_LPI", "Mammals_Only")))

bs_est2.md <- median_diff(bs_est2)

FS5<-plot(bs_est2.md, 
         color.column = clpi, 
         group.summaries = "median_quartiles", 
         rawplot.ylabel = "Body size (log)", 
         effsize.ylabel = "Median difference", 
         tick.fontsize = "9", 
         axes.title.fontsize = "12", 
         palette = c("#1B9E77", "#D95F02","#1B9E77", "#D95F02","#1B9E77", "#D95F02","#1B9E77", "#D95F02"), 
         theme = theme_classic() + 
           theme(legend.position = "none"))
FS5
## life span
ls_est2 <- verts2 %>% 
  mutate(Group = factor(Group, 
                        levels = c("Amphibians & Reptiles", "Birds", "Fishes", "Mammals"), 
                        labels = c("Amphibians & Reptiles", "Birds", "Fishes", "Mammals")),
         clpi = factor(clpi, 
                       levels = c("C-LPI", "C-Vertebrates (Only)"), 
                       labels = c("LPI", "Only"))) %>% 
  unite(col = "group_clpi", Group, clpi, remove = FALSE) %>% 
  select(Binomial, LifeSpan.log, Group, clpi, group_clpi) %>% 
  drop_na() %>% 
  dabest(group_clpi, LifeSpan.log, 
         idx = list(c("Amphibians & Reptiles_LPI", "Amphibians & Reptiles_Only"),
                    c("Birds_LPI", "Birds_Only"), 
                    c("Fishes_LPI", "Fishes_Only"),
                    c("Mammals_LPI", "Mammals_Only")))

ls_est2.md <- median_diff(ls_est2)

FS6<-plot(ls_est2.md, 
         color.column = clpi, 
         group.summaries = "median_quartiles", 
         rawplot.ylabel = "Lifespan (log)", 
         effsize.ylabel = "Median difference", 
         tick.fontsize = "9", 
         axes.title.fontsize = "12", 
         palette = c("#1B9E77", "#D95F02","#1B9E77", "#D95F02","#1B9E77", "#D95F02","#1B9E77", "#D95F02"), 
         theme = theme_classic() + 
           theme(legend.position = "none"))
FS6
#summary of results
bs_est.md ## overall comparison - body size
ls_est.md ## overall comparison - lifespan

bs_est2.md ## taxon-specific comparison - body size
ls_est2.md ## taxon-specific comparison - lifespan
