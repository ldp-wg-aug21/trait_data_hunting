###installing packages

# library(devtools)
# # Install traitdata package from Github
# remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)


library(traitdata)
library(tidyverse)

#load Canada data
dat<-read.csv("data-raw/CIEE_LPI_dataset.csv")
#herps-only data for taxonomy
herps<-unique(dat[dat$Class%in%c("Amphibia","Reptilia"),c("Binomial","Class","Order","Family","Genus","Species")])

data("amphibio")
colnames(amphibio)
#make Binomial column with same formatting and extract relevant data
amphibio$Binomial<-paste(amphibio$Genus,amphibio$Species,sep="_")
herps<-left_join(herps,amphibio,by="Binomial")
rm(amphibio)


data("amphi_lifehist")
amphi_lifehist$Binomial<-paste(amphi_lifehist$Genus,amphi_lifehist$Species,sep="_")
length(setdiff(herps$Binomial,amphi_lifehist))==nrow(herps) #No overlapping species
rm(amphi_lifehist)


#all Colombian species - no overlapping species aside from American Bullfrog
##already have Bullfrog data from amphibio
data(anuran_morpho)
colnames(anuran_morpho)
anuran_morpho$Binomial<-paste(anuran_morpho$Genus,anuran_morpho$Species,sep="_")
intersect(herps$Binomial,anuran_morpho$Binomial)
rm(anuran_morpho)

data(lizard_traits)
lizard_traits$Binomial<-gsub(" ","_",lizard_traits$scientificNameStd)
intersect(herps$Binomial,lizard_traits$Binomial) #five lined skink data!
herps[herps$Order.x=="Squamata",] #only lizard in dataset
herps<-left_join(herps,lizard_traits,by="Binomial")
rm(lizard_traits)
herps[herps$Binomial=="Plestiodon_fasciatus",]

#reptile_lifehist is only european species - but sea turtles swim a lot
data("reptile_lifehist")
reptile_lifehist$Binomial<-paste(reptile_lifehist$Genus,reptile_lifehist$Species,sep="_")
intersect(herps$Binomial,reptile_lifehist$Binomial) #leatherback sea turtle
#but all data are NA's?
reptile_lifehist[reptile_lifehist$Binomial=="Dermochelys_coriacea",]
#herps<-left_join(herps,reptile_lifehist,by="Binomial")
rm(reptile_lifehist)


#amphibiaweb not in R - will have to extract data manually




#Start cleaning up dataframe
herps<-herps[,colSums(is.na(herps))<nrow(herps)] #remove rows of all NAs
colnames(herps)
#clean up repetitive columns
herps<-herps[,!colnames(herps)%in%c("id","Order.y","Family.y","Genus.y","Species.y",
                                    "scientificNameStd.x","scientificNameStd.y",
                                    "Genus","Species","Family")]

body_size_colnames<-c("Body_size_mm","Size_at_maturity_min_mm", "Size_at_maturity_max_mm",
                      "maximum SVL","female SVL")
herps.1<-herps[,c("Binomial","Class","Order.x",body_size_colnames)]
herps.1[rowSums(is.na(herps.1[,3:8]))==5,] #no body size estimates for most reptiles
