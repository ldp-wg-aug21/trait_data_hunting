###installing packages

# library(devtools)
# # Install traitdata package from Github
# remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)
# #Install squamatabase
# devtools::install_github("blueraleigh/squamatabase")


library(traitdata)
library(tidyverse)
library(squamatabase)

setwd("C:/Users/Steph/OneDrive/Documents/LDP working group/trait_data_hunting/")

#load Canada data
lpi.dat<-read.csv("data-raw/CIEE_LPI_dataset.csv")
levels(as.factor(lpi.dat$Class))
#herps-only data for taxonomy
herps<-unique(lpi.dat[lpi.dat$Class%in%c("Amphibia","Reptilia","Herps"),c("Binomial","Class","Order","Family","Genus","Species")])
nrow(unique(herps[,c("Binomial","Order","Family","Genus","Species")]))
lpi.dat[lpi.dat$Order=="Urodela",1:8]


data("amphibio")
#make Binomial column with same formatting and extract relevant data
amphibio$Binomial<-paste(amphibio$Genus,amphibio$Species,sep="_")
#remove intersecting cols to avoid issue with NAs in amphibio
amphibio<-amphibio[,!colnames(amphibio)%in%intersect(colnames(herps),colnames(amphibio))[2:5]]
#too much data - select species of interest
amphibio<-subset(amphibio,amphibio$Binomial%in%herps$Binomial)
amphibio<-data.frame(amphibio)

amphibio$Herb<-rowSums(amphibio[,c("Leaves","Flowers","Seeds","Fruits")],na.rm=T)
amphibio$Carn<-rowSums(amphibio[,c("Arthro","Vert")],na.rm=T)
amphibio$Diet<-NA
amphibio$Diet[amphibio$Herb==0&amphibio$Carn>0]<-"carnivore"
amphibio$Diet[amphibio$Herb>0&amphibio$Carn==0]<-"herbivore"
amphibio$Diet[amphibio$Herb>0&amphibio$Carn>0]<-"omnivore"

for(x in c("Fos","Ter","Aqu","Arb","Diu","Noc","Crepu","Wet_warm","Wet_cold",
           "Dry_warm","Dry_cold","Dir","Lar","Viv",
           "Leaves","Flowers","Seeds","Fruits","Arthro","Vert")){
  amphibio[,x]<-gsub(1,x,amphibio[,x])}
for(x in c("Dir","Lar","Viv")){amphibio[,x]<-gsub(0,NA,amphibio[,x])}; rm(x)

amphibio<-amphibio%>%unite("Habitat",c(Fos,Ter,Aqu,Arb),sep=",",na.rm=T)
amphibio<-amphibio%>%unite("Seasonality",c(Wet_warm,Wet_cold,Dry_warm,Dry_cold),sep=",",na.rm=T)
amphibio<-amphibio%>%unite("Diet_items",c(Leaves,Flowers,Seeds,Fruits,Arthro,Vert),sep=",",na.rm=T)
amphibio<-amphibio%>%unite("Breeding_strategy",c(Dir,Lar,Viv),sep=",",na.rm=T)
amphibio<-amphibio%>%unite("Diel",c(Diu,Noc,Crepu),sep=",",na.rm=T)

amphibio<-amphibio%>%select(c(Habitat,Diet_items,Diel,Seasonality,Body_mass_g,Age_at_maturity_min_y,
                              Age_at_maturity_max_y,Body_size_mm,Size_at_maturity_min_mm,Size_at_maturity_max_mm,
                              Longevity_max_y,Litter_size_min_n,Litter_size_max_n,Reproductive_output_y,
                              Offspring_size_min_mm,Offspring_size_max_mm,Breeding_strategy,Binomial,Diet))

colnames(amphibio)[colnames(amphibio)!="Binomial"]<-
  paste("db1",colnames(amphibio)[colnames(amphibio)!="Binomial"],sep="_")


herps<-left_join(herps,amphibio,by="Binomial",na.omit=T)
rm(amphibio)


# data("amphi_lifehist")
# amphi_lifehist$Binomial<-paste(amphi_lifehist$Genus,amphi_lifehist$Species,sep="_")
# length(setdiff(herps$Binomial,amphi_lifehist))==nrow(herps) #No overlapping species
# rm(amphi_lifehist)


#all Colombian species - no overlapping species aside from American Bullfrog
##already have Bullfrog data from amphibio
# data(anuran_morpho)
# colnames(anuran_morpho)
# anuran_morpho$Binomial<-paste(anuran_morpho$Genus,anuran_morpho$Species,sep="_")
# intersect(herps$Binomial,anuran_morpho$Binomial)
# rm(anuran_morpho)

data(lizard_traits)
lizard_traits$Binomial<-gsub(" ","_",lizard_traits$scientificNameStd)
intersect(herps$Binomial,lizard_traits$Binomial) #five lined skink data!
herps[herps$Order=="Squamata",] #only lizard in dataset
lizard_traits<-select(lizard_traits,c("maximum SVL","female SVL","hatchling/neonate SVL","diet","reproductive mode",
                                      "clutch size","smallest clutch","largest clutch","smallest mean clutch size",
                                      "largest mean clutch size","breeding age (months)","Binomial"))
colnames(lizard_traits)[1:3]<-paste(c("maximum SVL","female SVL","hatchling/neonate SVL"),"mm",sep="_")
colnames(lizard_traits)[6:10]<-paste(colnames(lizard_traits)[6:10],"n",sep="_")
colnames(lizard_traits)[colnames(lizard_traits)!="Binomial"]<-
  paste("db2",colnames(lizard_traits)[colnames(lizard_traits)!="Binomial"],sep="_")
herps<-left_join(herps,lizard_traits,by="Binomial")
rm(lizard_traits)

#reptile_lifehist is only european species - but sea turtles swim a lot
# data("reptile_lifehist")
# reptile_lifehist$Binomial<-paste(reptile_lifehist$Genus,reptile_lifehist$Species,sep="_")
# intersect(herps$Binomial,reptile_lifehist$Binomial) #leatherback sea turtle
# #but all data are NA's?
# reptile_lifehist[reptile_lifehist$Binomial=="Dermochelys_coriacea",]
# #herps<-left_join(herps,reptile_lifehist,by="Binomial")
# rm(reptile_lifehist)


#amphibiaweb not in R - will have to extract data manually

data("amniota")
amniota<-amniota[amniota$Class=="Reptilia",] #only keep reptiles
amniota$Binomial<-gsub(" ","_",amniota$scientificNameStd)
intersect(herps$Binomial,amniota$Binomial) #so many herps!
amniota<-amniota[,!colnames(amniota)%in%c(intersect(colnames(herps),colnames(amniota))[2:7],
                                          "common_name","Subspecies","scientificNameStd")]
amniota[,grep("_cm",colnames(amniota))]<-amniota[,grep("_cm",colnames(amniota))]*10
colnames(amniota)<-gsub("_cm","_mm",colnames(amniota))
colnames(amniota)[colnames(amniota)!="Binomial"]<-
  paste("db3",colnames(amniota)[colnames(amniota)!="Binomial"],sep="_")
herps<-left_join(herps,amniota,by="Binomial")
rm(amniota)



#squamatabase
squam.herps<-subset(herps,herps$Order=="Squamata")
squam.herps$Species_temp<-gsub("_"," ",squam.herps$Binomial)
squam.herps$prop.animal.diet<-NA
squam.herps$adult_svl_mm<-NA
squam.herps$adult_mass_g<-NA
i<-2
for(i in 1:nrow(squam.herps)){ #for each squamata species
  #search squamatabase data for each species ID
  diet1<-filter_records(predator_taxon=squam.herps$Species_temp[i])
  if(nrow(diet1>0)){ #if there are records
    diet2<-filter_records(diet1, prey_taxon = "Animalia") #get animal records, calc poportion of animal prey
    squam.herps[squam.herps$Species_temp==squam.herps$Species_temp[i],
                "prop.animal.diet"]<-nrow(diet1)/nrow(diet2)
    diet3<-subset(diet1,predator_age=="adult") #select adults only
    squam.herps[squam.herps$Species_temp==squam.herps$Species_temp[i],
                "adult_svl"]<-mean(diet3$predator_svl,na.rm=T)
    squam.herps[squam.herps$Species_temp==squam.herps$Species_temp[i],
                "adult_mass"]<-mean(diet3$predator_mass,na.rm=T)}}

squam.herps<-select(squam.herps,c("Binomial","prop.animal.diet","adult_svl","adult_mass"))
squam.herps[,3:4]<-sapply(squam.herps[,3:4],as.numeric)
colnames(squam.herps)[colnames(squam.herps)!="Binomial"]<-
  paste("db4",colnames(squam.herps)[colnames(squam.herps)!="Binomial"],sep="_")

herps<-left_join(herps,squam.herps,by="Binomial")
rm(diet1,diet2,diet3,squam.herps,i)


#Atwood et al. 2020 data
#downloaded 31-Aug-2021 https://www.science.org/doi/10.1126/sciadv.abb8458
Atwood1<-read.csv("../abb8458_diet_data_set_s1.csv")
Atwood1<-subset(Atwood1,Atwood1$Class=="REPTILIA")
Atwood1$Binomial<-paste(Atwood1$Genus,Atwood1$Species,sep="_")
intersect(herps$Binomial,Atwood1$Binomial) #so many herps!
Atwood1<-Atwood1[,!colnames(Atwood1)%in%intersect(colnames(herps),colnames(Atwood1))[2:6]]
colnames(Atwood1)[colnames(Atwood1)!="Binomial"]<-
  paste("db5",colnames(Atwood1)[colnames(Atwood1)!="Binomial"],sep="_")
herps<-left_join(herps,Atwood1[,2:3],by="Binomial"); rm(Atwood1)


#Santini el al. 2017 data
#downloaded 01-Sep-2021 https://onlinelibrary.wiley.com/doi/full/10.1111/1749-4877.12268
Santini1<-read.csv("../inz212268-sup-0002-s1.csv")
Santini1<-subset(Santini1,!Santini1$Data_type%in%c("","Individual")) #only keep mean and range data types
colnames(Santini1)[colnames(Santini1)=="Species_ph"]<-"Binomial"
Santini1<-select(Santini1,c("Binomial","M_mean","SVL_mean","M_max","SVL_max",
                            "M_m_mean","M_m_max","M_f_mean","M_f_max","SVL_m_mean","SVL_m_max",
                            "SVL_f_mean","SVL_f_max","N"))
colnames(Santini1)[grep("M_",colnames(Santini1))]<-
  paste(colnames(Santini1)[grep("M_",colnames(Santini1))],"g",sep="_")
colnames(Santini1)[grep("SVL_",colnames(Santini1))]<-
  paste(colnames(Santini1)[grep("SVL_",colnames(Santini1))],"mm",sep="_")

Santini1<-subset(Santini1,Santini1$Binomial%in%herps$Binomial)
Santini2<-Santini1; Santini2[,2:26]<-NA
Santini2<-unique(Santini2); Santini2$N<-NULL

#select highest N value for each trait for each species
for(x in levels(as.factor(Santini1$Binomial))){ #for each species
  sub1<-subset(Santini1,Santini1$Binomial==x) #species-only data
  sub1<-sub1[,colSums(is.na(sub1))<nrow(sub1)]
  
  for(i in 2:(ncol(sub1)-1)){ #for each column (except Binomial and N)
    sub2<-sub1[,c("Binomial",colnames(sub1)[i],"N")] #select column
    sub2<-sub2[!is.na(sub2[,colnames(sub1)[i]]),] #keep non-NA values for focal trait
    #select value in column with highest N, save it in dataframe
    Santini2[Santini2$Binomial==sub1$Binomial[1],colnames(sub1)[i]]<- #find right row and column
      sub2[which(sub2$N==max(sub2$N))[1],2]}} #select value with highest N
colnames(Santini2)[colnames(Santini2)!="Binomial"]<-
  paste("db6",colnames(Santini2)[colnames(Santini2)!="Binomial"],sep="_")
herps<-left_join(herps,Santini2,by="Binomial")
rm(Santini1,Santini2,sub1,sub2,i,x)


##########################################################

#Start cleaning up dataframe
herps<-herps[,colSums(is.na(herps))<nrow(herps)] #remove rows of all NAs



body_size_colnames<-c( #amphibio
  "Body_mass_g","Body_size_mm","Size_at_maturity_min_mm", "Size_at_maturity_max_mm",
  #lizard_traits
  "maximum SVL","female SVL",
  #amniota
  "adult_body_mass_g","female_body_mass_g","male_body_mass_g","no_sex_body_mass_g",
  "adult_svl_cm","male_svl_cm","female_svl_cm","no_sex_svl_cm")

#which value to take for adult SVL?
herps[herps$Class=="Reptilia",c("Binomial","Order","adult_svl_cm","male_svl_cm",
                                "female_svl_cm","no_sex_svl_cm")]
#values for adult body mass for all
herps[herps$Class=="Reptilia",c("Binomial","Order","adult_body_mass_g",
                                "female_body_mass_g","male_body_mass_g","no_sex_body_mass_g")]

herps.1<-herps[,c("Binomial","Class","Order",body_size_colnames)]


#some kind of body size for all species
herps.1[rowSums(is.na(herps.1[,4:ncol(herps.1)]))==(ncol(herps)-3),] 
herps.1[rowSums(is.na(herps.1[,4:ncol(herps.1)]))<(ncol(herps)-3),] 

#note: SVL from lizard_traits is in mm
#herps<-herps %>% unite("body_length_mm",Body_size_mm,"female SVL", na.rm = TRUE)


colnames(herps)


