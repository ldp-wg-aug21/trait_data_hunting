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
herps<-unique(lpi.dat[lpi.dat$Class%in%c("Amphibia","Reptilia","Herps"),c("Binomial","Order","Family","Genus","Species")])
herps$Class<-NA
herps$Class[herps$Order%in%c("Testudines","Squamata")]<-"Reptilia"
herps$Class[herps$Order%in%c("Caudata","Anura","Urodela")]<-"Amphibia"
herps<-herps[herps$Species!="ambystoma",]

#wild data only
wild.dat<-read.csv("data-raw/WildSpecies2015Data.csv")
levels(as.factor(wild.dat$TAXONOMIC.GROUP...GROUPE.TAXONOMIQUE))
wild.dat<-subset(wild.dat,wild.dat$TAXONOMIC.GROUP...GROUPE.TAXONOMIQUE%in%
                   c("Amphibians - Amphibiens","Reptiles - Reptiles"))
colnames(wild.dat)<-c("code","Class","Order","Family","scientific_name",
                      "Binomial","common_name","CA")



data("amphibio")
#make Binomial column with same formatting and extract relevant data
amphibio$Binomial<-paste(amphibio$Genus,amphibio$Species,sep="_")
#remove intersecting cols to avoid issue with NAs in amphibio
amphibio<-amphibio[,!colnames(amphibio)%in%intersect(colnames(herps),colnames(amphibio))[2:5]]

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
wild.dat<-left_join(wild.dat,amphibio,by="Binomial",na.omit=T)

rm(amphibio)



data("amniota")
amniota<-amniota[amniota$Class=="Reptilia",] #only keep reptiles
amniota$Binomial<-gsub(" ","_",amniota$scientificNameStd)
intersect(herps$Binomial,amniota$Binomial) #so many herps!
amniota<-amniota[,!colnames(amniota)%in%c(intersect(colnames(herps),colnames(amniota))[2:7],
                                          "common_name","Subspecies","scientificNameStd")]
amniota[,grep("_cm",colnames(amniota))]<-amniota[,grep("_cm",colnames(amniota))]*10
colnames(amniota)<-gsub("_cm","_mm",colnames(amniota))
colnames(amniota)[colnames(amniota)!="Binomial"]<-
  paste("db2",colnames(amniota)[colnames(amniota)!="Binomial"],sep="_")
herps<-left_join(herps,amniota,by="Binomial")
wild.dat<-left_join(wild.dat,amniota,by="Binomial")
rm(amniota)




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
  paste("db3",colnames(lizard_traits)[colnames(lizard_traits)!="Binomial"],sep="_")
herps<-left_join(herps,lizard_traits,by="Binomial")
wild.dat<-left_join(wild.dat,lizard_traits,by="Binomial")
rm(lizard_traits)



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
  paste("db4",colnames(Santini2)[colnames(Santini2)!="Binomial"],sep="_")
herps<-left_join(herps,Santini2,by="Binomial")
wild.dat<-left_join(wild.dat,Santini2,by="Binomial")
rm(Santini1,Santini2,sub1,sub2,i,x)




#Atwood et al. 2020 data
#downloaded 31-Aug-2021 https://www.science.org/doi/10.1126/sciadv.abb8458
Atwood1<-read.csv("../abb8458_diet_data_set_s1.csv")
Atwood1<-subset(Atwood1,Atwood1$Class=="REPTILIA")
Atwood1$Binomial<-paste(Atwood1$Genus,Atwood1$Species,sep="_")
intersect(herps$Binomial,Atwood1$Binomial) #so many herps!
Atwood1<-Atwood1[,!colnames(Atwood1)%in%intersect(colnames(herps),colnames(Atwood1))[2:6]]
colnames(Atwood1)[colnames(Atwood1)!="Binomial"]<-
  paste("db5",colnames(Atwood1)[colnames(Atwood1)!="Binomial"],sep="_")
herps<-left_join(herps,Atwood1[,2:3],by="Binomial")
wild.dat<-left_join(wild.dat,Atwood1[,2:3],by="Binomial")
rm(Atwood1)

####NEXT: add in this index
#https://www.species360.org/serving-conservation/species-knowledge-index/
"Demographic_Database.csv" 



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
                "adult_svl_mm"]<-mean(diet3$predator_svl,na.rm=T)
    squam.herps[squam.herps$Species_temp==squam.herps$Species_temp[i],
                "adult_mass_g"]<-mean(diet3$predator_mass,na.rm=T)}}

squam.herps<-select(squam.herps,c("Binomial","prop.animal.diet","adult_svl_mm","adult_mass_g"))
squam.herps[,3:4]<-sapply(squam.herps[,3:4],as.numeric)

squam.herps$diet[squam.herps$prop.animal.diet==1]<-"carnivore"
colnames(squam.herps)[colnames(squam.herps)!="Binomial"]<-
  paste("db6",colnames(squam.herps)[colnames(squam.herps)!="Binomial"],sep="_")

herps<-left_join(herps,squam.herps,by="Binomial")
wild.dat<-left_join(wild.dat,squam.herps,by="Binomial")
rm(diet1,diet2,diet3,squam.herps,i)





##########################################################

#Start cleaning up dataframe
herps<-herps[,colSums(is.na(herps))<nrow(herps)] #remove rows of all NAs
herps[herps=="NaN"]<-NA
wild.dat<-wild.dat[,colSums(is.na(wild.dat))<nrow(wild.dat)] #remove rows of all NAs
wild.dat[wild.dat=="NaN"]<-NA


#function to merge columns on same scale
merge_trait_cols<-function(df.1,new_col_name,old_cols){
  df.1<-unite(df.1,new_col,old_cols,na.rm=T)
  df.1[,"new_col"]<-as.vector(t(data.frame(str_split(df.1[,"new_col"],"_"))[1,]))
  colnames(df.1)[colnames(df.1)=="new_col"]<-new_col_name
  df.1}

View(herps[,c("Species","Order",longevity_cols)])


#hierarchical inclusion of data based on order in cols vectors
##ex- if no data in db1_Body_mass_g, will save data from db2_adult_body_mass_g
##otherwise if there is data from db1_Body_mass_g other body mass info deleted
make_trait_df<-function(df){
body_mass_cols<-c("db1_Body_mass_g","db2_adult_body_mass_g",
                  "db4_M_mean_g","db6_adult_mass_g")
SVL_cols<-c("db1_Body_size_mm","db2_adult_svl_mm","db4_SVL_mean_mm","db6_adult_svl_mm")

longevity_cols<-c("db1_Longevity_max_y","db2_maximum_longevity_y")

diet_cols<-c("db1_Diet","db3_diet","db5_Trophic_Level_10","db6_diet")

#get mean of max and min clutch sizes to get general clutch size estimate for db1
df$db1_litter_size_n<-(as.numeric(df$db1_Litter_size_max_n)+as.numeric(df$db1_Litter_size_min_n))/2
clutch_size_cols<-c("db2_litter_or_clutch_size_n","db3_clutch size_n","db1_litter_size_n")

df$db1_offspring_size_mm<-(df$db1_Offspring_size_min_mm+df$db1_Offspring_size_max_mm)/2
offspring_size_cols<-c("db2_birth_or_hatching_svl_mm","db3_hatchling/neonate SVL_mm","db1_offspring_size_mm")

df$db1_Age_at_maturity_y<-(df$db1_Age_at_maturity_max_y+df$db1_Age_at_maturity_min_y)/2
df$db2_maturity_y<-((df$db2_male_maturity_d+df$db2_female_maturity_d)/2)/365
age_maturity_cols<-c("db1_Age_at_maturity_y","db2_maturity_y")

df.1<-df[,c("Binomial","Class","Order","Family",body_mass_cols,SVL_cols,longevity_cols,
                  diet_cols,clutch_size_cols,offspring_size_cols,age_maturity_cols)]


df.1<-merge_trait_cols(df.1,"body_mass_g",body_mass_cols)
df.1<-merge_trait_cols(df.1,"SVL_mm",SVL_cols)
df.1<-merge_trait_cols(df.1,"longevity_years",longevity_cols)
df.1<-merge_trait_cols(df.1,"diet",diet_cols)
df.1$diet[df.1$diet%in%c("Carnivorous","Predator")]<-"carnivore"
df.1$diet[df.1$diet%in%c("Omnivore")]<-"omnivore"
df.1<-merge_trait_cols(df.1,"clutch_size_n",clutch_size_cols)
df.1<-merge_trait_cols(df.1,"offspring_size_mm",offspring_size_cols)
df.1<-merge_trait_cols(df.1,"age_maturity_years",age_maturity_cols)
df.1}

herps.1<-make_trait_df(herps)
wild.dat<-make_trait_df(wild.dat)
#View(herps.2)
herps.2<-herps.1[,c(1:5,8,7)]
colnames(herps.2)[5:7]<-c("BodySize","TrophicLevel","LifeSpan")
# write.csv(herps.2,file="data-clean/herps_traits_CLPI_subset.csv")
# write.csv(herps.1,file="data-clean/herps_traits_CLPI.csv")
rm(herps.2)


# write.csv(wild.dat,"data-clean/herps_traits_allcanadiansp.csv")

