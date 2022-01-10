# Get mammal data from several trait databases
# databases: pantheria, amniota, elton_mammals, mammal_diet (and mammal_diet2)

# Install devtools if not available
#if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install traitdata package from Github
#remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)

# load packages
library(traitdata)
library(dplyr)

# load the Canadian dataset
lpd <- read.csv("data-raw/cLPI_data_resolved_species.csv")

# subset to mammals
sp <- filter(lpd, Class == "Mammals") %>% summarise(sp = unique(Binomial_resolved)) 
mammal_lpd <- filter(lpd, Class == "Mammals")

# rename Binomial_resolved column to Binomial (and remove the Binomial one) to
# make sure we are matching with the right names
mammal_lpd <- mammal_lpd %>% subset(select = -Binomial) %>%
  rename("Binomial" = "Binomial_resolved")

# convert to a vector
sp <- sp$sp

# Pantheria ---------------------------------------------------------------

# load pantheria dataset
data("pantheria")

# extract data
panth <- pantheria %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp) %>%
  # subset to the traits we want
  subset(select = c(scientificNameStd, 
                    AdultBodyMass_g, 
                    TrophicLevel, 
                    GestationLen_d,
                    MaxLongevity_m))
# look at the dataset  
summary(panth)

# check which species are missing data for each trait
panth[which(is.na(panth$AdultBodyMass_g)), "scientificNameStd"]
panth[which(is.na(panth$TrophicLevel)), "scientificNameStd"]
panth[which(is.na(panth$GestationLen_d)), "scientificNameStd"]
panth[which(is.na(panth$MaxLongevity_m)), "scientificNameStd"]
colnames(panth)[1] <- "Binomial"

# rename columns 
panth <- panth %>% rename(pantheria_AdultBodyMass_g = AdultBodyMass_g, 
              pantheria_TrophicLevel = TrophicLevel, 
              pantheria_GestationLen_d = GestationLen_d, 
              pantheria_MaxLongevity_m = MaxLongevity_m)
str(panth)
# 94/98 species located

# amniota ---------------------------------------------------------------

# load amniota dataset
data("amniota")

# extract data
amnio <- amniota %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp) %>%
  # subset to the traits we want
  subset(select = c(scientificNameStd, 
                    adult_body_mass_g, 
                    gestation_d,
                    maximum_longevity_y))
# look at the dataset  
summary(amnio)

# check which species are missing data for each trait
amnio[which(is.na(amnio$adult_body_mass_g)), "scientificNameStd"]
amnio[which(is.na(amnio$gestation_d)), "scientificNameStd"]
amnio[which(is.na(amnio$maximum_longevity_y)), "scientificNameStd"]
colnames(amnio)[1] <- "Binomial"

amnio <- amnio %>% rename(amniota_adult_body_mass_g = adult_body_mass_g, 
                    amniota_gestation_d = gestation_d, 
                    amniota_maximum_longevity_y = maximum_longevity_y)

str(amnio)
# 94/98 species located

# join to the panth dataset
lpd_traits <- left_join(panth, amnio, by = "Binomial")

# elton_mammals ---------------------------------------------------------------

# load elton_mammals dataset
data("elton_mammals")
str(elton_mammals)

elton_mam <- elton_mammals %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp) 

# extract data
elton1 <- elton_mam %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # subset to the traits we want
  select(scientificNameStd, BodyMass.Value, contains("Diet"), -Diet.Source, -Diet.Certainty)

# look at the dataset  
summary(elton1)

# check which species are missing data for each trait
elton1[which(is.na(elton1$BodyMass.Value)), "scientificNameStd"]
elton1[which(is.na(elton1$Diet.Inv)), "scientificNameStd"]
colnames(elton1)[1] <- "Binomial"
elton1 <- elton1 %>% rename(elton_BodyMass.Value_g = BodyMass.Value)

elton1 <- elton1 %>%
  group_by(Binomial) %>%
  # switch out species for _ in the species names to match the LPD
  mutate(Carnivores = sum(Diet.Inv, Diet.Vfish, Diet.Vend, Diet.Vect, Diet.Vunk, Diet.Scav),
         Herbivores = sum(Diet.Seed, Diet.Nect, Diet.PlantO, Diet.Fruit),
         Omnivores = (Carnivores-Herbivores)/100)
summary(elton1)

# assign a trophic level to each species 
elton1$Trophic_Level <- NA
for (i in (1:length(elton1$Binomial))){
  if(elton1$Omnivores[i] == "-1") {
    elton1$Trophic_Level[i] <- "Herbivores"
  }
  else if(elton1$Omnivores[i] == "1") {
    elton1$Trophic_Level[i] <- "Carnivores"
  }
  else{
    elton1$Trophic_Level[i] <- "Omnivores"
  }
}
elton1 <- elton1 %>%
  # subset to the traits we want
  select(Binomial, elton_BodyMass.Value_g, Trophic_Level)

# elton_mammals txt ---------------------------------------------------------------

# load elton raw dataset
# downloaded from: https://figshare.com/articles/dataset/Data_Paper_Data_Paper/3559887?backTo=/collections/EltonTraits_1_0_Species-level_foraging_attributes_of_the_world_s_birds_and_mammals/3306933
elton_mammals2 <- read.delim("data-raw/MamFuncDat.txt")

elton_mam2 <- elton_mammals2 %>%
  # switch out species for _ in the species names to match the LPD
  mutate(Scientific = gsub(" ", "_", Scientific)) %>%
  # filter for species who are in the LPD dataset
  filter(Scientific %in% sp) 

# extract data
elton2 <- elton_mam2 %>%
  # switch out species for _ in the species names to match the LPD
  mutate(Scientific = gsub(" ", "_", Scientific)) %>%
  # subset to the traits we want
  select(Scientific, BodyMass.Value, contains("Diet"), -Diet.Source, -Diet.Certainty)

# look at the dataset  
summary(elton2)
# 97/98 species located

# check which species are missing data for each trait
elton2[which(is.na(elton2$BodyMass.Value)), "scientificNameStd"]
elton2[which(is.na(elton2$Diet.Inv)), "scientificNameStd"]
colnames(elton2)[1] <- "Binomial"

# rename columns
elton2 <- elton2 %>% rename(elton_BodyMass.Value_g = BodyMass.Value)

elton2 <- elton2 %>%
  group_by(Binomial) %>%
  # switch out species for _ in the species names to match the LPD
  mutate(Carnivores = sum(Diet.Inv, Diet.Vfish, Diet.Vend, Diet.Vect, Diet.Vunk, Diet.Scav),
         Herbivores = sum(Diet.Seed, Diet.Nect, Diet.PlantO, Diet.Fruit),
         Omnivores = (Carnivores-Herbivores)/100)
summary(elton2)

# assign a trophic level to each species
elton2$Trophic_Level <- NA
for (i in (1:length(elton2$Binomial))){
  if(elton2$Omnivores[i] == "-1") {
    elton2$Trophic_Level[i] <- "Herbivores"
  }
  else if(elton2$Omnivores[i] == "1") {
    elton2$Trophic_Level[i] <- "Carnivores"
  }
  else {
    elton2$Trophic_Level[i] <- "Omnivores"
  }
}

elton2 <- elton2 %>%
 # subset to the traits we want
  select(Binomial, elton_BodyMass.Value_g, Trophic_Level)


# combine elton data from two sources
elton <- dplyr::union(elton1, elton2)

# join datasets
lpd_traits <- left_join(elton, lpd_traits, by = "Binomial")

# mammal_diet2 ---------------------------------------------------------------

# load dataset
data("mammal_diet2")
str(mammal_diet2)

# extract data
diet2 <- mammal_diet2 %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp) %>%
  # subset to the traits we want
  select(scientificNameStd, TrophicLevel)

# look at the dataset  
summary(diet2)

# check which species are missing data for each trait
diet2[which(is.na(diet2$scientificNameStd)), "scientificNameStd"]
diet2[which(is.na(diet2$TrophicLevel)), "scientificNameStd"]
colnames(diet2)[1] <- "Binomial"

# rename columns
diet2 <- diet2 %>% rename(diet2_TrophicLevel = TrophicLevel)

str(diet2)
# 84/98 species located

# join to the lpd_traits dataset
lpd_traits <- left_join(lpd_traits, diet2, by = "Binomial")

# Select the traits needed
# Trophic data is calculated from Elton Mammal
# Body mass is derived from Elton Mammal
# Gestation Period and Max Longivity is from Amniota
lpd_traits_mammal_clean <- dplyr::select(lpd_traits, 
                                         Binomial, 
                                         elton_BodyMass.Value_g, 
                                         Trophic_Level, 
                                         amniota_gestation_d, 
                                         amniota_maximum_longevity_y)
summary(lpd_traits_mammal_clean)

# join to the larger lpd dataset
lpd_traits_mammal <- left_join(lpd, lpd_traits_mammal_clean, by = "Binomial")

# remove duplicates
lpd_traits_mammal_clean <- distinct(lpd_traits_mammal_clean)

# write to csv
write.csv(lpd_traits_mammal_clean, "data-clean/traits-specific-mammals.csv")
# write to rds
saveRDS(lpd_traits_mammal_clean, "data-clean/traits-specific-mammals.rds")



# Wild Species QCQA -------------------------------------------------------------

# get list of mammals on the Wild Species List
Wild <- read.csv("data-raw/WildSpecies2015Data.csv")

# extract mammals
unique(Wild$TAXONOMIC.GROUP...GROUPE.TAXONOMIQUE)
WildMammal <- dplyr::filter(Wild, Wild$TAXONOMIC.GROUP...GROUPE.TAXONOMIQUE == "Mammals - Mammif\xe8res" )
WildMammal$Wild <- "Wild"
WildMammal <- dplyr::select(WildMammal, c("Binomial","Wild"))

# remove duplicated Binomials
WildMammal <- distinct(WildMammal)
WildMammal <- WildMammal[!duplicated(WildMammal$Binomial),]

# join to traits dataset
lpd_traits_wild <- left_join(WildMammal, lpd_traits, by = "Binomial") %>% distinct()
lpd_traits_wild <- lpd_traits_wild[!duplicated(lpd_traits_wild$Binomial),]
write.csv(lpd_traits_wild, "data-clean/WildSpecies_mammals_traits.csv")
