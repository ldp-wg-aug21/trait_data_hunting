# Get mammal data from Elton Traits

# Install devtools if not available
#if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install traitdata package from Github
#remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)

# load packages
library(traitdata)
library(dplyr)
# load the Canadian dataset
lpd <- read.csv("data-raw/CIEE_LPI_dataset.csv")
# subset to mammals
sp <- filter(lpd, Class == "Mammalia") %>% summarise(sp = unique(Binomial)) 
mammal_lpd <- filter(lpd, Class == "Mammalia")
# convert to a vector
sp <- sp$sp

# dataset related to mammals include:
# pantheria
# amniota
# elton_mammals
# mammal_diet - included in mammal_diet2
# mammal_diet2
# marsupials - none in Canada
# primates - none in Canada


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

# check if other datasets can fill in these gaps 

# # Load Elton Traits
# data("elton_mammals")

# # load dataset for trophic information
# data("mammal_diet2")

# mammal diet doesn't have more species
sp[which(!sp %in% gsub(" ", "_", mammal_diet2$scientificNameStd))]
sp[which(!sp %in% gsub(" ", "_", pantheria$scientificNameStd))]
sp[which(!sp %in% gsub(" ", "_", elton_mammals$scientificNameStd))]

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

# elton_mammals txt ---------------------------------------------------------------
# load elton raw dataset
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


elton2 <- elton2 %>% rename(elton_BodyMass.Value_g = BodyMass.Value)





# combine elton data from two sources
elton <- dplyr::union(elton1, elton2)


# join to the elton with pather and amnio dataset
lpd_traits <- left_join(elton, lpd_traits, by = "Binomial")

sp[which(!sp %in% gsub(" ", "_", lpd_traits$Binomial))]



# mammal_diet2 ---------------------------------------------------------------
# load amniota dataset
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


diet2 <- diet2 %>% rename(diet2_TrophicLevel = TrophicLevel)

str(diet2)
# 82/98 species located

# join to the lpd_traits dataset
lpd_traits <- left_join(lpd_traits, diet2, by = "Binomial")

# export full dataset
# write.csv(lpd_traits, "Mammals_traits.csv")

# write to rds
saveRDS(lpd_traits, "data-raw/Mammals_traits.rds")
