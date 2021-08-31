# Extract bird traits 

# Install devtools if not available
# if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install traitdata package from Github
# remotes::install_github("RS-eco/traitdata")

# load packages
library(traitdata)
library(dplyr)


## Load the Canadian dataset

clpi <- read.csv("data-raw/CIEE_LPI_dataset.csv")

# subset to birds
sp_birds <- filter(lpd, Class %in% c("Aves", "Birds")) %>% summarise(sp = unique(Binomial)) 

# convert to a vector
sp_birds <- sp_birds$sp


## 1. Extract avian elton traits 

# load Elton Traits
data("elton_birds")

# extract data
clpi_eltonbirds <- elton_birds %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp_birds) %>%
  # subset to the traits we want
  subset(select = c(scientificNameStd, 
                    Diet.5Cat, 
                    BodyMass.Value)) 
 
# look at the dataset  
summary(clpi_eltonbirds)

# check which species are missing body mass data
clpi_eltonbirds[which(is.na(clpi_eltonbirds$Diet.5Cat)), "scientificNameStd"]
clpi_eltonbirds[which(is.na(clpi_eltonbirds$BodyMass.Value)), "scientificNameStd"]

colnames(clpi_eltonbirds)[1] <- "Binomial"


## 2. Extract avian body size 

# load avian body size data
data("AvianBodySize")

# extract data
clpi_aviansize <- AvianBodySize %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp_birds) %>%
  # subset to the traits we want
  subset(select = c(scientificNameStd, 
                    M_mass, 
                    F_mass)) %>%
  # change -999.0 to NAs
  mutate(M_mass = replace(M_mass, M_mass == -999.0, NA),
         F_mass = replace(F_mass, F_mass == -999.0, NA)) 
  # compute average for male and female mass
    
# look at the dataset  
summary(clpi_aviansize)

# check which species are missing body mass data
clpi_aviansize[which(is.na(clpi_aviansize$mass)), "scientificNameStd"]
colnames(clpi_aviansize)[1] <- "Binomial"


## Join to the larger clpi dataset

clpi_birdtraits <- clpi %>% 
                left_join(clpi_eltonbirds, by = "Binomial") %>%
                left_join(clpi_aviansize, by = "Binomial")

# change NAs to NULL
clpi_birdtraits[is.na(clpi_birdtraits)] <- "NULL"

head(clpi_birdtraits)

# convert diets to broader categories 
clpi_birdtraits$Diet.5Cat[clpi_birdtraits$Diet.5Cat %in% c("VertFishScav", "Invertebrate")] <- "Carnivore"
clpi_birdtraits$Diet.5Cat[clpi_birdtraits$Diet.5Cat %in% c("PlantSeed", "FruiNect")] <- "Herbivore"

# write to rds
saveRDS(clpi_birds, "data-clean/LPI_birds.rds")