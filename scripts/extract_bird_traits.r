# Extract bird traits 

# Install devtools if not available
# if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install traitdata package from Github
# remotes::install_github("RS-eco/traitdata")

# load packages
library(traitdata)
library(dplyr)
library(readxl)
library(here)
library(janitor)
library(stringr)
library(visdat)

## Load the Sheard et al. 2020 dataset ------------------------------------------

# Sheard et al. 2020. Nature Communications. 
# https://zenodo.org/record/3832215#.YS4_TsZE1KM

# read_excel tries to automically parse the dataset
# but there are some warnings for parsing
# mostly because the authors used string characters for NA values
hwi_raw <- read_excel(
  here("data-raw", "heard-et-al_2020_hwi_2020.xlsx"), 
  sheet = 1
)

## Load the Canadian dataset ---------------------------------------------------

clpi <- read.csv("data-raw/CIEE_LPI_dataset.csv")

# subset to birds
sp_birds <- filter(clpi, Class %in% c("Aves", "Birds")) %>% summarise(sp = unique(Binomial)) 

# convert to a vector
sp_birds <- sp_birds$sp

# Elton dataset ----------------------------------------------------------------

## 1. Extract avian elton traits 

# load Elton Traits
data("elton_birds")

# extract data
elton_tidy <- elton_birds %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  select(
    binomial = scientificNameStd,   
    body_mass = BodyMass.Value
    ) %>%
  filter(!duplicated(binomial))

# Heard et al. 2020 dataset ----------------------------------------------------

# select the relevant columns from the avian trait data set

# candidate traits: 
# (1) body mass, 
# (2) range size, 
# (3) hang-wind index (a proxy for dispersal ability)
# (4) diet

# traits were previously collected from previous databases: 
# (1) body mass - Tobias and Pigot (2019) and Dunning (2007)
# (2) range size - IUCN Red List for Birds
# (3) hang-wing index - 45k adult live and museum specimens
# (4) diet - Pigot et al. 2020 and EltonTraits

hwi_tidy <- hwi_raw %>%
  janitor::clean_names() %>%
  select(
    binomial = species_name, 
    hwi, 
    range_size, 
    diet
  ) %>%
  mutate(across(.cols = everything(), na_if, "NA")) %>%
  mutate(range_size = as.numeric(range_size)) %>%
  mutate(binomial = str_replace(binomial, pattern = " ", replacement = "_")) %>%
  filter(!is.na(binomial))

# Amniote dataset --------------------------------------------------------------

data("amniota")

amniota_tidy <- amniota %>%
  filter(Class == "Aves") %>%
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  select(
    binomial = scientificNameStd,
    adult_body_mass_g, 
    maximum_longevity_y, 
    longevity_y,
  ) 
  
amniota_summ <- amniota_tidy %>%
  filter(!is.na(binomial)) %>%
  group_by(binomial) %>%
  summarize(
    mean_adult_body_mass_g = mean(adult_body_mass_g, na.rm = TRUE),
    mean_max_longevity_y = mean(maximum_longevity_y, na.rm = TRUE),
    mean_longevity_y = mean(longevity_y, na.rm = TRUE), 
    sample_size = n()
  ) %>%
  mutate(across(.cols = everything(), na_if, "NaN"))


# Merge ----

merge_tidy <- hwi_tidy %>%
  left_join(amniota_tidy, by = "binomial")

# Check for missing values -----------------------------------------------------

vis_miss(merge_tidy)

  
# Other stuf -------------------------------------------------------------------

## 2. Extract avian body size 

# load avian body size data
#data("AvianBodySize")

# extract data
#clpi_aviansize <- AvianBodySize %>%
  # switch out species for _ in the species names to match the LPD
#  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
#  filter(scientificNameStd %in% sp_birds) %>%
  # subset to the traits we want
#  subset(select = c(scientificNameStd, 
#                    M_mass, 
#                    F_mass)) %>%
  # change -999.0 to NAs
#  mutate(M_mass = replace(M_mass, M_mass == -999.0, NA),
#         F_mass = replace(F_mass, F_mass == -999.0, NA)) 
# compute average for male and female mass

# look at the dataset  
# summary(clpi_aviansize)

# check which species are missing body mass data
# clpi_aviansize[which(is.na(clpi_aviansize$mass)), "scientificNameStd"]
# colnames(clpi_aviansize)[1] <- "Binomial"


## Join to the larger clpi dataset

# clpi_birdtraits <- clpi %>% 
#  left_join(clpi_eltonbirds, by = "Binomial") %>%
#  left_join(clpi_aviansize, by = "Binomial")

# change NAs to NULL
# clpi_birdtraits[is.na(clpi_birdtraits)] <- "NULL"

# head(clpi_birdtraits)

# convert diets to broader categories 
# clpi_birdtraits$Diet.5Cat[clpi_birdtraits$Diet.5Cat %in% c("VertFishScav", "Invertebrate")] <- "Carnivore"
# clpi_birdtraits$Diet.5Cat[clpi_birdtraits$Diet.5Cat %in% c("PlantSeed", "FruiNect")] <- "Herbivore"

# write to rds
# saveRDS(clpi_birds, "data-clean/LPI_birds.rds")

# merge relevant traits with Canadian LPI database
# clpi_hwi <- clpi %>%
#  inner_join(hwi_tidy, by = c("Binomial" = "binomial")) %>%
#  select(
#    Binomial, 
#    sample_size,
#    body_mass_log,
#    diet
#  ) %>%
#  filter(!duplicated(Binomial))

