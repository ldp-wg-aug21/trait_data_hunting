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

## Load the Amniota dataset ----------------------------------------------------

data("amniota")

## Load the Canadian dataset ---------------------------------------------------

clpi <- read.csv("data-raw/CIEE_LPI_dataset.csv")

## Load the IUCN dataset -------------------------------------------------------

iucn <- read.csv(
  here("data-raw", "WildSpecies2015Data.csv")
)

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

vis_miss(hwi_tidy)

# Amniote dataset --------------------------------------------------------------

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
  ) %>%
  mutate(across(.cols = everything(), na_if, "NaN"))

vis_miss(amniota_summ)

# Build the global bird trait dataset ------------------------------------------

# Left join with Sheard et al. 2020 since that dataset has more species 
# than amniota 
merge_tidy <- hwi_tidy %>%
  left_join(amniota_summ, by = "binomial")

vis_miss(merge_tidy)

# Build with LPI dataset -------------------------------------------------------

# subset to birds
clpi_birds <- clpi %>%
  filter(Class %in% c("Aves", "Birds")) %>%
  inner_join(merge_tidy, by = c("Binomial" = "binomial")) %>%
  select(
    Binomial, 
    hwi, 
    range_size, 
    diet, 
    mean_adult_body_mass_g, 
    mean_max_longevity_y, 
    mean_longevity_y
  ) %>%
  filter(!duplicated(Binomial)) %>%
  filter(Binomial != "NA")

# check for missing values 
vis_miss(clpi_birds)

# Build IUCN dataset -----------------------------------------------------------

iucn_birds <- iucn %>%
  inner_join(merge_tidy, by = c("Binomial" = "binomial")) %>%
  select(
    Binomial, 
    hwi, 
    range_size, 
    diet, 
    mean_adult_body_mass_g, 
    mean_max_longevity_y, 
    mean_longevity_y
  ) %>%
  filter(!duplicated(Binomial)) %>%
  filter(Binomial != "NA")


