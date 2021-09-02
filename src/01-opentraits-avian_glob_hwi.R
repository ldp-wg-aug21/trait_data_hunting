# libraries --------------------------------------------------------------------
library(here)
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)

# import -----------------------------------------------------------------------

# Sheard et al. 2020. Nature Communications. 
# https://zenodo.org/record/3832215#.YS4_TsZE1KM
hwi_raw <- read_excel(
  here("data-raw", "Dataset HWI 2020-04-10.xlsx"), 
  sheet = 1
)

# Canadian LPI database (from Robin Freeman)
ciee_lpi <- read.csv(
  here("data-raw", "CIEE_LPI_dataset.csv")
)

# List of Canadian species from the IUCN database
can_iucn <- read_excel(
  here(
    "data-raw", 
    "Wild Species 2015 Data - Espèces sauvages 2015 Données.xlsx"
    ), 
  sheet = "Trends - Tendances" 
)

# check packaging --------------------------------------------------------------

str(hwi_raw)
head(hwi_raw, n = 10)
tail(hwi_raw, n = 10)

# data cleaning ----------------------------------------------------------------

# select the relevant columns from the avian trait data set

# candidate traits: 
# (1) body mass, 
# (2) range size, 

# get relevant traits
hwi_tidy <- hwi_raw %>%
  janitor::clean_names() %>%
  select(
    binomial = iucn_name, # uses IUCN taxonomic names
    sample_size,
    body_mass_log,
    diet
  ) %>%
  mutate(across(.cols = everything(), na_if, "NA")) %>%
  mutate(body_mass_log = as.numeric(body_mass_log)) %>%
  mutate(binomial = str_replace(binomial, pattern = " ", replacement = "_")) %>%
  filter(!is.na(body_mass_log), !is.na(diet))

# merge relevant traits with Canadian LPI database
can_birds <- ciee_lpi %>%
  inner_join(hwi_tidy, by = c("Binomial" = "binomial")) %>%
  select(
    Binomial, 
    sample_size,
    body_mass_log,
    diet
  ) %>%
  filter(!duplicated(Binomial))

# cross-reference with Canadian IUCN Wild Species database ---------------------
can_iucn_tidy <- clean_names(can_iucn)
iucn_spp_list <- can_iucn_tidy %>%
  rename(binomial_2015 = scientific_name_nom_scientifique_2015) %>%
  mutate(binomial_2015 = str_replace(
    binomial_2015, 
    pattern = " ", 
    replacement = "_")
    ) %>%
  pull(binomial_2015) %>%
  unique() 

not_iucn <- can_birds$Binomial[!(can_birds$Binomial %in% iucn_spp_list)]

# check for missing values -----------------------------------------------------

visdat::vis_miss(can_birds)

# histograms of each trait -----------------------------------------------------

# remove that ugly gray background
theme_set(theme_bw())

# body mass
(can_birds %>%
    ggplot(aes(x = body_mass_log)) +
    geom_histogram() 
)

# diet 
(can_birds %>%
    ggplot(aes(x = diet)) +
    geom_bar() 
)

# save to disk -----------------------------------------------------------------

write.csv(
  x = ciee_avian_traits, 
  file = here("output", "ciee_avian_traits.csv")
)

