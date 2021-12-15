# library ----
library(here)
library(dplyr)
library(visdat)

# import ----

amniote_raw <- read.csv(
  here("data-raw", "Amniote_Database_Aug_2015.csv"),
  stringsAsFactors = FALSE
)

ciee_lpi <- read.csv(
  here("data-raw", "CIEE_LPI_dataset.csv")
)

# data cleaning ----

# get relevant traits
amniote_tidy <- amniote_raw %>%
  mutate(across(.cols = everything(), na_if, "-999")) %>%
  mutate(binomial = paste(genus, species, sep = "_")) %>%
  select(
    class,
    binomial, 
    adult_body_mass_g,
    maximum_longevity_y,
    longevity_y,
    female_body_mass_g,
    male_body_mass_g,
    fledging_mass_g,
    adult_svl_cm,
    male_svl_cm,
   female_svl_cm
  )

# merge with canadian lpi database
ciee_amniote_traits <- ciee_lpi %>% 
  inner_join(amniote_tidy, by = c("Binomial" = "binomial")) %>%
  select(
    Binomial, 
    adult_body_mass_g,
    maximum_longevity_y,
    longevity_y,
    female_body_mass_g,
    male_body_mass_g,
    fledging_mass_g,
    adult_svl_cm,
    male_svl_cm,
    female_svl_cm
  ) %>%
  filter(!duplicated(Binomial))

# check for missing values
visdat::vis_miss(ciee_amniote_traits)  


  