# library ----
library(here)
library(dplyr)
library(janitor)
library(visdat)

# import ----

phy_traits_raw <- read.csv(
  here("data-raw", "Trait_data.csv")
)

ciee_lpi <- read.csv(
  here("data-raw", "CIEE_LPI_dataset.csv")
)

# data clean ----

phy_traits_tidy <- phy_traits_raw %>%
  clean_names() %>%
  select(
    binomial_1_2, 
    mass_g, 
    diet_plant, 
    diet_vertebrate, 
    diet_invertebrate
  )

ciee_phy_traits <- ciee_lpi %>%
  inner_join(phy_traits_tidy, by = c("Binomial" = "binomial_1_2")) %>%
  select(
    Binomial, 
    mass_g, 
    diet_plant, 
    diet_vertebrate, 
    diet_invertebrate
  ) %>%
  filter(!duplicated(Binomial))

# check for missing trait values
vis_miss(ciee_phy_traits)

