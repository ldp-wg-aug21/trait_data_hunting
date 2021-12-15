# library ----
library(here)
library(janitor)
library(stringr)
library(dplyr)
library(visdat)

# import ----

# https://datadryad.org/stash/dataset/doi:10.5061/dryad.f6t39kj
meiri_raw <- read.csv(
  here("data-raw", "Appendix S1 - Lizard data version 1.0.csv"),
  stringsAsFactors = FALSE
)

ciee_lpi <- read.csv(
  here("data-raw", "CIEE_LPI_dataset.csv")
)

# data cleaning ----

meiri_tidy <- meiri_raw %>%
  clean_names() %>%
  mutate(binomial = str_replace(binomial, pattern = " ", replacement = "_")) %>%
  select(
    binomial,
    maximum_svl, 
    female_svl, 
    diet, 
    foraging_mode
  ) 

ciee_meiri_traits <- ciee_lpi %>%
  inner_join(meiri_tidy, by = c("Binomial" = "binomial")) %>%
  select(
    Binomial, 
    maximum_svl, 
    female_svl, 
    diet, 
    foraging_mode
  ) %>%
  filter(!duplicated(Binomial))


visdat::vis_miss(ciee_meiri_traits)



  
