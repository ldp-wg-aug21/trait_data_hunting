# libraries --------------------------------------------------------------------
library(here)
library(janitor)
library(dplyr)
library(visdat)

# import -----------------------------------------------------------------------

anage_raw <- read.table(
  here("data-raw", "anage_data.txt"), 
  sep = "\t",
  stringsAsFactors = FALSE, 
  header = TRUE
)

ciee_lpi <- read.csv(
  here("data-raw", "CIEE_LPI_dataset.csv")
)

# data clean -------------------------------------------------------------------

# collect relevant traits from the anage dataset
anage_tidy <- anage_raw %>%
  clean_names() %>%
  mutate(spp_code = paste(genus, species, sep = "_")) %>%
  select(
    spp_code, 
    body_mass_g, 
    maximum_longevity_yrs, 
    metabolic_rate_w, 
    specimen_origin, 
    data_quality
  )

ciee_anage_traits <- ciee_lpi %>%
  inner_join(anage_tidy, by = c("Binomial" = "spp_code")) %>%
  select(
    Binomial, 
    body_mass_g, 
    maximum_longevity_yrs, 
    metabolic_rate_w, 
    specimen_origin, 
    data_quality
  )

# note: there are missing traits values
vis_miss(ciee_anage_traits)


