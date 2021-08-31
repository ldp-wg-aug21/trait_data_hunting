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

ciee_lpi <- read.csv(
  here("data-raw", "CIEE_LPI_dataset.csv")
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
ciee_avian_traits <- ciee_lpi %>%
  inner_join(hwi_tidy, by = c("Binomial" = "binomial")) %>%
  select(
    Binomial, 
    sample_size,
    body_mass_log,
    diet
  ) %>%
  filter(!duplicated(Binomial))

# how many bird species are in the Canadian LPI database?
# ciee_lpi %>% filter(Class %in% c("Aves", "Birds")) %>% nrow()

visdat::vis_miss(ciee_avian_traits)

# histograms of each trait -----------------------------------------------------

# remove that ugly gray background
theme_set(theme_bw())

# body mass
(ciee_avian_traits %>%
    ggplot(aes(x = body_mass_log)) +
    geom_histogram() 
)

# range_size 
(ciee_avian_traits %>%
    ggplot(aes(x = range_size)) +
    geom_histogram() 
)

# diet 
(ciee_avian_traits %>%
    ggplot(aes(x = diet)) +
    geom_bar() 
)

# save to disk -----------------------------------------------------------------

write.csv(
  x = ciee_avian_traits, 
  file = here("output", "ciee_avian_traits.csv")
)

