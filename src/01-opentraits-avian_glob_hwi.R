# libraries --------------------------------------------------------------------
library(here)
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)

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


# data cleaning ----------------------------------------------------------------

# select the relevant columns from the avian trait data set

# candidate traits: 
# (1) hand wing index: hwi, 
# (2) body mass, 
# (3) range size, 
# (4) diet

# note: there are some missing trait values 

hwi_tidy <- hwi_raw %>%
  janitor::clean_names() %>%
  select(
    binomial = tree_name, # uses IUCN taxonomic names
    hwi,
    sample_size,
    body_mass_log,
    range_size, 
    diet
  ) %>%
  filter(binomial != "NA") %>%
  mutate(
    body_mass_log = as.numeric(body_mass_log),
    range_size = as.numeric(range_size)
  ) 

# merge relevant traits with Canadian LPI database
ciee_avian_traits <- ciee_lpi %>%
  inner_join(hwi_tidy, by = c("Binomial" = "binomial")) %>%
  select(
    binomial, 
    hwi,
    sample_size,
    body_mass_log,
    range_size, 
    diet
  )

# how many bird species are in the Canadian LPI database?
# ciee_lpi %>% filter(Class %in% c("Aves", "Birds")) %>% nrow()

# histograms of each trait -----------------------------------------------------

# remove that ugly gray background
theme_set(theme_bw())

# hwi  
(ciee_avian_traits %>%
  filter(!is.na(hwi)) %>%
  ggplot(aes(x = hwi)) +
    geom_histogram() 
)

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

