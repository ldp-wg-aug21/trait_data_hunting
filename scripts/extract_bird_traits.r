# Extract bird traits 
# Code developed by Garland Xie and Francis Banville 

# Install devtools if not available
# if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install traitdata package from Github
# remotes::install_github("RS-eco/traitdata")

# load packages ----------------------------------------------------------------
library(traitdata)    # to extract trait data from many open data-sets
library(dplyr)        # to manipulate data 
library(readxl)       # to import excel spreadsheets
library(janitor)      # to clean column names into a machine-readable format
library(stringr)      # to manipulate string characters
library(visdat)       # to visualize missing data 
library(patchwork)    # to create multi-panel figures
library(ggplot2)      # to visualize adata
library(here)         # to create relative file-paths
library(tidyr)        # to create wide or long data frames 

# Import data ------------------------------------------------------------------

## Load the Sheard et al. 2020 dataset ------------------------------------------

# Sheard et al. 2020. Nature Communications. 
# https://zenodo.org/record/3832215#.YS4_TsZE1KM

# read_excel tries to automically parse the dataset
# but there are some warnings for parsing
# mostly because the authors used string characters for NA values
hwi_raw <- read_excel(
  "data-raw/heard-et-al_2020_hwi_2020.xlsx", 
  sheet = 1
)

## Load the Amniota dataset ----------------------------------------------------

# from traitdata R package 
data("amniota")

## Load the Canadian dataset ---------------------------------------------------

clpi <- read.csv("data-raw/cLPI_data_resolved_species.csv")

## Load the IUCN dataset -------------------------------------------------------

iucn <- read.csv("data-raw/WildSpecies2015Data.csv")

## Load SOCB dataset -----------------------------------------------------------

# SOCB = State of Canada's Birds (2019 report)
# http://nabci.net/resources/state-of-canadas-birds-2019/

socb <- read_excel(
  here("data-raw", "SOCB-Data-Sources_Source-de-donnees-EPOC-1.xlsx")
)


# Data cleaning (for separate datasets) ----------------------------------------

## Data cleaning: State of Canada's Birds --------------------------------------

socb_wide <- socb %>%
  clean_names() %>%
  rename(binomial = scientific_name_nom_scientifique) %>%
  mutate(binomial = str_replace(binomial, pattern = " ", replacement = "_")) %>%
  select(
    binomial, 
    waterfowl                      = waterfowl_sauvagine,
    waterfowl_goose                = waterfowl_and_wetland_birds_goose_species_sauvagine_et_oiseaux_de_milieux_humides_especes_doies,
    waterfowl_duck                 = waterfowl_and_wetland_birds_duck_species_sauvagine_et_oiseaux_de_milieux_humides_especes_de_canards,
    birds_prey                     = birds_of_prey_oiseaux_de_proie, 
    wetlands                       = wetland_birds_oiseaux_de_milieux_humides, 
    seabirds                       = seabirds_includes_all_58_seabird_species_all_species_other_than_those_with_the_data_source_indicating_no_acceptable_data_for_indicator_are_included_in_the_canadian_nesting_seabirds_indicator_oiseaux_de_mer_comprend_les_58_especes_doiseaux_de_mer_toutes_les_especes_sauf_celles_qui_ont_comme_source_de_donnees_aucune_donnee_acceptable_pour_lindicateur_sont_incluses_dans_lindicateur_des_oiseaux_de_mer_nichant_au_canada,
    shore                          = shorebirds_oiseaux_de_rivage, 
    shore_long                     = shorebirds_long_distance_migrants_oiseaux_de_rivage_migrant_sur_de_grandes_distances,
    shore_short                    = shorebirds_short_distance_migrants_oiseaux_de_rivage_migrant_sur_de_courtes_distances,
    grasslands                     = grassland_birds_oiseaux_de_prairie, 
    grasslands_native              = grassland_birds_species_dependent_on_native_grassland_oiseaux_de_prairie_especes_dependantes_des_prairies_indigenes,
    grasslands_agri                = grassland_birds_species_tolerant_of_agriculture_oiseaux_de_prairie_especes_tolerantes_des_paysages_agricoles,
    aerial_insect                  = aerial_insectivores_insectivores_aeriens, 
    forest_bird                    = forest_birds_oiseaux_forestiers,
    forest_SA                      = forest_birds_species_wintering_in_south_america_oiseaux_forestiers_especes_hivernant_en_amerique_du_sud,
    forest_CA                      = forest_birds_species_wintering_in_canada_oiseaux_forestiers_especes_hivernant_au_canada,
    forest_crop                    = forest_birds_forest_crop_specialists_oiseaux_forestiers_specialistes_de_graines_et_de_fruits_darbres,
    other                          = all_other_birds_tous_les_autres_oiseaux
  ) %>%
  pivot_longer(
    cols = c(
      "waterfowl", 
      "waterfowl_goose", 
      "waterfowl_duck", 
      "birds_prey", 
      "wetlands", 
      "seabirds", 
      "shore", 
      "shore_long",
      "shore_short", 
      "grasslands", 
      "grasslands_native", 
      "grasslands_agri",
      "aerial_insect",
      "forest_bird",
      "forest_SA",
      "forest_CA",
      "forest_crop", 
      "other"
      )
  ) %>%
  
  mutate(name = case_when(
    
    name == "waterfowl_goose"   ~ "waterfowl", 
    name == "waterfowl_duck"    ~ "waterfowl", 
    
    name == "grasslands_agri"   ~ "grasslands",
    name == "grasslands_native" ~ "grasslands",
    
    name == "shore_long"        ~ "shore",
    name == "shore_short"       ~ "shore",
    
    name == "forest_bird"        ~ "forest",
    name == "forest_crop"       ~ "forest", 
    name == "forest_SA"         ~ "forest",
    name == "forest_CA"         ~ "forest",
    TRUE ~ name)
    ) %>%
    
  filter(!is.na(value)) %>%
  
  group_by(binomial) %>%
  summarize(diet_guilds = list(name)) %>%
  ungroup() 

# create multiple functional groups for a given bird species (if necessary)
create_func_groups <- function(ls) {
  
  vec_bird_groups <- unlist(ls)
  
  # unit tests
  # example 1 - c("shore", "shore")
  # example 2 - c("grasslands", "grasslands")
  if(length(unique(vec_bird_groups)) == 1) { 
    
    bird_group <- unlist(vec_bird_groups)
    bird_group <- unique(bird_group)
    
    # unit tests
    # example 1 - c("shore", "shore", "grasslands", "grasslands")
    # example 2 - c("shore", "shore", "grasslands")
    # example 3 - c("wetlands", "seabirds")
  } else if(length(unique(vec_bird_groups) >= 2)) {
    
    bird_group <- unlist(vec_bird_groups)
    bird_group <- unique(bird_group)
    bird_group <- paste(bird_group, collapse = "+")
    
    # example 1 - "forest"
    # example 2 - "shore
  } else {
    
    bird_group <- vec_bird_groups
  }
  
  return(bird_group)
  
} 

socb_wide2 <- socb_wide %>%
  mutate(func_groups = sapply(diet_guilds, create_func_groups)) %>%
  select(binomial, func_groups)

# sanity check
vis_miss(socb_wide2)

## Data cleaning:  Heard et al. 2020 -------------------------------------------

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

# check for missing values 
vis_miss(hwi_tidy)

## Data cleaning: Amniote dataset ----------------------------------------------

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

# check for missing values 
vis_miss(amniota_summ)

# Data cleaning: compiling different datasets ----------------------------------

## Build the global bird trait dataset -----------------------------------------

# Left join with Sheard et al. 2020 
# since that dataset has more species than
# (1) amniota 

glob_birds <- hwi_tidy %>%
  left_join(amniota_summ, by = "binomial")

# check for missing values 
vis_miss(glob_birds)


## Build with C-LPI dataset ------------------------------------------------------

# subset to birds
clpi_birds <- clpi %>%
  filter(Class %in% c("Aves", "Birds")) %>%
  inner_join(glob_birds, by = c("Binomial" = "binomial")) %>%
  select(
    Binomial = Binomial_resolved, 
    hwi, 
    range_size, 
    diet, 
    mean_adult_body_mass_g, 
    mean_max_longevity_y, 
    mean_longevity_y
  ) %>%
  filter(!duplicated(Binomial)) %>%
  filter(Binomial != "NA") %>%
  left_join(socb_wide2, by = c("Binomial" = "binomial")) 

# check for missing values 
vis_miss(clpi_birds)

## Build IUCN dataset -----------------------------------------------------------

iucn_birds <- iucn %>%
  inner_join(glob_birds, by = c("Binomial" = "binomial")) %>%
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

# fix taxonomic synonyms so they match with SOCB dataset
iucn_birds <- iucn_birds %>%
  mutate(Binomial = case_when(
    Binomial == "Anas_americana"    ~ "Mareca_americana",
    Binomial == "Anas_clypeata"     ~ "Spatula_clypeata", 
    Binomial == "Anas_cyanoptera"   ~ "Spatula_cyanoptera",
    Binomial == "Anas_discors"      ~ "Spatula_discors",
    Binomial == "Anas_penelope"     ~ "Mareca_penelope",
    Binomial == "Anas_strepera"     ~ "Mareca_strepera",
    Binomial == "Chen_caerulescens" ~ "Anser_caerulescens",
    Binomial == "Chen_rossii"       ~ "Anser_rossii",
    Binomial == "Larus_thayeri"     ~ "Larus_glaucoides",
    TRUE ~ Binomial)
  ) 

iucn_birds <- iucn_birds %>%
  left_join(socb_wide2, by = c("Binomial" = "binomial")) 

# check for  missing values
vis_miss(iucn_birds)

# Data cleaning: CLPI and IUCN -------------------------------------------------

## Data cleaning: CLPI dataset --------------------------------------------------

# there's a scavenger class, which is tricky to categorize to
# herbivores, carnivores, or omnivores
# because scavengers can feed on animals or plants
# after some detective work, the only scavenger is turkey vulture
# (Cathartes aura), which almost exclusively feed on carrion

clpi_birds <- clpi_birds %>%
  mutate(diet = case_when(
    diet == "scav" ~ "carnivore", 
    TRUE ~ diet)
    )

# there are some missing trait values for functional groups
# use https://www.allaboutbirds.org as a resource to extract this information 

clpi_birds <- clpi_birds %>%
  mutate(Binomial = str_replace(Binomial, pattern = " ", replacement = "_")) %>%
  mutate(func_groups = case_when( 
    Binomial == "Ardea_alba"               ~ "wetlands",  # marshes
    Binomial == "Bubo_scandiacus"          ~ "other", # tundra habitat
    Binomial == "Calypte_anna"             ~ "forest",   # open woodland
    Binomial == "Haematopus_bachmani"      ~ "shore", 
    Binomial == "Himantopus_mexicanus"     ~ "wetlands",
    Binomial == "Lanius_excubitor"         ~ "other", # not found in website
    Binomial == "Larus_argentatus\n"       ~ "shore",
    Binomial == "Melanerpes_carolinus"     ~ "forest",
    Binomial == "Meleagris_gallopavo"      ~ "forest", # open woodland
    Binomial == "Polioptila_caerulea"      ~ "forest", # scrub
    Binomial == "Psaltriparus_minimus"     ~ "forest", # scrub
    Binomial == "Thryothorus_ludovicianus" ~ "forest", # open woodland
    Binomial == "Troglodytes_troglodytes"  ~ "other",
    TRUE ~ func_groups)
  )

## Data cleaning: IUCN ----------------------------------------------------------

# there's a scavenger class, which is tricky to categorize to
# herbivores, carnivores, or omnivores
# because scavengers can feed on animals or plants
# after some detective work, the only scavenger is turkey vulture
# (Cathartes aura), which almost exclusively feed on carrion

iucn_birds <- iucn_birds %>%
  mutate(diet = case_when(
    diet == "scav" ~ "carnivore", 
    TRUE ~ diet)
  )

# there are some missing trait values for functional groups
# use https://www.allaboutbirds.org as a resource to extract this information 

iucn_birds <- iucn_birds %>%
  mutate(
    Binomial = str_replace(
      Binomial, 
      pattern = " ", 
      replacement = "_")
    ) %>%

  # no data on All About Birds for the following:  
  # Circus cyaneus 
  # Fringilla montifringilla 
  # Pterodroma cahow
  # Puffinus genus 
  # so, use missing values (NAs) for them

  mutate(func_groups = case_when( 
    
    Binomial == "Ardea_alba"                 ~ "wetlands", # marshes
    Binomial == "Brachyramphus_brevirostris" ~ "shore+forest", 
    Binomial == "Bubo_scandiacus"            ~ "other", # tundra habitat
    Binomial == "Calypte_anna"               ~ "forest", # open woodland
    Binomial == "Grus_canadensis"            ~ "wetlands", # marshes
    Binomial == "Himantopus_mexicanus"       ~ "wetlands",
    Binomial == "Lanius_excubitor"           ~ "other", # not found in website
    Binomial == "Melanerpes_carolinus"       ~ "forest",
    Binomial == "Meleagris_gallopavo"        ~ "forest", # open woodland
    Binomial == "Polioptila_caerulea"        ~ "forest", # scrub
    Binomial == "Psaltriparus_minimus"       ~ "forest", # scrub
    Binomial == "Spizella_arborea"           ~ "forest",  # open woodland
    Binomial == "Thryothorus_ludovicianus"   ~ "forest", # open woodland
    Binomial == "Troglodytes_troglodytes"    ~ "other",
    
    TRUE ~ func_groups)
  )
    
# Data visualization: trait distributions --------------------------------------

# remove the gray background
theme_set(theme_bw())

# make palette
colors <- c("All Birds" = "slategray",
            "Canadian Wild Species" = "navyblue",
            "C-LPI" = "firebrick")

## Trait distribution: body mass -----------------------------------------------

ggplot() +
  geom_density(data = glob_birds,
               aes(x = log(mean_adult_body_mass_g), fill = "All Birds"),
               lwd = 0.2, alpha = .5) +
  geom_density(data = iucn_birds,
               aes(x = log(mean_adult_body_mass_g), fill = "Canadian Wild Species"),
               lwd = 0.2, alpha = .5) +
  geom_density(data = clpi_birds,
               aes(x = log(mean_adult_body_mass_g), fill = "C-LPI"),
               lwd = 0.2, alpha = .5) +
  scale_fill_manual(values = colors) +
  labs(title = "Body size", x = "log Body mass (g)", y = "Density",
       fill = "Dataset") 

## Trait distribution: hand wing index -----------------------------------------

ggplot() +
  geom_density(data = glob_birds,
               aes(x = log(hwi), fill = "All Birds"),
               lwd = 0.2, alpha = .5) +
  geom_density(data = iucn_birds,
               aes(x = log(hwi), fill = "Canadian Wild Species"),
               lwd = 0.2, alpha = .5) +
  geom_density(data = clpi_birds,
               aes(x = log(hwi), fill = "C-LPI"),
               lwd = 0.2, alpha = .5) +
  scale_fill_manual(values = colors) +
  labs(title = "Hand wing index", x = "log Hand wing index", y = "Density",
       fill = "Dataset") 

## Trait distribution: maximum longevity ---------------------------------------

ggplot() +
  geom_density(data = glob_birds,
               aes(x = log(mean_max_longevity_y), fill = "All Birds"),
               lwd = 0.2, alpha = .5) +
  geom_density(data = iucn_birds,
               aes(x = log(mean_max_longevity_y), fill = "Canadian Wild Species"),
               lwd = 0.2, alpha = .5) +
  geom_density(data = clpi_birds,
               aes(x = log(mean_max_longevity_y), fill = "C-LPI"),
               lwd = 0.2, alpha = .5) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Maximum longevity", 
    x = "log Max longevity (in years)", 
    y = "Density",
    fill = "Dataset"
    ) 

## Trait distributions: longevity ---------------------------------------------

ggplot() +
  geom_density(data = glob_birds,
               aes(x = log(mean_longevity_y), fill = "All Birds"),
               lwd = 0.2, alpha = .5) +
  geom_density(data = iucn_birds,
               aes(x = log(mean_longevity_y), fill = "Canadian Wild Species"),
               lwd = 0.2, alpha = .5) +
  geom_density(data = clpi_birds,
               aes(x = log(mean_longevity_y), fill = "C-LPI"),
               lwd = 0.2, alpha = .5) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Longevity", 
    x = "log longevity (in years)", 
    y = "Density",
    fill = "Dataset"
  ) 

## Trait distributions: dietary guilds -----------------------------------------

# diet (with all three databases)
ggplot() +
  geom_histogram(data = filter(glob_birds, !is.na(diet)),
                 aes(x = diet, fill = "All Birds"),
                 lwd = .2, alpha = .3, binwidth = .5,
                 stat = "count") +
  geom_histogram(data = filter(iucn_birds, !is.na(diet)),
                 aes(x = diet, fill = "Canadian Wild Species"),
                 lwd = .2, alpha = .7, binwidth = .5,
                 stat = "count") +
  geom_histogram(data = filter(clpi_birds, !is.na(diet)),
                 aes(x = diet, fill = "C-LPI"),
                 lwd = .2, alpha = .8, binwidth = .5,
                 stat = "count") +
  scale_fill_manual(values = colors) +
  labs(title = "Trophic Level", x = "Trophic Level", fill = "Dataset")

# diet (with C-LPI and Canadian Wild Species)
ggplot() +
  geom_histogram(data = filter(iucn_birds, !is.na(diet)),
                 aes(x = diet, fill = "Canadian Wild Species"),
                 lwd = .2, alpha = .7, binwidth = .5,
                 stat = "count") +
  geom_histogram(data = filter(clpi_birds, !is.na(diet)),
                 aes(x = diet, fill = "C-LPI"),
                 lwd = .2, alpha = .8, binwidth = .5,
                 stat = "count") +
  scale_fill_manual(values = colors) +
  labs(title = "Trophic Level", x = "Trophic Level", fill = "Dataset")

## Trait distributions: functional groups --------------------------------------

# functional groups (with C-LPI and Canadian Wild Species)
ggplot() +
  geom_histogram(data = filter(iucn_birds, !is.na(func_groups)),
                 aes(x = func_groups, fill = "Canadian Wild Species"),
                 lwd = .2, alpha = .7, binwidth = .5,
                 stat = "count") +
  geom_histogram(data = filter(clpi_birds, !is.na(func_groups)),
                 aes(x = func_groups, fill = "C-LPI"),
                 lwd = .2, alpha = .8, binwidth = .5,
                 stat = "count") +
  coord_flip() + 
  scale_fill_manual(values = colors) +
  labs(title = "Functional Groups", x = "Functional Groups", fill = "Dataset")

# Save to disk -----------------------------------------------------------------

saveRDS(clpi_birds, "data-clean/LPI_birds_traits.rds")
saveRDS(glob_birds, "data-clean/glob_birds_traits.rds")
saveRDS(iucn_birds, "data-clean/iucn_birds_traits.rds")
