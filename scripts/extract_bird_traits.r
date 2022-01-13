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
library(ggplot2)      # to visualize data
library(here)         # to create relative file-paths
library(tidyr)        # to create wide or long data frames 

# Custom functions --------------------------------------------------------------

#' Assign bird groups for a given species 
#' based on the State of Canada's Bird 2019 supplementary dataset 
#' 
#' @param ls a list of bird groups (see below for all categories)
#' 
#' @return a vector of character strings representing a single bird group 
#' or multiple bird groups 
#' 
#' @examples 
#' assign_bird_grp(list(("shore", "shore")))
#' assign_bird_grp(list("shore", "shore", "grasslands"))
#' assign_bird_grp(list("wetlands", "seabirds"))
#' 
#' A list of all categories
#' 1) aerial insectivore, 
#' 2) birds_prey, 
#' 3) forest,  
#' 4) grasslands, 
#' 5) seabird,
#' 6) shore, 
#' 7) waterfowl, 
#' 8) wetlands, 
#' 9) other
#' 
assign_bird_grp <- function(ls) {
  
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


# Import data ------------------------------------------------------------------

## Load the Sheard et al. 2020 dataset -----------------------------------------

# Sheard et al. 2020. Nature Communications. 
# https://zenodo.org/record/3832215#.YS4_TsZE1KM

# read_excel tries to automically parse the dataset
# but there are some warnings for parsing
# mostly because the authors used string characters for NA values
hwi_raw <- read_excel(
  "data-raw/heard-et-al_2020_hwi_2020.xlsx", 
  sheet = "speciesdata"
)

## Load the Amniota dataset ----------------------------------------------------

# from traitdata R package 
data("amniota")

## Load the Canadian LPI dataset ---------------------------------------------------

clpi <- read.csv("data-raw/cLPI_data_resolved_species.csv")

## Load the  Wild Species QCQA -------------------------------------------------------

wild <- read.csv("data-raw/WildSpecies2015Data.csv")

## Load SOCB dataset -----------------------------------------------------------

# SOCB = State of Canada's Birds (2019 report)
# http://nabci.net/resources/state-of-canadas-birds-2019/

socb <- read_excel(
  here("data-raw", "SOCB-Data-Sources_Source-de-donnees-EPOC-1.xlsx"),
  sheet = "Species Groups_Groupes d'espèce"
)

# Data cleaning (for separate datasets) ----------------------------------------

## Data cleaning: State of Canada's Birds --------------------------------------

socb_wide <- socb %>%
  janitor::clean_names() %>%
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
  summarize(grp = list(name)) %>%
  ungroup() 

socb_wide2 <- socb_wide %>%
  mutate(bird_grp = sapply(grp, assign_bird_grp)) %>%
  select(binomial, bird_grp)

# sanity check
vis_miss(socb_wide2)

# fix taxonomic synonyms so they match with Wild Species QCQA/C-LPI datasets
socb_wide2 <- socb_wide2 %>%
  mutate(binomial = case_when(
    binomial == "Mareca_americana"   ~ "Anas_americana",    
    binomial == "Spatula_clypeata"   ~ "Anas_clypeata", 
    binomial == "Spatula_cyanoptera" ~ "Anas_cyanoptera",
    binomial == "Spatula_discors"    ~ "Anas_discors",
    binomial == "Mareca_penelope"    ~ "Anas_penelope",
    binomial == "Mareca_strepera"    ~ "Anas_strepera",
    binomial == "Anser_caerulescens" ~ "Chen_caerulescens",
    binomial == "Anser_rossii"       ~ "Chen_rossii",
    binomial == "Larus_glaucoides"   ~ "Larus_thayeri",
    TRUE ~ binomial)
  ) 

## Data cleaning:  Heard et al. 2020 -------------------------------------------

# select the relevant columns from the avian trait data set

# candidate traits: 
# (1) body mass, 
# (2) hang-wind index (a proxy for dispersal ability)
# (3) diet

# traits were previously collected from previous databases: 
# (1) body mass - Tobias and Pigot (2019) and Dunning (2007)
# (2) hang-wing index - 45k adult live and museum specimens
# (3) diet - Pigot et al. 2020 and EltonTraits

hwi_tidy <- hwi_raw %>%
  janitor::clean_names() %>%
  select(
    binomial = species_name, 
    hwi, 
    diet
  ) %>%
  mutate(across(.cols = everything(), na_if, "NA")) %>%
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

## Build Wild Species QCQA dataset ---------------------------------------------

wild_birds <- wild %>%
  inner_join(glob_birds, by = c("Binomial" = "binomial")) %>%
  select(
    Binomial, 
    hwi, 
    diet, 
    mean_adult_body_mass_g, 
    mean_max_longevity_y, 
    mean_longevity_y
  ) %>%
  filter(!duplicated(Binomial)) %>%
  filter(Binomial != "NA") 

wild_birds <- wild_birds %>%
  left_join(socb_wide2, by = c("Binomial" = "binomial")) 

# check for  missing values
vis_miss(wild_birds)

# Data cleaning: CLPI and Wild Species QCQA ------------------------------------

## Data cleaning: CLPI dataset -------------------------------------------------

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

clpi_birds <- clpi_birds %>%
  rename(habitat = bird_grp) %>%
  mutate(
    Binomial = str_replace(Binomial, pattern = " ", replacement = "_"),
    
    habitat  = case_when(
    
      # habitat-level modifications
      habitat == "aerial_insect+forest"  ~ "forest",
      habitat == "birds_prey+grasslands" ~ "grasslands",
      habitat == "wetlands+seabirds"     ~ "wetlands+oceans",
      habitat == "seabirds"              ~ "oceans",
      
      # species-level modifications for birds of prey
      Binomial == "Accipiter_cooperii"       ~ "forest",
      Binomial == "Buteo_lagopus"            ~ "grasslands",
      Binomial == "Falco_peregrinus"         ~ "shore",
      Binomial == "Falco_columbarius"        ~ "forest",
      Binomial == "Pandion_haliaetus"        ~ "other",  # lakes and ponds
      Binomial == "Cathartes_aura"           ~ "forest", # open woodlands
      Binomial == "Buteo_platypterus"        ~ "forest",
      Binomial == "Buteo_lineatus"           ~ "forest",
      Binomial == "Buteo_jamaicensis"        ~ "forest", # open woodlands
      Binomial == "Falco_rusticolus"         ~ "other",  # tundra
      Binomial == "Aquila_chrysaetos"        ~ "grasslands",
      Binomial == "Accipiter_striatus"       ~ "forest",
      Binomial == "Accipiter_gentilis"       ~ "forest",
      Binomial == "Accipiter_cooperii"       ~ "forest",
      Binomial == "Haliaeetus_leucocephalus" ~ "forest",
      
      # species-level modifications for waterfowls
      Binomial == "Lophodytes_cucullatus"     ~ "lakes/ponds",
      Binomial == "Aix_sponsa"                ~ "lakes/ponds", 
      Binomial == "Anas_crecca"               ~ "lakes/ponds", 
      Binomial == "Anas_acuta"                ~ "wetlands", # marshes
      Binomial == "Anas_platyrhynchos"        ~ "lakes/ponds",
      Binomial == "Anas_rubripes"             ~ "lakes/ponds",  
      Binomial == "Anser_albifrons"           ~ "lakes/ponds",  
      Binomial == "Aythya_affinis"            ~ "lakes/ponds",
      Binomial == "Aythya_americana"          ~ "lakes/ponds",
      Binomial == "Aythya_collaris"           ~ "lakes/ponds",
      Binomial == "Aythya_marila"             ~ "lakes/ponds",
      Binomial == "Aythya_valisineria"        ~ "lakes/ponds",
      Binomial == "Branta_bernicla"           ~ "wetlands", # marshes
      Binomial == "Branta_hutchinsii"         ~ "lakes/ponds",
      Binomial == "Bucephala_albeola"         ~ "lakes/ponds",
      Binomial == "Bucephala_clangula"        ~ "lakes/ponds",
      Binomial == "Bucephala_islandica"       ~ "lakes/ponds",
      Binomial == "Clangula_hyemalis"         ~ "lakes/ponds",
      Binomial == "Cygnus_buccinator"         ~ "lakes/ponds", 
      Binomial == "Cygnus_columbianus"        ~ "lakes/ponds", 
      Binomial == "Histrionicus_histrionicus" ~ "other", # river/streams
      Binomial == "Melanitta_fusca"           ~ "other", # not found in website
      Binomial == "Mergus_merganser"          ~ "lakes/ponds",
      Binomial == "Mergus_serrator"           ~ "lakes/ponds",
      Binomial == "Oxyura_jamaicensis"        ~ "wetlands", # marshes
      Binomial == "Somateria_mollissima"      ~ "oceans", 
      Binomial == "Somateria_spectabilis"     ~ "oceans",
      
      # species-level modification for aerial insectivores
      Binomial == "Tachycineta_thalassina"     ~ "forest", # open woodland
      Binomial == "Stelgidopteryx_serripennis" ~ "other",  # river/streams
      Binomial == "Riparia_riparia"            ~ "lakes/ponds", 
      Binomial == "Progne_subis"               ~ "lakes/ponds",
      Binomial == "Hirundo_rustica"            ~ "grasslands", 
      Binomial == "Petrochelidon_pyrrhonota"   ~ "lakes/ponds", 
      Binomial == "Cypseloides_niger"          ~ "grasslands",
      Binomial == "Chordeiles_minor"           ~ "grasslands", 
      Binomial == "Chaetura_pelagica"          ~ "other", # towns
      Binomial == "Phalaenoptilus_nuttallii"   ~ "other",  # scrub
      Binomial == "Aeronautes_saxatalis"       ~ "other",  # scrub
      Binomial == "Tachycineta_bicolor"        ~ "lakes/ponds",
    
      TRUE ~ habitat)
  )

## Data cleaning: WIld Species QCQA --------------------------------------------

# there's a scavenger class, which is tricky to categorize to
# herbivores, carnivores, or omnivores
# because scavengers can feed on animals or plants
# after some detective work, the only scavenger is turkey vulture
# (Cathartes aura), which almost exclusively feed on carrion

wild_birds <- wild_birds %>%
  mutate(diet = case_when(
    diet == "scav" ~ "carnivore", 
    TRUE ~ diet)
  )


wild_birds <- wild_birds %>%
  rename(habitat = bird_grp) %>%
  mutate(
    Binomial = str_replace(Binomial, pattern = " ", replacement = "_"),
    
    habitat  = case_when(
      
      # habitat-level modifications
      habitat == "aerial_insect+forest"  ~ "forest",
      habitat == "birds_prey+grasslands" ~ "grasslands",
      habitat == "wetlands+seabirds"     ~ "wetlands+oceans",
      habitat == "seabirds"              ~ "oceans",
      
      # species-level modifications for birds of prey
      Binomial == "Accipiter_cooperii"       ~ "forest",
      Binomial == "Buteo_lagopus"            ~ "grasslands",
      Binomial == "Falco_peregrinus"         ~ "shore",
      Binomial == "Falco_columbarius"        ~ "forest",
      Binomial == "Pandion_haliaetus"        ~ "other",  # lakes and ponds
      Binomial == "Cathartes_aura"           ~ "forest", # open woodlands
      Binomial == "Buteo_platypterus"        ~ "forest",
      Binomial == "Buteo_lineatus"           ~ "forest",
      Binomial == "Buteo_jamaicensis"        ~ "forest", # open woodlands
      Binomial == "Falco_rusticolus"         ~ "other",  # tundra
      Binomial == "Aquila_chrysaetos"        ~ "grasslands",
      Binomial == "Accipiter_striatus"       ~ "forest",
      Binomial == "Accipiter_gentilis"       ~ "forest",
      Binomial == "Accipiter_cooperii"       ~ "forest",
      Binomial == "Haliaeetus_leucocephalus" ~ "forest",
      
      # species-level modifications for waterfowls
      Binomial == "Lophodytes_cucullatus"     ~ "lakes/ponds",
      Binomial == "Aix_sponsa"                ~ "lakes/ponds", 
      Binomial == "Anas_americana"            ~ "lakes/ponds",
      Binomial == "Anas_clypeata"             ~ "wetlands",
      Binomial == "Anas_cyanoptera"           ~ "wetlands",
      Binomial == "Anas_crecca"               ~ "lakes/ponds", 
      Binomial == "Anas_discors"              ~ "wetlands",
      Binomial == "Anas_penelope"             ~ "lakes/ponds",
      Binomial == "Anas_strepera"             ~ "wetlands",
      Binomial == "Anas_acuta"                ~ "wetlands", # marshes
      Binomial == "Anas_platyrhynchos"        ~ "lakes/ponds",
      Binomial == "Anas_rubripes"             ~ "lakes/ponds",  
      Binomial == "Anser_albifrons"           ~ "lakes/ponds",  
      Binomial == "Aythya_affinis"            ~ "lakes/ponds",
      Binomial == "Aythya_americana"          ~ "lakes/ponds",
      Binomial == "Aythya_collaris"           ~ "lakes/ponds",
      Binomial == "Aythya_marila"             ~ "lakes/ponds",
      Binomial == "Aythya_valisineria"        ~ "lakes/ponds",
      Binomial == "Branta_canadensis"         ~ "wetlands", # marshes
      Binomial == "Branta_bernicla"           ~ "wetlands", # marshes
      Binomial == "Branta_hutchinsii"         ~ "lakes/ponds",
      Binomial == "Bucephala_albeola"         ~ "lakes/ponds",
      Binomial == "Bucephala_clangula"        ~ "lakes/ponds",
      Binomial == "Bucephala_islandica"       ~ "lakes/ponds",
      Binomial == "Chen_caerulescens"         ~ "lakes/ponds",
      Binomial == "Chen_rossii"               ~ "lakes/ponds",
      Binomial == "Clangula_hyemalis"         ~ "lakes/ponds",
      Binomial == "Cygnus_buccinator"         ~ "lakes/ponds", 
      Binomial == "Cygnus_columbianus"        ~ "lakes/ponds", 
      Binomial == "Histrionicus_histrionicus" ~ "other", # river/streams
      Binomial == "Melanitta_americana"       ~ "oceans",
      Binomial == "Melanitta_perspicillata"   ~ "oceans",
      Binomial == "Melanitta_fusca"           ~ "other", # not found in website
      Binomial == "Mergus_merganser"          ~ "lakes/ponds",
      Binomial == "Mergus_serrator"           ~ "lakes/ponds",
      Binomial == "Oxyura_jamaicensis"        ~ "wetlands", # marshes
      Binomial == "Somateria_mollissima"      ~ "oceans", 
      Binomial == "Somateria_spectabilis"     ~ "oceans",
      
      # species-level modification for aerial insectivores
      Binomial == "Tachycineta_thalassina"     ~ "forest", # open woodland
      Binomial == "Stelgidopteryx_serripennis" ~ "other",  # river/streams
      Binomial == "Riparia_riparia"            ~ "lakes/ponds", 
      Binomial == "Progne_subis"               ~ "lakes/ponds",
      Binomial == "Hirundo_rustica"            ~ "grasslands", 
      Binomial == "Petrochelidon_pyrrhonota"   ~ "lakes/ponds", 
      Binomial == "Cypseloides_niger"          ~ "grasslands",
      Binomial == "Chordeiles_minor"           ~ "grasslands", 
      Binomial == "Chaetura_pelagica"          ~ "other", # towns
      Binomial == "Phalaenoptilus_nuttallii"   ~ "other",  # scrub
      Binomial == "Aeronautes_saxatalis"       ~ "other",  # scrub
      Binomial == "Tachycineta_bicolor"        ~ "lakes/ponds",
      
      TRUE ~ habitat)
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
  geom_density(data = wild_birds,
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
  geom_density(data = wild_birds,
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
  geom_density(data = wild_birds,
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
  geom_density(data = wild_birds,
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
  geom_histogram(data = filter(wild_birds, !is.na(diet)),
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
  geom_histogram(data = filter(wild_birds, !is.na(diet)),
                 aes(x = diet, fill = "Canadian Wild Species"),
                 lwd = .2, alpha = .7, binwidth = .5,
                 stat = "count") +
  geom_histogram(data = filter(clpi_birds, !is.na(diet)),
                 aes(x = diet, fill = "C-LPI"),
                 lwd = .2, alpha = .8, binwidth = .5,
                 stat = "count") +
  scale_fill_manual(values = colors) +
  labs(title = "Trophic Level", x = "Trophic Level", fill = "Dataset")

## Trait distributions: habitat ------------------------------------------------

# habitat (with C-LPI and Wild Species QCQA)
ggplot() +
  geom_histogram(data = wild_birds,
                 aes(x = habitat, fill = "Canadian Wild Species"),
                 lwd = .2, alpha = .7, binwidth = .5,
                 stat = "count") +
  geom_histogram(data = clpi_birds,
                 aes(x = habitat, fill = "C-LPI"),
                 lwd = .2, alpha = .8, binwidth = .5,
                 stat = "count") +
  coord_flip() + 
  scale_fill_manual(values = colors) +
  labs(title = "Habitat", x = "Habitat", fill = "Dataset")

# Save to disk -----------------------------------------------------------------

saveRDS(clpi_birds, "data-clean/birds_traits_CLPI.rds")
saveRDS(glob_birds, "data-clean/birds_traits_globalLPI.rds")
saveRDS(wild_birds, "data-clean/birds_traits_allcanadiansp.rds")
