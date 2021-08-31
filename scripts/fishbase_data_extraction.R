## Extracting trait data from Fishbase

#load packages
pkgs <- c("tidyverse", "rfishbase")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

# load Canadian LPI data
data <- read_csv("./data-raw/CIEE_LPI_dataset.csv")

# filter out fish only
fish_LPI <- data %>% 
  filter(Class == "Fish")

# what species do we have abundance data for?
fish_species <- unique(fish_LPI$Binomial)

#replace '_' with ' ' to match fishbase format
fish_species <- gsub("_", " ", fish_species)


# Trophic Level -----------------------------------------------------------

#using the ecology() function in rfishbase to get trophic level information
fish_ecology <- ecology(species_list = fish_species,
                        fields = c("Species", "Herbivory2", "DietTroph", "DietSeTroph", 
                                   "DietRemark"))

fish_ecology <- fish_ecology %>% 
  rename(Binomial = Species) 

fish_ecology$Binomial <- gsub(" ", "_", fish_ecology$Binomial)

fish_ecology$trophic_database <- "fishbase"

# join new columns back with fish_LPI data set 
fish_LPI <- left_join(x = fish_LPI, y = fish_ecology, by = "Binomial")








# Habitat Type ------------------------------------------------------------

habitat_type <- names(ecology(species_list = "fish_species"))

View(ecology()[1:5, ])
habitat_types <- ecology(species_list = fish_species)
habitat_types %>% 
  select(Neritic:Cave2) %>% View

# Body Size ---------------------------------------------------------------

?length_weight()

fish_size <- length_weight(fish_species)
