## Extracting trait data from Fishbase

#load packages
pkgs <- c("tidyverse", "rfishbase")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

# load Canadian LPI data
lpi_data <- read_csv("./data-raw/CIEE_LPI_dataset.csv")

#chnage subclass to class
lpi_data$Class <- gsub("Actinopteri", "Fish",lpi_data$Class)
lpi_data$Class <- gsub("Coelacanthi", "Fish",lpi_data$Class)
lpi_data$Class <- gsub("Elasmobranchii", "Fish",lpi_data$Class)
lpi_data$Class <- gsub("Dipneusti", "Fish",lpi_data$Class)
lpi_data$Class <- gsub("Petromyzonti", "Fish",lpi_data$Class)
lpi_data$Class <- gsub("Holocephali", "Fish",lpi_data$Class)

# filter out fish only
fish_LPI <- lpi_data %>% 
  filter(Class == "Fish")

# what species do we have abundance data for?
fish_species <- unique(fish_LPI$Binomial)

#replace '_' with ' ' to match fishbase format
fish_species <- gsub("_", " ", fish_species)

# Trophic Level -----------------------------------------------------------

#using the ecology() function in rfishbase to get trophic level information
fish_ecology <- ecology(species_list = fish_species,
                        fields = c("Species", "Herbivory2", "DietTroph", "DietSeTroph", 
                                    "FoodTroph", "FoodSeTroph", "DietRemark"))



## filter to species that have trophic information in at least one column 
fish_ecology <- fish_ecology %>% 
  filter_at(vars(Herbivory2, DietTroph, FoodTroph), any_vars(!is.na(.)))

#are there duplicates?
dup_species <- fish_ecology[duplicated(fish_ecology, 1)]

## Joining it back with the original dataset   
fish_ecology <- fish_ecology %>% 
  rename(Binomial = Species) 

fish_ecology$Binomial <- gsub(" ", "_", fish_ecology$Binomial)

#adding a column in so we know which dataset the information came from 
fish_ecology$trophic_database <- "fishbase"

# join new columns back with fish_LPI data set 
fish_LPI <- left_join(x = fish_LPI, y = fish_ecology, by = "Binomial")


# Habitat Type ------------------------------------------------------------

habitat_types <- ecology(species_list = fish_species)
habitat_types <- habitat_types %>% 
  select(c(Species, Neritic:Cave2)) 

# Body Size Metrics ---------------------------------------------------------------

fish_size <- length_weight(fish_species) %>% 
  select(Species, LengthMin, LengthMax, Type, Sex)

fish_length <- length_length(fish_species)
