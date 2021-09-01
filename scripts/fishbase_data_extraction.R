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
# add in references, food remark and food ref 
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


# Comparing CLPI trait distribution to larger dataset traits --------------

# List of all species in Canada
wildsp <- readr::read_csv("/Users/sandraemry/Documents/LPI_working_group/trait_based/trait_data_hunting/data-raw/WildSpecies2015Data.csv", col_names = FALSE)

names(wildsp) <- as.character(wildsp[1, ])

canadian_fish_sp <- wildsp[2:nrow(wildsp),6]

canadian_fish_sp <- unique(canadian_fish_sp)

canadian_fish_sp <- canadian_fish_sp %>% 
  separate(Binomial, into = c("genus", "species"), sep = "_") %>% 
  unite("Binomial", c("genus", "species"), sep = " ", remove = TRUE)

#vector of all Canadian fish species 
canadian_fish_sp <- pull(canadian_fish_sp, Binomial)

#Pulling trophic level traits for all Canadian fish 
canadian_fish_trophic_data <- ecology(species_list = canadian_fish_sp,
                        fields = c("Species", "Herbivory2", "DietTroph", "DietSeTroph", 
                                   "FoodTroph", "FoodSeTroph", "DietRemark"))

## filter to species that have trophic information in at least one column 
canadian_fish_trophic_data <- canadian_fish_trophic_data %>% 
  filter_at(vars(Herbivory2, DietTroph, FoodTroph), any_vars(!is.na(.)))

## Are there duplicates?
dup_species <- canadian_fish_trophic_data[which((duplicated(canadian_fish_trophic_data$Species))), 1]

canadian_fish_trophic_data$Species == as.vector(dup_species)

# Pulling body size data 
all_canadian_fish_size <- length_weight(canadian_fish_sp) %>% 
  select(Species, LengthMin, LengthMax, Type, Sex)

## filter to species that have size info in at least one column 
all_canadian_fish_size <- all_canadian_fish_size %>% 
  filter_at(vars(LengthMin, LengthMax), any_vars(!is.na(.)))

dup_species <- all_canadian_fish_size[which((duplicated(all_canadian_fish_size$Species))), 1]    

all_canadian_fish_size %>% 
  group_by(Species) %>% 
  count()

# Life span data
all_canadian_fish_age <- estimate(canadian_fish_sp) %>% 
  select(Species, AgeMin, AgeMax) %>% 
  filter(!is.na(AgeMax))

# More length, and age (tmax) data
all_canadian_fish_popchar <- popchar(canadian_fish_sp) %>% 
  select(Species, PopCharRefNo, Sex, Lmax, Type, tmax)

names(reproduction(canadian_fish_sp))

