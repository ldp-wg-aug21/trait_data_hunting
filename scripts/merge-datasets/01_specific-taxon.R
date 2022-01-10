# Script to make specific taxon trait datasets

# load libraries 
library(tibble)
library(dplyr)


## MAMMALS #####################################################################

# read dataset
mammals <- readRDS("data-clean/mammals_traits_clpi.rds")

# filter to just mammals
mammals <- mammals %>%
  # keep unique binomials
  distinct(Binomial, .keep_all = TRUE)
# save original column names to keep track of what the traits were in the source database
original_names <- colnames(mammals)

# rename columns to standardize everything across taxons
mammals <- mammals %>% 
  # this part is specific to each dataset
  rename(
    "BodySize" = "elton_BodyMass.Value_g",
    "TrophicLevel" = "Trophic_Level",
         "LifeSpan" = "amniota_maximum_longevity_y") 

# convert trophic level to 1 2 and 3
mammals$TrophicLevel[which(mammals$TrophicLevel == "Herbivores")] <- 1
mammals$TrophicLevel[which(mammals$TrophicLevel == "Omnivores")] <- 2
mammals$TrophicLevel[which(mammals$TrophicLevel == "Carnivores")] <- 3

# convert body size measurement to comparable metric?
mammals$BodySize <- mammals$BodySize*(1/max(mammals$BodySize, na.rm = TRUE)) 

# write to file
write.csv(mammals, "data-clean/traits-specific-mammals.csv")
# UUID is then assigned in the following script 02_generate_UUID.R

# # generate metadata file
# meta_mammals <- data.frame("column_name" = colnames(mammals),
#                            "original_name" = c(original_names[1:2], NA, original_names[3:5]),
#            "Units" = c(NA, "g", NA, NA, "days", "months"),
#            "Type" = c(NA, "continuous", "continuous", "categorical", "continuous", "continuous"),
#            "Source" = "Pantheria",
#            "Description" = NA)
# write metadata file
# commented out because it is being filled online
#write.csv(meta_mammals, "data-clean/metadata/traits-specific-mammals_metadata.csv", row.names = FALSE)



## BIRDS #######################################################################

birds <- readRDS("data-clean/LPI_birds_traits.rds")

# save original column names to keep track of what the traits were in the source database
original_names <- colnames(birds)

  # add a TrophicLevel variable to be filled in from the diet information
birds$TrophicLevel <- NA
birds[which(birds$diet %in% c("vertebrates", "invertebrates", "scav")),"TrophicLevel"] <- 3
birds[which(birds$diet %in% c("plants", "seeds", "fruit", "nectar")),"TrophicLevel"] <- 1
birds[which(birds$diet %in% c("omnivore" )),"TrophicLevel"] <- 2

# rename lifespan variable
birds <- rename(birds,
                "LifeSpan" = "mean_max_longevity_y")

# convert body size measurement to comparable metric?
temp <- birds$mean_adult_body_mass_g*(1/max(birds$mean_adult_body_mass_g, na.rm = TRUE))
# add the  standardized body size metric to the dataset
birds <- add_column(birds, BodySize = temp, .after = "mean_adult_body_mass_g")

# write to file
write.csv(birds, "data-clean/traits-specific-birds.csv")
# UUID is then assigned in the following script 02_generate_UUID.R

# commented out because this was editted manually on github
# # generate metadata file
# meta_birds <- data.frame("column_name" = colnames(birds),
#                            "original_name" = c(original_names[1:5], NA, original_names[6:7], NA),
#                            "Units" = NA,
#                            "Type" = NA,
#                            "Source" = NA,
#                            "Description" = NA)
# # write metadata file
# write.csv(meta_birds, "data-clean/metadata/traits-specific-birds_metadata.csv", row.names = FALSE)


## FISH ########################################################################

fish <- read.csv("data-clean/fish/fish_traits_subset.csv") 

fish <- fish %>%
  # keep unique binomials
  distinct(Binomial, .keep_all = TRUE) %>%
  # then subset to columns we want to keep
  subset(select = c(2, 87:ncol(fish)))

# convert trophic level to 1 2 and 3
fish$TrophicLevel[which(fish$TrophicLevel == "herbivore")] <- 1
fish$TrophicLevel[which(fish$TrophicLevel == "omnivore")] <- 2
fish$TrophicLevel[which(fish$TrophicLevel == "carnivore")] <- 3

# convert body size measurement to comparable metric
fish$BodySize <- fish$BodySize*(1/max(fish$BodySize, na.rm = TRUE))

# write to file
write.csv(fish, "data-clean/traits-specific-fish.csv")
# UUID is then assigned in the following script 02_generate_UUID.R

# # generate metadata file
# meta_fish <- data.frame("column_name" = colnames(fish),
#                          "original_name" = NA,
#                          "Units" = NA,
#                          "Type" = NA,
#                          "Source" = NA,
#                          "Description" = NA)
# write metadata file
# commented out because it was filled manually on github
# write.csv(meta_fish, "data-clean/metadata/traits-specific-fish_metadata.csv", row.names = FALSE)


### HERPS ######################################################################

herps <- read.csv("data-clean/herp_traits_subset.csv") 

herps <- herps %>%
  # keep unique binomials
  distinct(Binomial, .keep_all = TRUE) %>%
  # then subset to columns we want to keep
  subset(select = c(2, 6:ncol(herps)))

# convert trophic level to 1 2 and 3
herps$TrophicLevel[which(herps$TrophicLevel == "herbivore")] <- 1
herps$TrophicLevel[which(herps$TrophicLevel == "omnivore")] <- 2
herps$TrophicLevel[which(herps$TrophicLevel == "carnivore")] <- 3

# convert body size measurement to comparable metric
herps$BodySize <- herps$BodySize*(1/max(herps$BodySize, na.rm = TRUE))

# write to file
write.csv(herps, "data-clean/traits-specific-herps.csv")
# UUID is then assigned in the following script 02_generate_UUID.R

# # generate metadata file
# meta_herps <- data.frame("column_name" = colnames(herps),
#                         "original_name" = NA,
#                         "Units" = NA,
#                         "Type" = NA,
#                         "Source" = NA,
#                         "Description" = NA)
# # write metadata file
# write.csv(meta_herps, "data-clean/metadata/traits-specific-herps_metadata.csv", row.names = FALSE)
