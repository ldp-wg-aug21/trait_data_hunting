# Script to make specific taxon trait datasets

# load libraries 
library(tibble)
library(dplyr)


## MAMMALS #####################################################################

# read dataset
mammals <- readRDS("data-clean/LPI_pantheria.rds")

# filter to just mammals
mammals <- filter(mammals, Class %in% c("Mammals", "Mammalia")) %>%
  # keep unique binomials
  distinct(Binomial, .keep_all = TRUE) %>%
  # then subset to columns we want to keep
  subset(select = c(2, 87:ncol(mammals)))
# save original column names to keep track of what the traits were in the source database
original_names <- colnames(mammals)

# rename columns to standardize everything across taxons
mammals <- mammals %>% 
  # this part is specific to each dataset
  rename("BodySize" = "AdultBodyMass_g",
         "TrophicLevel" = "TrophicLevel",
         "LifeSpan" = "MaxLongevity_m") 

# convert body size measurement to comparable metric?
temp <- log(mammals$BodySize)*(1/max(log(mammals$BodySize), na.rm = TRUE))
# add the  standardized body size metric to the dataset
mammals <- add_column(mammals, BodySize_std = temp, .after = "BodySize")

# write to file
write.csv(mammals, "data-clean/traits-specific-mammals.csv")
# UUID is then assigned in the following script 02_generate_UUID.R

# generate metadata file
meta_mammals <- data.frame("column_name" = colnames(mammals),
                           "original_name" = c(original_names[1:2], NA, original_names[3:5]),
           "Units" = c(NA, "g", NA, NA, "days", "months"),
           "Type" = c(NA, "continuous", "continuous", "categorical", "continuous", "continuous"),
           "Source" = "Pantheria",
           "Description" = NA)
# write metadata file
write.csv(meta_mammals, "data-clean/metadata/traits-specific-mammals_metadata.csv", row.names = FALSE)



## BIRDS #######################################################################

birds <- readRDS("data-clean/LPI_birds.rds")

# filter to just birds
birds <- filter(birds, Class %in% c("Aves", "Birds")) %>%
  # keep unique binomials
  distinct(Binomial, .keep_all = TRUE) %>%
  # then subset to columns we want to keep
  subset(select = c(2, 87:ncol(birds)))
# save original column names to keep track of what the traits were in the source database
original_names <- colnames(birds)

# rename columns to standardize everything across taxons
birds <- birds %>% 
  # this part is specific to each dataset
  rename("BodySize" = "BodyMass.Value"#,
         #"TrophicLevel" = "TrophicLevel",
         #"LifeSpan" = "MaxLongevity_m"
         ) %>%
  # convert body size to numeric
  mutate(BodySize = as.numeric(BodySize))

# convert body size measurement to comparable metric?
temp <- log(birds$BodySize)*(1/max(log(birds$BodySize), na.rm = TRUE))
# add the  standardized body size metric to the dataset
birds <- add_column(birds, BodySize_std = temp, .after = "BodySize")

# write to file
write.csv(birds, "data-clean/traits-specific-birds.csv")
# UUID is then assigned in the following script 02_generate_UUID.R

# generate metadata file
meta_birds <- data.frame("column_name" = colnames(birds),
                           "original_name" = c(original_names[1:3], NA, original_names[4:5]),
                           "Units" = NA,
                           "Type" = NA,
                           "Source" = NA,
                           "Description" = NA)
# write metadata file
write.csv(meta_birds, "data-clean/metadata/traits-specific-birds_metadata.csv", row.names = FALSE)


## FISH ########################################################################

fish <- read.csv("data-clean/fish_traits_subset.csv") %>%
  # keep unique binomials
  distinct(Binomial, .keep_all = TRUE) %>%
  # then subset to columns we want to keep
  subset(select = c(2, 87:ncol(fish)))


# convert body size measurement to comparable metric?
temp <- log(fish$BodySize)*(1/max(log(fish$BodySize), na.rm = TRUE))
# add the  standardized body size metric to the dataset
fish <- add_column(fish, BodySize_std = temp, .after = "BodySize")

# write to file
write.csv(fish, "data-clean/traits-specific-fish.csv")
# UUID is then assigned in the following script 02_generate_UUID.R

# generate metadata file
meta_fish <- data.frame("column_name" = colnames(fish),
                         "original_name" = NA,
                         "Units" = NA,
                         "Type" = NA,
                         "Source" = NA,
                         "Description" = NA)
# write metadata file
write.csv(meta_fish, "data-clean/metadata/traits-specific-fish_metadata.csv", row.names = FALSE)
