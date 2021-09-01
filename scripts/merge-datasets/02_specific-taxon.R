# Script to make specific taxon trait datasets
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

# add species ID
mammals <- mammals %>% add_column(UUID = NA, .before = "Binomial")

# convert body size measurement to comparable metric?
mammals <- mammals %>% add_column(bodysize = NA,
                                  .after = "AdultBodyMass_g")
# write to file
write.csv(mammals, "data-clean/traits-specific-mammals.csv")

# generate metadata file
meta_mammals <- data.frame("Variable" = colnames(mammals),
           "Units" = c(NA, NA, "g", NA, NA, "days", "months"),
           "Type" = c(NA, NA, "continuous", "continuous", "categorical", "continuous", "continuous"),
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

# add species ID
birds <- birds %>% add_column(UUID = NA, .before = "Binomial")

# convert body size measurement to comparable metric?
birds <- birds %>% add_column(bodysize = NA)

# write to file
write.csv(birds, "data-clean/traits-specific-birds.csv")

# generate metadata file
meta_birds <- data.frame("Variable" = colnames(birds),
                   "Units" = NA,
                   "Type" = NA,
                   "Source" = NA,
                   "Description" = NA)
write.csv(meta_birds, "data-clean/metadata/traits-specific-birds.csv", row.names = FALSE)
