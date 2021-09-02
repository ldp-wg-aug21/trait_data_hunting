# Script to merge all taxon-specific datasets into one mega-dataset to rule them all

# read in the taxon-specific datasets
taxon_traits <- lapply(paste0("data-clean/", list.files("data-clean", pattern = "traits-specific-")), 
                       read.csv, row.names = 1)
# name each element of the list from the file names (and remove the file extension etc.)
names(taxon_traits) <- gsub("traits-specific-", "", 
                            list.files("data-clean", pattern = "traits-specific-")) %>%
  gsub(".csv", "", .)

# select the three standardized traits
standard_traits <- lapply(taxon_traits, 
                          subset, 
                          select = c("UUID", 
                                     "Binomial", 
                                     "BodySize", 
                                     "TrophicLevel", 
                                     "LifeSpan"))

# bind all datasets together into an overall trait dataset
all_traits <- bind_rows(standard_traits, .id = "Group")

# convert trophic level to a factor
all_traits$TrophicLevel <- factor(all_traits$TrophicLevel,
                                  levels = c("1", "2", "3"))

# save file
write.csv(all_traits, "data-clean/traits-all.csv")
