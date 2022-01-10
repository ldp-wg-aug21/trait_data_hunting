# Get mammal data from Elton Traits

# Install devtools if not available
#if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install traitdata package from Github
#remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)

# load packages
library(traitdata)
library(dplyr)

# load the Canadian dataset
lpd <- read.csv("data-raw/CIEE_LPI_dataset.csv")
# subset to mammals
sp <- filter(lpd, Class == "Mammalia") %>% summarise(sp = unique(Binomial)) 
# convert to a vector
sp <- sp$sp

# load pantheria dataset
data("pantheria")

# extract data
df <- pantheria %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp) %>%
  # subset to the traits we want
  subset(select = c(scientificNameStd, 
                    AdultBodyMass_g, 
                    TrophicLevel, 
                    GestationLen_d,
                    MaxLongevity_m))
# look at the dataset  
summary(df)

# check which species are missing data for each trait
df[which(is.na(df$AdultBodyMass_g)), "scientificNameStd"]
df[which(is.na(df$TrophicLevel)), "scientificNameStd"]
df[which(is.na(df$GestationLen_d)), "scientificNameStd"]
df[which(is.na(df$MaxLongevity_m)), "scientificNameStd"]
colnames(df)[1] <- "Binomial"

# check if other datasets can fill in these gaps 

# # Load Elton Traits
# data("elton_mammals")

# # load dataset for trophic information
# data("mammal_diet2")

# mammal diet doesn't have more species
sp[which(!sp %in% gsub(" ", "_", mammal_diet2$scientificNameStd))]
sp[which(!sp %in% gsub(" ", "_", pantheria$scientificNameStd))]
sp[which(!sp %in% gsub(" ", "_", elton_mammals$scientificNameStd))]

# join to the larger lpd dataset
lpd_traits <- left_join(lpd, df, by = "Binomial")

# write to rds
saveRDS(lpd_traits, "data-clean/LPI_pantheria.rds")
