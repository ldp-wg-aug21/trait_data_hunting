# Extract bird traits 

# Install devtools if not available
# if(!"remotes" %in% installed.packages()[,"Package"]) install.packages("remotes")

# Install traitdata package from Github
# remotes::install_github("RS-eco/traitdata")

# load packages
library(traitdata)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)
library(visdat)
library(patchwork)
library(ggplot2)

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

data("amniota")

## Load the Canadian dataset ---------------------------------------------------

clpi <- read.csv("data-raw/cLPI_data_resolved_species.csv")

## Load the IUCN dataset -------------------------------------------------------

iucn <- read.csv("data-raw/WildSpecies2015Data.csv")

# Heard et al. 2020 dataset ----------------------------------------------------

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

# Amniote dataset --------------------------------------------------------------

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

# Build the global bird trait dataset ------------------------------------------

# Left join with Sheard et al. 2020 since that dataset has more species 
# than amniota 
glob_birds<- hwi_tidy %>%
  left_join(amniota_summ, by = "binomial")

# check for missing values 
vis_miss(glob_birds)

# Build with LPI dataset -------------------------------------------------------

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
  filter(Binomial != "NA")

# check for missing values 
vis_miss(clpi_birds)

# Build IUCN dataset -----------------------------------------------------------

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

# check for  missing values
vis_miss(iucn_birds)

# Data visualization: trait distributions --------------------------------------

# remove the gray background
theme_set(theme_bw())

# make palette
colors <- c("All Birds" = "slategray",
            "Canadian Wild Species" = "navyblue",
            "C-LPI" = "firebrick")

# body mass (in grams)
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

# hand wing index
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

# maximum longevity (in years)
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

# longevity (in years)
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

# Save to disk -----------------------------------------------------------------

saveRDS(clpi_birds, "data-clean/LPI_birds_traits.rds")
=======


## Load the Canadian dataset

clpi <- read.csv("data-raw/CIEE_LPI_dataset.csv")

# subset to birds
sp_birds <- filter(lpd, Class %in% c("Aves", "Birds")) %>% summarise(sp = unique(Binomial)) 

# convert to a vector
sp_birds <- sp_birds$sp


## 1. Extract avian elton traits 

# load Elton Traits
data("elton_birds")

# extract data
clpi_eltonbirds <- elton_birds %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp_birds) %>%
  # subset to the traits we want
  subset(select = c(scientificNameStd, 
                    Diet.5Cat, 
                    BodyMass.Value)) 
 
# look at the dataset  
summary(clpi_eltonbirds)

# check which species are missing body mass data
clpi_eltonbirds[which(is.na(clpi_eltonbirds$Diet.5Cat)), "scientificNameStd"]
clpi_eltonbirds[which(is.na(clpi_eltonbirds$BodyMass.Value)), "scientificNameStd"]

colnames(clpi_eltonbirds)[1] <- "Binomial"


## 2. Extract avian body size 

# load avian body size data
data("AvianBodySize")

# extract data
clpi_aviansize <- AvianBodySize %>%
  # switch out species for _ in the species names to match the LPD
  mutate(scientificNameStd = gsub(" ", "_", scientificNameStd)) %>%
  # filter for species who are in the LPD dataset
  filter(scientificNameStd %in% sp_birds) %>%
  # subset to the traits we want
  subset(select = c(scientificNameStd, 
                    M_mass, 
                    F_mass)) %>%
  # change -999.0 to NAs
  mutate(M_mass = replace(M_mass, M_mass == -999.0, NA),
         F_mass = replace(F_mass, F_mass == -999.0, NA)) 
  # compute average for male and female mass
    
# look at the dataset  
summary(clpi_aviansize)

# check which species are missing body mass data
clpi_aviansize[which(is.na(clpi_aviansize$mass)), "scientificNameStd"]
colnames(clpi_aviansize)[1] <- "Binomial"


## Join to the larger clpi dataset

clpi_birds <- clpi %>% 
                left_join(clpi_eltonbirds, by = "Binomial") %>%
                left_join(clpi_aviansize, by = "Binomial")

# change NAs to NULL
clpi_birds[is.na(clpi_birds)] <- "NULL"

head(clpi_birds)

# write to rds
saveRDS(clpi_birds, "data-clean/LPI_birds.rds")
