## script to query rfishbase for Canadian LPI species traits 
## developed by Nikki

## install rfishbase if necessary:
#install.packages("rfishbase",repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"),type="source") 

library(tidyverse)
library(rfishbase)

####################################################
##    1. get list of Canadian LPI fish species    ##
####################################################

## read in our data:
clpi <- read_csv("data-raw/CIEE_LPI_dataset.csv") %>%
  filter(Class %in% c("Fish", "Actinopterygii", "Chondrichthyes", "Holocephali",
                      "Elasmobranchii", "Myxini")) # filter to fish

## get binomials of fish species:
Binomial <- unique(clpi$Binomial) %>%
  str_replace_all(., '\\_', ' ') # replace _ with a space

length(Binomial) #368 species!


####################################################
##       2. query fishbase for our fish spp       ##
####################################################
## make fish list into dataframe so it can be read by functions in rfishbase
fish <- as.data.frame(Binomial) %>%
  mutate(Binomial = as.character(Binomial)) # make binomial a character vector rather than a factor 

## get all fish in fishbase:
fishbase <- load_taxa(server = "fishbase")
db <- as.data.frame(fishbase)

## get data for our species:
query <- species(fish$Binomial)

## note: this might work for you, but it doesn't for me - the database returned only has species with genuses starting with the letters A-G

####################################################
##              3. don't use rfishbase            ##
####################################################
## so now I manually download and wrangl the fishbase data:
spp <- read.delim("data-raw/fb.2fspecies.tsv", sep = '\t')
# gen <- read.delim("data-raw/fb.2fgenera.tsv", sep = '\t')
# fam <- read.delim("data-raw/fb.2ffamilies.tsv", sep = '\t')
# ord <- read.delim("data-raw/fb.2forders.tsv", sep = '\t')
# cls <- read.delim("data-raw/fb.2fclasses.tsv", sep = '\t')

## subset the entire fishbase data base to only our species:
our_spp <- spp %>%
  filter(Binomial %in% fish$Binomial)

## read in ecological data
eco <- read.delim("data-raw/fb.2fecology.tsv", sep = '\t') %>%
  filter(!is.na(SpecCode)) %>%
  filter(SpecCode %in% our_spp$SpecCode) %>%
  mutate(SpecCode = as.factor(SpecCode)) %>%
  select(SpecCode, Herbivory2, DietTroph, DietSeTroph, DietRemark, DietRef,
         FoodTroph, FoodSeTroph, FoodRemark, FoodRef) %>%
  group_by(SpecCode) %>%
  ## if species have more than one value, take the mean
  mutate(DietTroph = mean(as.numeric(as.character(DietTroph)), na.rm = F),
         DietSeTroph = mean(as.numeric(as.character(DietSeTroph)), na.rm = F),
         FoodTroph = mean(as.numeric(as.character(FoodTroph)), na.rm = F),
         FoodSeTroph = mean(as.numeric(as.character(FoodSeTroph)), na.rm = F),
         DietRemark = paste(DietRemark, collapse = ", "),
         DietRef = paste(DietRef, collapse = ", "),
         FoodRemark = paste(FoodRemark, collapse = ", ")) %>%
  ungroup() %>%
  unique()

## read in estimated traits based on models
est <- read.delim("data-raw/fb.2festimate.tsv", sep = '\t') %>%
  mutate(SpecCode = as.factor(SpecCode)) %>%
  select(SpecCode, Troph, seTroph, TrophObserved, # trophic level and se
         Median_T, lcl_T, ucl_T, n_T) # generation time estimated as median ln(3)/K based on n_T growth studies


## info on trophic level data: https://github.com/ropensci/rfishbase/issues/199
## FoodTroph: MonteCarlo estimate of trophic level based on known food items
## DietTroph: mean or median of trophic levels derived from actual diet composition studies

spp$Binomial <- paste(spp$Genus, spp$Species, sep = ' ')

length(which(fish$Binomial %in% spp$Binomial))
## much better - now have data on 361 species

our_spp <- our_spp %>%
  left_join(., eco, by = 'SpecCode')%>%
  left_join(., est, by = 'SpecCode') %>%
  left_join(fish, ., by = "Binomial") # left join so we don't lose the 3 species not in fishbase
## note: some species duplicated because of eco table

## what traits do we get?
colnames(our_spp)

## nice - for now we just want:
# - habitat specialist/generalist
# - life span
# - body length
# - trophic level and position
# - generation time

traits <- our_spp %>%
  select("Binomial", "Genus", "Species", "FBname", "SpeciesRefNo", "LongevityWild",
         "LongevityCaptive", "Length", "LTypeMaxM", "LengthFemale", "LTypeMaxF",
         "CommonLength", "LTypeComM", "CommonLengthF", 
         "LTypeComF", "DepthRangeShallow", "DepthRangeDeep", "DepthRangeRef", 
         "DepthRangeComShallow", "DepthRangeComDeep", "DepthComRef", 
         "SpecCode", "Troph", "seTroph", "TrophObserved", 
         "Median_T", "lcl_T", "ucl_T", "n_T",
         "Herbivory2", "DietTroph", "DietSeTroph", "DietRemark", "DietRef",
         "FoodTroph", "FoodSeTroph", "FoodRemark", "FoodRef")



## merge with our CLPI database:
clpi_fish <- traits %>%
  mutate(Binomial = str_replace_all(Binomial, ' ', '_')) %>%
  select(-Genus, -Species) %>%
  left_join(clpi, ., by = c('Binomial'))

write.csv(clpi_fish, "data-clean/clpi_fishbase_merge.csv", row.names = FALSE)

