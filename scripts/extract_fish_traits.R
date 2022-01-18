## script to query rfishbase for Canadian LPI species traits 
## developed by Nikki and Sandra
library(tidyverse)
library(rfishbase)

####################################################
##                    functions                   ##
####################################################
summarise_traits <- function(spp_traits) {
  ## look at all the values of our traits
  trait_summary <- spp_traits %>%
    select("Binomial", "Genus", "Species","LongevityWild","Length", 
           "LTypeMaxM", 
           "LengthFemale", 
           "LTypeMaxF",
           "CommonLength", 
           "LTypeComM", 
           "CommonLengthF",
           "LTypeComF",
           "Weight", "WeightFemale", 
           "Median_T", "lcl_T", "ucl_T",
           "Herbivory2", "DietTroph", "DietSeTroph", 
           "FoodTroph", "FoodSeTroph",
           "Troph", "seTroph") %>%
    summarise_all(~ sum(is.na(.))) %>%
    gather(key = "Trait", value = 'Number of species missing trait') %>%
    mutate('Variable_type' = c(rep("categorical", 3), 
                               rep("continuous", 2), 
                               "NA", 
                               "continuous",
                               "NA",
                               "continuous",
                               "NA", 
                               "continuous",
                               "NA",
                               rep("continuous", 5),
                               'NA',
                               rep("continuous", 6)
    )) %>%
    mutate(Units = c(rep("NA", 3), rep("years", 1), 
                     "cm", "NA", 
                     "cm",
                     "NA",
                     "cm",
                     "NA", 
                     "cm",
                     "NA", 
                     rep("grams", 2),
                     rep('years', 3),
                     "NA",
                     rep('trophic levels', 6)))
  
  pasted = data.frame(val_cat = sapply(spp_traits, function(x) paste(unique(x),
                                                                     collapse = ', '))) %>%
    mutate(var = rownames(.)) %>%
    filter(var %in% trait_summary$Trait)
  
  minmax = data.frame(val_cont = sapply(spp_traits, 
                                        function(x) paste(round(min(as.numeric(as.character(x)),
                                                                    na.rm=T),2),
                                                          round(max(as.numeric(as.character(x)),
                                                                    na.rm=T),2),
                                                          sep = ' - '))) %>%
    mutate(var = rownames(.)) %>%
    filter(var %in% trait_summary$Trait)
  
  vals = left_join(pasted, minmax) 
  
  trait_summary = trait_summary %>%
    left_join(., vals, by = c("Trait" = 'var')) %>%
    mutate('Values' = ifelse(Variable_type == 'continuous', as.character(val_cont),
                             as.character(val_cat))) %>%
    select(-val_cont, -val_cat) %>%
    select(Trait, `Variable_type`, Units, Values, everything())
  
  return(trait_summary)
}


####################################################
##    1. get list of Canadian LPI fish species    ##
####################################################
## read in our data:
clpi <- read_csv("data-raw/CIEE_LPI_dataset.csv") %>%
  filter(Class %in% c("Fish", "Actinopterygii", "Chondrichthyes", "Holocephali",
                      "Elasmobranchii", "Myxini")) # filter to fish

## get binomials of fish species:
Binomial <- unique(clpi$Binomial) 

length(Binomial) #368 species!

## make fish list into dataframe 
fish <- as.data.frame(Binomial) %>%
  mutate(Binomial = as.character(Binomial)) # make binomial a character vector rather than a factor 

## manually download and wrangle the fishbase data:
spp <- read.delim("data-raw/fishbase/fb.2fspecies.tsv", sep = '\t') %>%
  mutate(Binomial = paste(Genus, Species, sep = '_'))

## clean 
spp <- filter(spp, !Genus %in% spp$Genus[grep("[0-9]", spp$Binomial)])

## look for species synonyms:
## read clpi data with information about species synonyms that Joey made:
syn <- read.csv('data-raw/cLPI_data_resolved_species.csv') %>%
  filter(Class %in% c("Fish", "Actinopterygii", "Chondrichthyes", "Holocephali",
                      "Elasmobranchii", "Myxini")) %>% # filter to fish
  select(Binomial, Binomial_resolved) %>%
  unique() %>%
  mutate(match = ifelse(as.character(Binomial) == as.character(Binomial_resolved), TRUE, FALSE)) %>%
  filter(match == FALSE) %>%
  mutate(Binomial_resolved = str_replace_all(.$Binomial_resolved, "\\n", "")) %>% # clean 
  # select only species with synonyms found
  select(-match) %>%
  rename("Binomial_clpi" = Binomial)

length(which(as.character(syn$Binomial_resolved) %in% as.character(spp$Binomial))) # 2 sp under syn names

## get species under synonyms
## change binomial in FishBase to match C-LPI binomial
spp_mismatch <- spp %>%
  filter(Binomial %in% as.character(syn$Binomial_resolved)) %>%
  left_join(., syn, by = c("Binomial" = "Binomial_resolved")) %>%
  mutate(Binomial = Binomial_clpi) %>%
  select(-Binomial_clpi)

spp <- filter(spp, !Binomial %in% as.character(syn$Binomial_resolved)) %>%
  rbind(., spp_mismatch)

## subset the entire fishbase data base to only our species:
our_spp <- spp %>%
  filter(spp$Binomial %in% clpi$Binomial)

## read in ecological data
eco <- read.delim("data-raw/fishbase/fb.2fecology.tsv", sep = '\t') %>%
  filter(!is.na(SpecCode)) %>%
  mutate(SpecCode = as.factor(SpecCode)) %>%
  select(SpecCode, Herbivory2, DietTroph, DietSeTroph, DietRemark, DietRef,
         FoodTroph, FoodSeTroph, FoodRemark, FoodRef) %>%
  group_by(SpecCode) %>%
  ## if species have more than one value, take the mean
  mutate(Herbivory2 = paste(Herbivory2, collapse = ", "),
         DietTroph = mean(as.numeric(as.character(DietTroph)), na.rm = F),
         DietSeTroph = mean(as.numeric(as.character(DietSeTroph)), na.rm = F),
         FoodTroph = mean(as.numeric(as.character(FoodTroph)), na.rm = F),
         FoodSeTroph = mean(as.numeric(as.character(FoodSeTroph)), na.rm = F),
         DietRemark = paste(DietRemark, collapse = ", "),
         DietRef = paste(DietRef, collapse = ", "),
         FoodRemark = paste(FoodRemark, collapse = ", "),
         FoodRef = paste(FoodRemark, collapse = ", ")) %>%
  ungroup() %>%
  unique()

## read in estimated traits based on models
est <- read.delim("data-raw/fishbase/fb.2festimate.tsv", sep = '\t') %>%
  mutate(SpecCode = as.factor(SpecCode)) %>%
  select(SpecCode, Troph, seTroph, TrophObserved, # trophic level and se
         Median_T, lcl_T, ucl_T, n_T) # generation time estimated as median ln(3)/K based on n_T growth studies


## info on trophic level data: https://github.com/ropensci/rfishbase/issues/199
## FoodTroph: MonteCarlo estimate of trophic level based on known food items
## DietTroph: mean or median of trophic levels derived from actual diet composition studies

length(which(unique(clpi$Binomial) %in% spp$Binomial))
## much better - now have data on 362 species

our_spp <- our_spp %>%
  left_join(., eco, by = 'SpecCode')%>%
  left_join(., est, by = 'SpecCode') %>%
  left_join(fish, ., by = "Binomial") # left join so we don't lose the 3 species not in fishbase
## note: some species duplicated because of eco table

## what traits do we get?
colnames(our_spp)

traits <- our_spp %>%
  select("Binomial", "Genus", "Species", "FBname", "SpecCode", "SpeciesRefNo", "LongevityWild",
         "Length", "LTypeMaxM", "LengthFemale", "LTypeMaxF",
         "CommonLength", "LTypeComM", "CommonLengthF", "LTypeComF", 
         "Weight", "WeightFemale", "MaxWeightRef", 
         "Median_T", "lcl_T", "ucl_T", "n_T",
         "Herbivory2", "DietTroph", "DietSeTroph", "DietRemark", "DietRef",
         "FoodTroph", "FoodSeTroph", "FoodRemark", "FoodRef",
         "Troph", "seTroph", "TrophObserved")

## Add in 'AgeMax' column from the estimate() function
traits_MaxAge <- estimate(unique(paste(traits$Genus, traits$Species, sep =' '))) %>% 
  select(Species, AgeMax) %>% 
  rename(Binomial = Species) %>% 
  mutate(Binomial = gsub(" ", "_", Binomial)) %>% 
  filter(!is.na(AgeMax)) %>% 
  group_by(Binomial) %>% 
  summarise(AgeMax = mean(AgeMax))

# Join with the rest of the traits data 
traits <- left_join(traits, traits_MaxAge, by = "Binomial")

## do some QA/QC/tidying:
## fix some dirty data spotted:
traits$LTypeMaxM[which(traits$LTypeMaxM == 'ot')] = "OT"
traits$LTypeComM[which(traits$LTypeComM == '')] = "NA"

## look at all the values of our traits
trait_summary <- summarise_traits(traits)

## notes: species that has max age 392 is a shark!!


### standardize measurements across length columns 
length_only <- traits %>%
  select(SpecCode, Binomial, Length, LTypeMaxM)

length_length <- read.delim("data-raw/fishbase/fb.2fpopll.tsv", sep = '\t') %>%
  select(SpecCode, Length1, Length2, a, b, Sex) %>%
  # get rid of juvenile
  filter(Sex != "juvenile") %>%
  rename("L_measured" = Length2, "L_desired" = Length1) %>%
  mutate(SpecCode = factor(SpecCode)) %>%
  unique() %>%
  left_join(length_only, ., by = c("SpecCode", "LTypeMaxM" = "L_measured")) %>%
  # get rid of ones that are already TL
  filter(LTypeMaxM != "TL") %>%
  # get rid of ones that we cannot convert to TL
  filter(L_desired == "TL") %>%
  # covert measures to TL
  mutate(a = as.numeric(as.character(a))) %>%
  mutate(L_converted = a + b*Length) %>%
  group_by(Binomial) %>%
  # take mean if multiple converted estimates 
  mutate(L_converted = mean(L_converted)) 

ggplot(length_length, aes(x = Length, y = L_converted)) + geom_point()

## make a new column for length where only total lengths/lengths converted to total lengths are included 
traits <- length_length %>%
  select(Binomial, SpecCode, Length, L_converted) %>%
  unique() %>%
  rename("MaxLength_TLonly" = L_converted)%>%
  left_join(traits, .) %>%
  mutate(MaxLength_TLonly = ifelse(LTypeMaxM == "TL", as.character(Length),
                                   as.character(MaxLength_TLonly)))

## make categorical trophic level trait:
traits = traits %>%
  mutate(TrophCategorical = ifelse(Troph <= 2, "herbivore", 
                                   ifelse(Troph >2 & Troph <3, "omnivore",
                                          ifelse(Troph >= 3, "carnivore", 
                                                 NA))))


# rename the columns for cleanliness and de-select a few unimportant ones
renamed <- traits
renamed <- select(renamed, c(-TrophObserved, -FBname, -SpeciesRefNo))
colnames(renamed) <- c("Binomial", "Genus", "Species", "SpecCode", "LongevityWild",
                       "MaximumLengthMale", "LTypeMaxM", "MaximumLengthFemale", "LTypeMaxF",
                       "CommonLengthMale", "LTypeComM", "CommonLengthFemale", "LTypeComF", 
                       "WeightMale", "WeightFemale", "MaxWeightRef",
                       "GenerationTime", "GenerationTime_lcl", "GenerationTime_ucl", "GenerationTime_nObs",
                       "Herbivory", "DietTrophicLevel", "DietSeTrophicLevel", "DietRemark", "DietRef",
                       "FoodTrophicLevel", "FoodSeTrophicLevel", "FoodRemark", "FoodRef",
                       "TrophicLevel", "seTrophicLevel",  "MaximumLength", "TrophicCategorical", "AgeMax")

## merge with our CLPI database:
clpi_fish <- renamed %>%
  select(-Genus, -Species) %>%
  left_join(clpi, ., by = c('Binomial'))


# write a tidy csv file with C-LPI data and traits from fishbase added to it 
write.csv(clpi_fish, "data-clean/fish_traits_CLPI.csv", row.names = FALSE)
clpi_fish = read.csv("data-clean/fish_traits_CLPI.csv")

## make a subset to merge with other types of taxa:
# This csv will include the C-LPI dataset and four additional columns: 'LifeSpan',
# 'BodySize', "BodySizeScaled', 'TrophicLevel'

# using Longevity Wild (from the species table) if there is a value present, 
# otherwise, use AgeMax (from the estimate table)
clpi_fish$LifeSpan[!is.na(clpi_fish$LongevityWild)] <- clpi_fish$LongevityWild[!is.na(clpi_fish$LongevityWild)]
clpi_fish$LifeSpan[is.na(clpi_fish$LongevityWild)] <- clpi_fish$AgeMax[is.na(clpi_fish$LongevityWild)]

# changing body size to a relative value 
# what's the maximum body size?
clpi_fish$MaximumLength <- as.numeric(clpi_fish$MaximumLength)
max_bodysize <- max(clpi_fish$MaximumLength, na.rm = TRUE)

fish_traits_subset <- clpi_fish %>% 
  select(ID:`2020`, "LifeSpan", "TrophicCategorical", "MaximumLength") %>%
  rename(TrophicLevel = TrophicCategorical,
         BodySize = MaximumLength)   

#write csv with fish C-LPI dataset and three traits columns
write_csv(fish_traits_subset, "./data-clean/fish_traits_CLPI_subset.csv")

##################################################################
##           2. get traits for all Canadian fish spp            ##
##################################################################
## get list of all Canadian fish species:
wildsp <- data.table::fread("data-raw/WildSpecies2015Data.csv")

## look for species synonyms:
## change binomial in WildSpecies list to match C-LPI binomial
wildsp_mismatch <- wildsp %>%
  filter(Binomial %in% as.character(syn$Binomial_resolved)) %>%
  left_join(., syn, by = c("Binomial" = "Binomial_resolved")) %>%
  mutate(Binomial = Binomial_clpi) %>%
  select(-Binomial_clpi)

wildsp <- filter(wildsp, !Binomial %in% as.character(syn$Binomial_resolved))%>%
  rbind(., wildsp_mismatch)

## fix wonky column names 
colnames(wildsp) <- c(str_replace_all(colnames(wildsp), '\\-', '_'))
colnames(wildsp) <- c(str_replace_all(colnames(wildsp), ' ', '_'))
colnames(wildsp) <- c(str_replace_all(colnames(wildsp), '\\___', '_'))

wildfish <- filter(wildsp, TAXONOMIC_GROUP_GROUPE_TAXONOMIQUE == "Fishes - Poissons")

length(unique(wildfish$Binomial))
# 1043 species 

length(which(unique(traits$Binomial) %in% unique(wildfish$Binomial)))
# 366/368 - 2 species missing 

missing_sp <- clpi_fish$Binomial[which(!unique(clpi_fish$Binomial) %in% unique(wildfish$Binomial))]
## these 2 species are missing from the overall Canadian species list

## get the traits from fishbase:
can_spp <- spp %>%
  filter(spp$Binomial %in% wildfish$Binomial)

length(which(unique(can_spp$Binomial) %in% unique(wildfish$Binomial)))
# 1017/1043 - 34 Canadian species missing from fishbase :(

can_spp <- can_spp %>%
  left_join(., eco, by = 'SpecCode') %>%
  left_join(., est, by = 'SpecCode') 

## quality check the Canadian species trait data:
cantraits <- can_spp %>%
  select("Binomial", "Genus", "Species", "FBname", "SpecCode", "SpeciesRefNo", "LongevityWild",
         "Length", "LTypeMaxM", "LengthFemale", "LTypeMaxF",
         "CommonLength", "LTypeComM", "CommonLengthF", "LTypeComF", 
         "Weight", "WeightFemale", "MaxWeightRef", 
         "Median_T", "lcl_T", "ucl_T", "n_T",
         "Herbivory2", "DietTroph", "DietSeTroph", "DietRemark", "DietRef",
         "FoodTroph", "FoodSeTroph", "FoodRemark", "FoodRef",
         "Troph", "seTroph", "TrophObserved")

## Add in 'AgeMax' column from the estimate() function
cantraits_MaxAge <- estimate(unique(paste(cantraits$Genus, cantraits$Species, sep =' '))) %>% 
  select(Species, AgeMax) %>% 
  rename(Binomial = Species) %>% 
  mutate(Binomial = gsub(" ", "_", Binomial)) %>% 
  filter(!is.na(AgeMax)) %>% 
  group_by(Binomial) %>% 
  summarise(AgeMax = mean(AgeMax))

# Join with the rest of the traits data 
cantraits <- left_join(cantraits, cantraits_MaxAge, by = "Binomial")

trait_summary <- summarise_traits(cantraits)

## do the same thing for max length, trophic level and age that we did for the c-lpi data
#########################################################################################
### standardize measurements across length columns 
length_only <- cantraits %>%
  select(SpecCode, Binomial, Length, LTypeMaxM)

length_length <- read.delim("data-raw/fishbase/fb.2fpopll.tsv", sep = '\t') %>%
  select(SpecCode, Length1, Length2, a, b, Sex) %>%
  # get rid of juvenile
  filter(Sex != "juvenile") %>%
  rename("L_measured" = Length2, "L_desired" = Length1) %>%
  mutate(SpecCode = factor(SpecCode)) %>%
  unique() %>%
  left_join(length_only, ., by = c("SpecCode", "LTypeMaxM" = "L_measured")) %>%
  # get rid of ones that are already TL
  filter(LTypeMaxM != "TL") %>%
  # get rid of ones that we cannot convert to TL
  filter(L_desired == "TL") %>%
  # covert measures to TL
  mutate(a = as.numeric(as.character(a))) %>%
  mutate(L_converted = a + b*Length) %>%
  group_by(Binomial) %>%
  # take mean if multiple converted estimates 
  mutate(L_converted = mean(L_converted)) 

ggplot(length_length, aes(x = Length, y = L_converted)) + geom_point()

## make a new column for length where only total lengths/lengths converted to total lengths are included 
cantraits <- length_length %>%
  select(Binomial, SpecCode, Length, L_converted) %>%
  unique() %>%
  rename("MaxLength_TLonly" = L_converted) %>%
  left_join(cantraits, .) %>%
  mutate(MaxLength_TLonly = ifelse(LTypeMaxM == "TL", as.character(Length),
                                   as.character(MaxLength_TLonly)))

## make categorical trophic level trait:
cantraits = cantraits %>%
  mutate(TrophCategorical = ifelse(Troph <= 2, "herbivore", 
                                   ifelse(Troph >2 & Troph <3, "omnivore",
                                          ifelse(Troph >= 3, "carnivore", 
                                                 NA))))



write.csv(cantraits, "data-clean/fish_traits_allcanadiansp.csv", row.names = FALSE)

alltraits <- spp %>%
  left_join(., eco, by = 'SpecCode')%>%
  left_join(., est, by = 'SpecCode') %>%
  select("Binomial", "Genus", "Species", "FBname", "SpecCode", "SpeciesRefNo", "LongevityWild",
         "Length", "LTypeMaxM", "LengthFemale", "LTypeMaxF",
         "CommonLength", "LTypeComM", "CommonLengthF", "LTypeComF", 
         "Weight", "WeightFemale", "MaxWeightRef", 
         "Median_T", "lcl_T", "ucl_T", "n_T",
         "Herbivory2", "DietTroph", "DietSeTroph", "DietRemark", "DietRef",
         "FoodTroph", "FoodSeTroph", "FoodRemark", "FoodRef",
         "Troph", "seTroph", "TrophObserved")


## Add in 'AgeMax' column from the estimate() function
alltraits_MaxAge <- estimate(unique(paste(alltraits$Genus, alltraits$Species, sep =' '))) %>% 
  select(Species, AgeMax) %>% 
  rename(Binomial = Species) %>% 
  mutate(Binomial = gsub(" ", "_", Binomial)) %>% 
  filter(!is.na(AgeMax)) %>% 
  group_by(Binomial) %>% 
  summarise(AgeMax = mean(AgeMax))

# Join with the rest of the traits data 
alltraits <- left_join(alltraits, alltraits_MaxAge, by = "Binomial")

length_only <- alltraits %>%
  select(SpecCode, Binomial, Length, LTypeMaxM)

length_length <- read.delim("data-raw/fishbase/fb.2fpopll.tsv", sep = '\t') %>%
  select(SpecCode, Length1, Length2, a, b, Sex) %>%
  # get rid of juvenile
  filter(Sex != "juvenile") %>%
  rename("L_measured" = Length2, "L_desired" = Length1) %>%
  mutate(SpecCode = factor(SpecCode)) %>%
  unique() %>%
  left_join(length_only, ., by = c("SpecCode", "LTypeMaxM" = "L_measured")) %>%
  # get rid of ones that are already TL
  filter(LTypeMaxM != "TL") %>%
  # get rid of ones that we cannot convert to TL
  filter(L_desired == "TL") %>%
  # covert measures to TL
  mutate(a = as.numeric(as.character(a))) %>%
  mutate(L_converted = a + b*Length) %>%
  group_by(Binomial) %>%
  # take mean if multiple converted estimates 
  mutate(L_converted = mean(L_converted)) 

ggplot(length_length, aes(x = Length, y = L_converted)) + geom_point()

## make a new column for length where only total lengths/lengths converted to total lengths are included 
alltraits <- length_length %>%
  select(Binomial, SpecCode, Length, L_converted) %>%
  unique() %>%
  rename("MaxLength_TLonly" = L_converted) %>%
  left_join(alltraits, .) %>%
  mutate(MaxLength_TLonly = ifelse(LTypeMaxM == "TL", as.character(Length),
                                   as.character(MaxLength_TLonly)))

## make categorical trophic level trait:
alltraits = alltraits %>%
  mutate(TrophCategorical = ifelse(Troph <= 2, "herbivore", 
                                   ifelse(Troph >2 & Troph <3, "omnivore",
                                          ifelse(Troph >= 3, "carnivore", 
                                                 NA))))



write.csv(alltraits, "data-clean/fish_traits_global.csv", row.names = FALSE)


## combine C-LPI traits, Canadian species traits and all species traits
traits$collection = "C-LPI"
cantraits$collection = "Canadian Wild Species"
alltraits$collection = "All species"

all <- rbind(traits, cantraits, alltraits)

write.csv(all, "data-clean/fish_traits_CLPI_canadiansp_global.csv")

####################################################
##       pull in more data from other sources     ##
####################################################
## IUCN habitat data 
habitat_data_all_df <- readRDS("data-raw/habitat_data_canada.Rds")
## clpi with RL IDs
clpi_rl <- fread("data-raw/CIEE_LPI_dataset_RL_info.csv", na.strings = "NA")  %>%
  filter(Binomial %in% clpi_fish$Binomial) # filter to fish

i=1
while(i < length(habitat_data_all_df)) {
  cur = habitat_data_all_df[[i]]
  
  if(!is.null(cur)) {
    if(!is_empty(cur$result)) {
      if(i == 1) {
        cum = cur$result %>%
          mutate(RL_ID = cur$id)
      }
      else {
        temp = cur$result %>%
          mutate(RL_ID = cur$id)
        cum = rbind(cum, temp)
      }
    }
  }
  i = i + 1
}


hab = Filter(function(x) nrow(x)[1] > 0, habitat_data_all_df)

## merge data on redlist ID "RL_ID" 
hab <- bind_rows(habitat_data_all_df)

colnames(clpi_rl)  



#subset to fish
