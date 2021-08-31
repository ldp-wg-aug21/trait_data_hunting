# Extract Pantheria -------------------------------------------------------
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


# Wild Species QCQA -------------------------------------------------------------
# Wild <- read.csv("data-raw/WildSpecies2015Data_UTF8.csv")
# Wild <- tidyr::separate(Wild, SCIENTIFIC.NAME...NOM.SCIENTIFIQUE, c("x", "y")) 
# Wild <- tidyr::unite(Wild, "Binomial", c("x", "y"), sep = "_") 
# write.csv(Wild, "data-raw/WildSpecies2015Data_UTF8_Binomial.csv")

Wild <- read.csv("data-raw/WildSpecies2015Data_UTF8_Binomial.csv")
# unique(Wild$TAXONOMIC.GROUP...GROUPE.TAXONOMIQUE)
# 
unique(Wild$TAXONOMIC.GROUP...GROUPE.TAXONOMIQUE)[34]
# "Mammals - MammifËres"


Wild_Mammal <- dplyr::filter(Wild, Wild$TAXONOMIC.GROUP...GROUPE.TAXONOMIQUE == "Mammals - MammifËres" )
Wild_Mammal$Wild <- "Yes"
Wild_Mammal_1 <- dplyr::select(Wild_Mammal, c("Binomial","Wild"))
# write.csv(Wild_Mammal_1, "WildMammal.csv")

lpd_traits_wild <- left_join(lpd_traits, Wild_Mammal_1, by = "Binomial")
# write.csv(lpd_traits_wild, "lpd_traits_wild.csv")


Wild_Mammal_Tax <- unique(Wild_Mammal_1$Binomial)
length(Wild_Mammal_Tax)
# 222

lpd_Mammal_tax <- unique(lpd_traits$Binomial[lpd_traits$Class == "Mammalia"])
length(lpd_Mammal_tax)
# 98

mutual <- dplyr::intersect(Wild_Mammal_Tax, lpd_Mammal_tax)
length(mutual)
# 88


library(VennDiagram)
grid.newpage()
draw.pairwise.venn(222,98,88, category = c("Wild Mammals 2015", "IPD Mammals"),
                   lty = rep("blank",2), fill = c("light blue", "pink"), 
                   sep.dist = -0.1, rotation.degree = 300,
                   alpha = rep(0.5, 2), cat.pos = c(0, 20), cat.dist = rep(0.025, 1))



# QCQA: LPD vs with Priority Wild Mammals ----------------------------------
Wild_Mammal_Priority <- read.csv("data-raw/WildMammalPriority.csv")

Wild_Mammal_Priority <- tidyr::separate(Wild_Mammal_Priority, 
                                        SCIENTIFIC.NAME...NOM.SCIENTIFIQUE, c("Order", "Family"))

Wild_Mammal_Priority <- tidyr::unite(Wild_Mammal_Priority, "Binomial", 
                                     c("Order", "Family"), sep = "_")

Wild_Mammal_Priority_Tax <- unique(Wild_Mammal_Priority$Binomial)

length(Wild_Mammal_Priority_Tax)
# 23

mutual_pri_lpd <- dplyr::intersect(Wild_Mammal_Priority_Tax, lpd_Mammal_tax)
length(mutual_pri_lpd)
# 6

mutual_pri_lpd <- dplyr::intersect(Wild_Mammal_Priority_Tax, lpd_Mammal_tax)
length(mutual_pri_lpd)
# 6

mutual_pri_wild <- dplyr::intersect(Wild_Mammal_Priority_Tax, Wild_Mammal_Tax)
length(mutual_pri_wild)
# 23

mutual_all <- dplyr::intersect(mutual_pri_wild, lpd_Mammal_tax)
length(mutual_all)
# 6
