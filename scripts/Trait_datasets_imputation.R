# Load packages
library(tidyverse)
library(data.table)
library(ggpubr)
library(effectsize)
library(traitdata)
#library(patchwork)
library(missForest)
#library(randomForest)
library(here)
library(caret)
library(DataExplorer)
library(skimr)
library(corrplot)

#Open data
here()
#set up data for Canadian + C-LPI species
herps<-fread("data-clean/herps_traits_allcanadiansp.csv", stringsAsFactors=TRUE)
fish<-fread("data-clean/fish_traits_CLPI_canadiansp_global.csv", stringsAsFactors=TRUE) #added stringsAsFactors=TRUE as in the new R version this is not done automatically
birds<-fread("data-clean/birds_traits_globalLPI.csv", stringsAsFactors=TRUE)
mammals<-fread("data-clean/mammals_traits_allcanadiansp.csv", stringsAsFactors=TRUE)

lpi.dat<-fread("data-raw/CIEE_LPI_dataset.csv")
wild.dat<-fread("data-raw/WildSpecies2015Data.csv")
lpi.dat2<-fread("data-raw/cLPI_data_resolved_species.csv")



###Create a folder to save the results of the imputation
dir.create(file.path(getwd(), 'imputation results'), recursive = TRUE)

## Fish data ----

# Not in Canada but currently in C-LPI (need to eliminate)
fish<-subset(fish, Binomial!="Bathyraja_aleutica" & Binomial!="Bathyraja_minispinosa")
dim(fish)
# plot(fish) takes forever

###Initial data exploration
fish %>% skimr::skim()
fish %>% 
  plot_missing()

fish %>% 
  DataExplorer::plot_histogram()

###Checking species that have binomial but are missing genus and species name
check <- fish[is.na(fish$Genus) == TRUE]
check

###Look at number of NAs per column
colSums(is.na(fish))
###Calculating percentage of missing data in each colum
fish_NA_percentage <- fish %>% summarize_all(funs(sum(is.na(.)) / length(.)))
write.csv(fish_NA_percentage, "imputation results/fish_NA_percentage.csv")

###Exclude variables we don't want to use
fish_to_impute <- fish
fish_to_impute <- fish_to_impute %>% select(c("Binomial","LongevityWild","Length","LengthFemale","CommonLength","CommonLengthF","Weight","WeightFemale","TrophCategorical","MaxLength_TLonly"))

###Change class of variables that don't seem to be read in correctly 
fish_to_impute$LengthFemale <- as.numeric(fish_to_impute$LengthFemale)

###Check for highly correlated variables?####################
###Create a correlation matrix
descrCor <-cor(fish_to_impute[,-c("Binomial", "TrophCategorical")], y = NULL, use = "complete.obs",
               method = c("pearson", "kendall", "spearman"))
#descrCor <-  cor(BM_numeric[,-1])
summary(descrCor[upper.tri(descrCor)])

###Visualise correlation and save as pdf in figures folder
# Opening the graphical device
pdf("figures/corrplot_fish.pdf")
# Creating a plot
corrplot(descrCor)
# Closing the graphical device
dev.off() 

###Detect highly correlated variables and exclude them?
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
highlyCorDescr ###as this is zero there is no need to delete any of the columns

###Impute missing data
fish_imputed <- missForest(fish_to_impute[,2:10], verbose = TRUE) ###impute missing data 
fish_imputed$OOBerror ###Error for numerical(NRMSE) and categorical (PFC) variables
summary(fish_imputed$ximp) ###Sumamry of imputed data
###check there are no missing data after imputation
fish_imputed <- cbind(fish_to_impute[,1], fish_imputed$ximp) ###Join Binomial back together (had to exclude it as missforest can't handle categorical variables with more than 53 levels)
colSums(is.na(fish_imputed))

######Look at the data and save it to csv
dim(fish_imputed) # check dataset
names(fish_imputed)
summary(fish_imputed)
typeof(fish_imputed)
str(fish_imputed)
###Look at the data
dim(fish_final) # check dataset
names(fish_final)
summary(fish_final)
typeof(fish_final)
str(fish_final)
write.csv(fish_imputed, "imputation results/fish_imputed_dataset.csv")


# Birds data ----

###Initial data exploration
birds %>% skimr::skim()
birds %>% 
  plot_missing()

birds %>% 
  DataExplorer::plot_histogram()

###Checking species that have binomial but are missing genus and species name
check <- birds[is.na(birds$Genus) == TRUE]
check # there are none

###Look at number of NAs per column
colSums(is.na(birds))
###Calculating percentage of missing data in each colum
birds_NA_percentage <- birds %>% summarize_all(funs(sum(is.na(.)) / length(.)))
write.csv(birds_NA_percentage, "imputation results/birds_NA_percentage.csv")

###Exclude variables we don't want to use
birds_to_impute <- birds
birds_to_impute <- birds_to_impute %>% select(c("binomial","hwi","diet","mean_adult_body_mass_g","mean_max_longevity_y","mean_longevity_y"))

###Check for highly correlated variables?####################
###Create a correlation matrix
descrCor <-cor(birds_to_impute[,-c("binomial", "diet")], y = NULL, use = "complete.obs",
               method = c("pearson", "kendall", "spearman"))
#descrCor <-  cor(BM_numeric[,-1])
summary(descrCor[upper.tri(descrCor)])

###Visualise correlation and save as pdf in figures folder
# Opening the graphical device
pdf("figures/corrplot_birds.pdf")
# Creating a plot
corrplot(descrCor)
# Closing the graphical device
dev.off() 

###Detect highly correlated variables and exclude them?
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
highlyCorDescr ###only the two longevity measures are highly correlated

###Impute missing data
birds_imputed <- missForest(birds_to_impute[,2:6], verbose = TRUE) ###impute missing data 
birds_imputed$OOBerror ###Error for numerical(NRMSE) and categorical (PFC) variables
summary(birds_imputed$ximp) ###Sumamry of imputed data
###check there are no missing data after imputation
birds_imputed <- cbind(birds_to_impute[,1], birds_imputed$ximp) ###Join Binomial back together (had to exclude it as missforest can't handle categorical variables with more than 53 levels)
colSums(is.na(birds_imputed))

######Look at the data and save it to csv
dim(birds_imputed) # check dataset
names(birds_imputed)
summary(birds_imputed)
typeof(birds_imputed)
str(birds_imputed)
write.csv(birds_imputed, "imputation results/birds_imputed_dataset.csv")

## Herps data ----

###Initial data exploration
herps %>% skimr::skim()
herps %>% 
  plot_missing()

herps %>% 
  DataExplorer::plot_histogram()

###Checking species that have binomial but are missing genus and species name
check <- herps[is.na(herps$Genus) == TRUE]
check # there are none

###Look at number of NAs per column
colSums(is.na(herps))
###Calculating percentage of missing data in each colum
herps_NA_percentage <- herps %>% summarize_all(funs(sum(is.na(.)) / length(.)))
write.csv(herps_NA_percentage, "imputation results/herps_NA_percentage.csv")

###Exclude variables we don't want to use
herps_to_impute <- herps
herps_to_impute <- herps_to_impute %>% select(c("Binomial","longevity_years","body_mass_g","SVL_mm","diet","clutch_size_n","offspring_size_mm","age_maturity_years"))

###Check for highly correlated variables?####################
###Create a correlation matrix
descrCor <-cor(herps_to_impute[,-c("Binomial", "diet")], y = NULL, use = "complete.obs",
               method = c("pearson", "kendall", "spearman"))
#descrCor <-  cor(BM_numeric[,-1])
summary(descrCor[upper.tri(descrCor)])

###Visualise correlation and save as pdf in figures folder
# Opening the graphical device
pdf("figures/corrplot_herps.pdf")
# Creating a plot
corrplot(descrCor)
# Closing the graphical device
dev.off() 

###Detect highly correlated variables and exclude them?
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
highlyCorDescr ###only the two longevity measures are highly correlated

###Impute missing data
herps_imputed <- missForest(herps_to_impute[,2:8], verbose = TRUE) ###impute missing data 
herps_imputed$OOBerror ###Error for numerical(NRMSE) and categorical (PFC) variables
summary(herps_imputed$ximp) ###Sumamry of imputed data
###check there are no missing data after imputation
herps_imputed <- cbind(herps_to_impute[,1], herps_imputed$ximp) ###Join Binomial back together (had to exclude it as missforest can't handle categorical variables with more than 53 levels)
colSums(is.na(herps_imputed))

######Look at the data and save it to csv
dim(herps_imputed) # check dataset
names(herps_imputed)
summary(herps_imputed)
typeof(herps_imputed)
str(herps_imputed)
write.csv(herps_imputed, "imputation results/herps_imputed_dataset.csv")

# Mammals data ----
###Initial data exploration
mammals %>% skimr::skim()
mammals %>% 
  plot_missing()

mammals %>% 
  DataExplorer::plot_histogram()

###Checking species that have binomial but are missing genus and species name
check <- mammals[is.na(mammals$Genus) == TRUE]
check # there are none

###Look at number of NAs per column
colSums(is.na(mammals))
###Calculating percentage of missing data in each colum
mammals_NA_percentage <- mammals %>% summarize_all(funs(sum(is.na(.)) / length(.)))
write.csv(mammals_NA_percentage, "imputation results/mammals_NA_percentage.csv")

###Exclude variables we don't want to use
mammals_to_impute <- mammals
mammals_to_impute <- mammals_to_impute %>% select(c("Binomial","elton_BodyMass.Value_g","Trophic_Level","amniota_gestation_d","amniota_maximum_longevity_y"))

###Check for highly correlated variables?####################
###Create a correlation matrix
descrCor <-cor(mammals_to_impute[,-c("Binomial", "Trophic_Level")], y = NULL, use = "complete.obs",
               method = c("pearson", "kendall", "spearman"))
#descrCor <-  cor(BM_numeric[,-1])
summary(descrCor[upper.tri(descrCor)])

###Visualise correlation and save as pdf in figures folder
# Opening the graphical device
pdf("figures/corrplot_mammals.pdf")
# Creating a plot
corrplot(descrCor)
# Closing the graphical device
dev.off() 

###Detect highly correlated variables and exclude them?
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
highlyCorDescr ###only the two longevity measures are highly correlated

###Impute missing data
mammals_imputed <- missForest(mammals_to_impute[,2:5], verbose = TRUE) ###impute missing data 
mammals_imputed$OOBerror ###Error for numerical(NRMSE) and categorical (PFC) variables
summary(mammals_imputed$ximp) ###Sumamry of imputed data
###check there are no missing data after imputation
mammals_imputed <- cbind(mammals_to_impute[,1], mammals_imputed$ximp) ###Join Binomial back together (had to exclude it as missforest can't handle categorical variables with more than 53 levels)
colSums(is.na(mammals_imputed))

######Look at the data and save it to csv
dim(mammals_imputed) # check dataset
names(mammals_imputed)
summary(mammals_imputed)
typeof(mammals_imputed)
str(mammals_imputed)
write.csv(mammals_imputed, "imputation results/mammals_imputed_dataset.csv")













