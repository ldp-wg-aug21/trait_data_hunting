library(devtools)
library(data.table)
library(tidyr)
library(dplyr)
library(rlpi)


lpi_canada = fread("data-raw/CIEE_LPI_dataset.csv", na.strings = "NA")
EnVar = fread("data-raw/EnVar_Ranges.csv")
Ranges = fread("data-raw/SpeciesRanges.csv")

EnVar$Binomial <- EnVar$BinomialName
Ranges$Binomial <- Ranges$BinomialName

lpi_rangedata <- merge(Ranges, EnVar, by=c("Binomial"), all.x = TRUE)

lpi_range_combined <- merge(lpi_canada, lpi_rangedata, by=c("Binomial"), all.x = TRUE)
colnames(lpi_range_combined)