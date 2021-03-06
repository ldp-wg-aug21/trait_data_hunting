# trait_data_hunting

****Trait Database for Species within the C-LPI****

Species traits (according to bionomial Latin name) have been appended to species with temporal population trends, as captured within the Canada Living Planet Index dataset. The information can be used for a variety of purposes. 

Here, we explore two main questions:
Are the traits of the species within the C-LPI dataset representative of the traits of the broader vertebrate subphylum in Canada?
Are there specific traits that inform/predict trends in vertebrate abundance?



**Species Ranges**

Ranges (polygons) for Canadian vertebrate species were downladed from the IUCN Red List (https://www.iucnredlist.org/). Ranges were clipped to Canada (and its associated EEZ to account for marine species). The proportion of the global range found in Canada was calculated. Data also includes the IUCN ID, scientific name, common name and Red List category.

| Trait | Description |
|------:|:-------|
| Area_km2 | IUCN gloabl range in km2 |
| Clip_area_km2 | IUCN range within Canada/EEZ in km2  |
| Proportion_CAN | Proportion of range within Canada/EEZ  |
| Range_CAN | Percent of range within Canada/EEZ  |


**Environmental Variables for Species Ranges**

For each species range in Canada, mean values for environmental variables were calculated. Topograhpic variables were derived from the CanaDEM product (https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333) and climate variables were derived from ClimateNA (https://adaptwest.databasin.org/pages/adaptwest-climatena/).

Degree days are somewhat less intuitive (i.e., not representative of calendar days). Details on this variable can be accessed here: https://www.degreedays.net/calculation


**Comparing Distributions of Traits**

Species listed within the Wild Species Reports were compiled into a complimentary dataset to compare traits of species within the C-LPI, to the broader group of species within Canada. The list of species was derived from the 2015 Wild Species Reports (https://www.wildspecies.ca/reports) and includes native Canadian species with applicable conservation status (i.e., exotic species, hybrids and accidental species under the NatureServe rank of ???not applicable??? were excluded). 

**Habitat data from IUCN**

The script [scripts/get_iucn_habitats.Rmd](scripts/get_iucn_habitats.Rmd) uses the mappings between the canadian population data and the red list in [data-raw/CIEE_LPI_dataset_RL_info.csv](data-raw/CIEE_LPI_dataset_RL_info.csv) to extract habitat data using the rredlist package. 

The extracted data is saved as an Rds file [data-raw/habitat_data_canada.Rds](data-raw/habitat_data_canada.Rds) containing extracted habitat data for each canadian species in the LPI data that can be found on the IUCN red list. It is in the form of a list with each element containing the red-list ID (id) and the habitat data (result). This matches the raw form returned from the rredlist package.

This is then converted to tabular format in [data-raw/habitat_data_all_df.Rds](data-raw/habitat_data_all_df.Rds)

And finally summarised into number of habitats and number of high-level habitats in [data-clean/canadian_lpi_data_with_habitat.csv](data-clean/canadian_lpi_data_with_habitat.csv)

**Mammals**

Two life history parameters, gestation period and maximum longevity, were retrieved from Amniota (https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/15-0846R.1) using the R package traitdata. Body size data were extracted from Elton Mammal (https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/13-1917.1). Mammal trophic level was calculated using diet composition from Elton Mammal dataset where herbivore was defined as species associated with a 100% vegetarian diet that including plants, seeds, nectar and fruit; carnivore is species that has a full meat-based diet, including: various invertebrates, vertebrates, herp, fish, and scavenge; and omnivore are species that consumes a mixed diet that include both veg- and meat-based components. 

The script responsible for extracting this data for all Canadian Wild Species (C-vertebrates dataset) is [scripts/extract_wild_mammal_traits.R](scripts/extract_wild_mammal_traits.R). This script generates the dataset [mammals_traits_allcanadiansp.csv](data-clean/mammals_traits_allcanadiansp.csv).

The script responsible for extracting this data for the species in the Canadian LPI (C-LPI) is [scripts/extract_mammal_traits.R](scripts/extract_mammal_traits.R). This script generates the dataset [mammals_traits_clpi.csv](data-clean/mammals_traits_allcanadiansp.csv).

The cleaned dataset with both mammal traits and C-LPI data can be found here as a .csv file here: [data-clean/traits-specific-mammals.csv](data-clean/traits-specific-mammals.csv).


| Trait | Description |
|------:|:-------|
| Body Mass (g) | Adult body mass, in grams, which was aggregated from multiple records in a given species from the Elton Mammal dataset |
| Trophic Level | Trophic level (herbivor, carnivore, omnivore) calculated based on diet composition from Elton Mammal dataset.  |
| Gestation Period (days) | Median gestation period (days) calculated from a variety of publicly available databases, data tables embedded in individual papers and books, and species-specific studies by experts. |
| Maximum Longevity (years) | Median max longevity (years) calculated from a variety of publicly available databases, data tables embedded in individual papers and books, and species-specific studies by experts. |


**Herpetofauna**

The script [herps_data_extraction.R](scripts/herps_data_extraction.R) extracts trait data for species in each the LPI Canadian dataset and the Canadian species list. Data was extracted from (1) amphibio (https://www.nature.com/articles/sdata2017123) (2) amniota (https://esajournals.onlinelibrary.wiley.com/doi/10.1890/15-0846R.1) (3) lizard_traits (https://onlinelibrary.wiley.com/doi/abs/10.1111/geb.12773) (4) an amphibian allometry database (https://onlinelibrary.wiley.com/doi/full/10.1111/1749-4877.12268) (5) an amniote diet dataset (https://www.science.org/doi/10.1126/sciadv.abb8458) and (6) SquamataBase (https://bdj.pensoft.net/article/49943/)

Data in SquamataBase was listed for individuals. To estimate diet, I calculated the proportion of prey items listed for a species that are animals. This proportion equaled one for all LPI species in that dataset, so all species were then listed as carnivores. For SVL and body mass estimates from SquamataBase, I averaged the measurements listed for adult individuals for a species. Multiple entries were listed for some species in the amphibian allometry database. I selected the value for each trait for each species that had the highest sample size. For offspring size, clutch size, and age at maturity, minimum and maximum values listed in amphibio were averaged to produce a single estimate for each species. For age at maturity, male and female values listed in amniota were also averaged to produce a species estimate.

Trait data that were comparable across datasets (example ??? body mass in g) were combined. This was done hierarchically. For example, body mass was first extracted from amphibio, then from amniota for species where amphibio had no body mass data.

The set of traits were reasonable coverage across species were:
| Trait | Description |
|------:|:-------|
| body_mass_g | Mean weight for adults in g |
| SVL_mm | Mean snout-vent length for adults in mm |
| longevity_years | Maximum recorded longevity in years |
| diet | Diet category: ???herbivore???,???omnivore???, or ???carnivore??? |
| clutch_size_n | Mean clutch or litter size in number of offspring |
| offspring_size_mm | Mean size of offspring or hatchlings in mm |
| age_maturity_years | Mean age of maturity in years |

Species taxonomy and traits with reasonable data coverage across species were saved in [herp_traits_all.csv](data_clean/herp_traits_all.csv) for LPI species and [herps_canadian_sp.csv](data_clean/herps_canadian_sp.csv) for all Canadian species.

Species taxonomy, body size, longevity, and diet for LPI species were saved in [herp_traits_subset.csv](data_clean/herp_traits_subset.csv)


**Birds**

The following trait data for birds was extracted from these data sources:

1) the [Amniote database] (https://figshare.com/collections/An_amniote_life-history_database_to_perform_comparative_analyses_with_birds_mammals_and_reptiles/3308127) (i.e., maximum longevity, longevity, and adult body mass),

2) a [Zenodo data repository] (https://zenodo.org/record/3832215#.YTF8zsZE1ap) curated by Sheard et al. 2020. Nature Communications (i.e., diet and hand-wing index), 

3) and the [Supplement Data from NABCI's 'The State of Canada's Birds 2019'] (nabci.net/resources/state-of-canadas-birds-2019) (i.e., functional groups)

The script [extract_bird_traits.r](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/scripts/extract_bird_traits.r) extracts the seven candidate traits from the three data sources and then subsets to only bird species in the C-LPI database. The output from this script is [clpi_bird_traits.rds](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/data-clean/LPI_birds_traits.rds), where each row represents a unique bird species within the C-LPI database and additional columns represent the candidate traits. 

| Trait | Description |
|------:|:-------|
| Adult body mass | Adult body mass, in grams, which was aggregrated from multiple records in a given species from the Amniote database
| Maximum longevity | Maximum published longevity recorded for a given species, in years, where mean value was calculated from multiple records in a given species from the Amniote database 
| Longevity | Published longevity recorded for a given species, in years, where mean value was calculated from multiple records in a given species from the Amniote database 
| Hand wing index | A proxy for dispersal ability, represented as 100*(DK/Lw), where DK is Kipp's distance (the distance between the tip of the first secondary feather and the tip of the longest feather) and Lw is wing length
| Diet | Trophic level (i.e., vertebrates, plants, omnivore, invertebrates, fruit, nectar, and seeds) comprised of sources from Eltontraits and updated literature (since 2014) curated by Sheard et al. (2020). 
| Functional group | Functional groups (i.e., aerial insectivore, birds of prey, forest, grasslands, seabirds, waterfowl, wetlands, shore, and others) comprised of different data sources compiled in the supplementary dataset of NABCI's 'The State of Canada's Bird 2019' report. It is important to note that a single species can have multiple functional groups in this aggregrated dataset (e.g., shore + grasslands)

**Fish**

[Trait data for fish taxa](https://github.com/ldp-wg-aug21/trait_data_hunting/tree/main/data-raw/fishbase) were extracted from FishBase (http://www.fishbase.org/search.php) and subset to only include species in the the C-LPI database in the script [extract_fish_traits.R](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/scripts/01_querying-fishbase.R). This script produces the output [clpi_fishbase_merge.csv](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/data-clean/fish/clpi_fishbase_merge.csv), which is a subset of the original C-LPI database containing only fish species with additional columns for species traits. In this data, each row represents a unique Canadian population. 

To compare traits of the subset of fish species included in the C-LPI to traits of all Canadian fish species, the script [extract_fish_traits.R](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/scripts/extract_fish_traits.R) also extracts traits from FishBase for all fish species in the [Wild Species List](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/data-raw/WildSpecies2015Data.csv). These full Canadian fish species list along with their traits are found in [fishbase_canadian-spp.csv](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/data-clean/fish/fishbase_canadian-spp.csv), while all data to compare C-LPI species, all Canadian species, and all species globally are found in [fishbase_traits-for-comparison.csv](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/data-clean/fish/fishbase_traits-for-comparison.csv).

Available fish traits from FishBase were filtered down to include only traits of interest, including:
| Trait | Description |
|------:|:-------|
|   SpecCode | Unique fishbase identifier |
|   LongevityWild | Maximum published longevity recorded from a wild individual in years |
|   MaximumLengthMale |   Maximum published body length, in cm, of a male or unsexed fish measured using method described by variable  *LTypeMaxM*  |
|   MaximumLengthFemale |   Maximum published body length, in cm, of a female fish measured using method described by variable  *LTypeMaxF*  |
|   MaximumLength |   Maximum _total_ body length, in cm, of a male or unsexed fish either from an estimate published in literature or converted from another body length measurement type using length-length conversion factors  |
|   CommonLengthMale |   Common published body length, in cm, of a male or unsexed fish measured using method described by variable  *LTypeComM*  |
|   CommonLengthFemale |   Common published body length, in cm, of a female fish measured using method described by variable  *LTypeComF*  |
|   WeightMale |   Maximum published weight, in g, of a male or unsexed fish |
|   WeightFemale |   Maximum published weight, in g, of a female fish  |
|   GenerationTime |   Median generation time, in years, estimated as median ln(3)/K based on growth studies  |
|   DietTrophicLevel |   Trophic level estimated based on diet composition studies  |
|   FoodTrophicLevel |   MonteCarlo estimate of trophic level based on known food items |
|   TrophicLevel |   Trophic level estimate where if available, value of *DietTrophiicLevel* is used but if not, value of *FoodTrophicLevel* is used  |
|   Herbivory |   Description of the general trophic level occupied by a species |
|   TrophCategorical | Category of trophic level, where 1 = herbivore, 2 = omnivore, and 3 = carnivore based on *TrophicLevel* |


We then subset the fish trait data to include only the 3 general traits of interest for all taxa (BodySize, TrophicLevel, and LifeSpan) by creating a dataset [fish_traits_subset.csv](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/data-clean/fish_traits_subset.csv) with only the variables MaxLength_TLonly, TrophCategorical and LongevityWild. In this data, each row represents a unique species in the C-LPI dataset and it's traits. MaxLength_TLonly represents the total length (cm) measured, if only standard length or fork length was measured, conversion factors from fishbase for that species were used to convert to total length. LifeSpan values came from LongevityWild where available, otherwise we used values from  AgeMax.

## Merging the trait datasets

We selected three traits to gather for *all* taxon, which can be found in 

| Trait | Description |
|------:|:-------|
|   UUID | Univerally unique identifier for the species  |
|   BodySize |   Body mass or length  normalised between 0 and 1 within each taxon group  |
|  TrophicLevel | Category of trophic level, where 1 = herbivore, 2 = omnivore, and 3 = carnivore  |
|    LifeSpan  |   Maximum longevity in years |

To generate this dataset, we do the following steps, which are scripted in [merge-datasets](https://github.com/ldp-wg-aug21/trait_data_hunting/tree/main/scripts/merge-datasets):

1. [01_specific-taxon.R](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/scripts/merge-datasets/01_specific-taxon.R): This script wrangles each taxon-specific dataset to ensure it includes the common traits described above. Additional traits which were interesting to collect for the taxons were retained in the resulting datasets. This script generates one dataset per taxon, stored in [data-clean/](https://github.com/ldp-wg-aug21/trait_data_hunting/tree/main/data-clean):
  - `traits-specific-mammals.csv`
  - `traits-specific-birds.csv`
  - `traits-specific-fish.csv`
  - `traits-specific-herps.csv`
  
  Each of these datasets is accompanied by a metadata file generated in `01_specific-taxon.R` to describe each column, including both the common traits and the additional ones. These files are stored in [data-clean/metadata/](https://github.com/ldp-wg-aug21/trait_data_hunting/tree/main/data-clean/metadata).
  
  2. [02_generate_UUID.R](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/scripts/merge-datasets/02_generate_UUID.R): Each species is then assigned a universally unique identifier (UUID). The trait-specific datasets are then overwritten with a version that includes the UUID. 
  
  3. [03_merge_all.R](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/scripts/merge-datasets/03_merge_all.R): This script subsets the taxon-specific trait datasets to the three common traits described above, and then merges them together into one species-level dataset: [traits-all.csv](https://github.com/ldp-wg-aug21/trait_data_hunting/blob/main/data-clean/traits-all.csv).
  
