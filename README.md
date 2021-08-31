# trait_data_hunting

****Trait Database for Species within the C-LPI****

Species traits (according to bionomial Latin name) have been appended to species with temporal population trends, as captured within the Canada Living Planet Index dataset. The information can be used for a variety of purposes. 

Here, we explore two main questions:
Are the traits of the species within the C-LPI dataset representative of the traits of the broader vertebrate subphylum in Canada?
Are there specific traits that inform/predict trends in vertebrate abundance?



**Species Ranges**

Ranges (polygons) for Canadian vertebrate species were downladed from the IUCN Red List (https://www.iucnredlist.org/). Ranges were clipped to Canada (and its associated EEZ to account for marine species). The proportion of the global range found in Canada was calculated.

Area_km2 = IUCN gloabl range in km2 

Clip_area_km2 = IUCN range within Canada/EEZ in km2 

Proportion_CAN = Proportion of range within Canada/EEZ 

Range_CAN = Percent of range within Canada/EEZ


Data also includes the IUCN ID, scientific name, common name and Red List category.


**Environmental Variables for Species Ranges**

For each species range in Canada, mean values for environmental variables were calculated. Topograhpic variables were derived from the CanaDEM product (https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333) and climate variables were derived from ClimateNA (https://adaptwest.databasin.org/pages/adaptwest-climatena/).

Degree days are somewhat less intuitive (i.e., not representative of calendar days). Details on this variable can be accessed here: https://www.degreedays.net/calculation


**Comparing Distributions of Traits**

Species listed within the Wild Species Reports were compiled into a complimentary dataset to compare traits of species within the C-LPI, to the broader group of species within Canada. The list of species was derived from the 2015 Wild Species Reports (https://www.wildspecies.ca/reports) and includes native Canadian species with applicable conservation status (i.e., exotic species, hybrids and accidental species under the NatureServe rank of “not applicable” were excluded). 


**Mammals**


**Herpetofauna**


**Birds**


**Fish**


