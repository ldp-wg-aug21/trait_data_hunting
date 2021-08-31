# trait_data_hunting

Data and scripts to answer the question: are there specific traits that inform/predict trends in vertebrate abundance?


**Species Ranges**

Ranges (polygons) for Canadian vertebrate species were downladed from the IUCN Red List (https://www.iucnredlist.org/). Ranges were clipped to Canada (and its associated EEZ to account for marine species). The proportion of the global range found in Canada was calculated.

Area_km2 = IUCN gloabl range in km2 Clip_area_km2 = IUCN range within Canada/EEZ in km2 Proportion_CAN = Proportion of range within Canada/EEZ Range_CAN = Percent of range within Canada/EEZ

Data also includes the IUCN ID, scientific name, common name and Red List category.


**Environmental Variables for Species Ranges**

For each species range in Canada, mean values for environmental variables were calculated. Topograhpic variables were derived from the CanaDEM product (https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333) and climate variables were derived from ClimateNA (https://adaptwest.databasin.org/pages/adaptwest-climatena/).

Degree days are somewhat less intuitive (i.e., not representative of calendar days). Details on this variable can be accessed here: https://www.degreedays.net/calculation
