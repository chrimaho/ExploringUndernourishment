<!--
#==============================================================================#
#                                                                              #
#    Title      : ReadMe file                                                  #
#    Purpose    : Vital for any Git repo                                       #
#    Notes      : .                                                            #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : .                                                            #
#    Sources    : .                                                            #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#
-->

# Exploring Undernourishment
Exploring data from the United Nations to better understand undernourishment.

## Overview

Shiny application provided in the folder `ExploringUndernourishment\`.

## Data Sources

Data provided by [Food and Agriculture Organization of the United Nations](http://www.fao.org/home/en/) (FAO).

Raw data obtained from:

>* [FAO: Suite of Food Security Indicators](http://www.fao.org/faostat/en/#data/FS)

Additional information about the data sources can be found at:

>* [FAO: Sustainable Development Goals: Indicator 2.1.1 - Prevalence of Undernourishment](http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/)
>* [FAO: Food Security Indicators](http://www.fao.org/economic/ess/ess-fs/ess-fadata/en/#.XrXa5Wgzack)
>* [Our World in Data: Hunger and Undernourishment](https://ourworldindata.org/hunger-and-undernourishment)

---

## Data Dictionary

Field|Description
:----|:----------
`avg_dietary_adequacy`|Average dietary energy supply adequacy (percent) (3-year average)
`pct_access_to_drinking_water`|Percentage of population using at least basic drinking water services (percent)

|Field|Description|
|:----|:----------|
|`country`|The country being reported against|
|`year`|The reporting year|
|`avg_dietary_adequacy`|Average dietary energy supply adequacy (percent) (3-year average)|
|`avg_value_of_food_production`|Average value of food production (constant 2004-2006 I\$/cap) (3-year average)|
|`caloric_energy_from_cereals_roots_tubers`|Share of dietary energy supply derived from cereals, roots and tubers (kcal/cap/day) (3-year average)|
|`avg_protein_supply`|Average protein supply (g/cap/day) (3-year average)|
|`avg_supply_of_protein_of_animal_origin`|Average supply of protein of animal origin (g/cap/day) (3-year average)|
|`gross_domestic_product_per_capita_ppp`|Gross domestic product per capita, PPP, dissemination (constant 2011 international \$)|
|`prv_prevalence_of_undernourishment`|Prevalence of undernourishment (percent) (3-year average)|
|`num_people_undernourished`|Number of people undernourished (million) (3-year average)|
|`prv_severe_food_insecurity`|Prevalence of severe food insecurity in the total population (percent) (3-year average)|
|`prv_moderate_food_insecurity`|Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)|
|`num_severe_food_insecurity`|Number of severely food insecure people (million) (3-year average)|
|`num_moderate_food_insecurity`|Number of moderately or severely food insecure people (million) (3-year average)|
|`cereal_import_dependency_ratio`|Cereal import dependency ratio (percent) (3-year average)|
|`pct_arable_land`|Percent of arable land equipped for irrigation (percent) (3-year average)|
|`val_food_imports_as_share_of_merch_exports`|Value of food imports in total merchandise exports (percent) (3-year average)|
|`val_political_stability`|Political stability and absence of violence/terrorism (index)|
|`val_food_production_variability`|Per capita food production variability (constant 2004-2006 thousand int$ per capita)|
|`val_food_supply_variability`|Per capita food supply variability (kcal/cap/day)|
|`pct_access_to_drinking_water`|Percentage of population using at least basic drinking water services (percent)|
|`pct_access_to_sanitation_services`|Percentage of population using at least basic sanitation services (percent)|
|`pct_children_affected_by_wasting`|Percentage of children under 5 years affected by wasting (percent)|
|`pct_children_who_are_stunted`|Percentage of children under 5 years of age who are stunted (percent)|
|`pct_children_who_are_overweight`|Percentage of children under 5 years of age who are overweight (percent)|
|`prv_adults_who_are_overweight`|Prevalence of obesity in the adult population (18 years and older)|
|`prv_women_with_anemia`|Prevalence of anemia among women of reproductive age (15-49 years)|
|`prv_women_who_can_breastfeed`|Prevalence of exclusive breastfeeding among infants 0-5 months of age|
|`val_rail_line_density`|Rail lines density (total route in km per 100 square km of land area)|
|`pct_access_to_managed_drinking_water`|Percentage of population using safely managed drinking water services (Percent)|
|`pct_access_to_managed_sanitation_services`|Percentage of population using safely managed sanitation services (Percent)|
|`prv_low_birthrate`|Prevalence of low birthweight (percent)|
