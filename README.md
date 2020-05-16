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

## Shiny App

The primary means of exploring this data:

* Shiny App deployed to: [Exploring Undernourishment](https://chrimaho.shinyapps.io/ExploringUndernourishment/).
* Shiny App source code: [`ExploringUndernourishment\`](https://github.com/chrimaho/ExploringUndernourishment/tree/master/ExploringUndernourishment).

## Data Sources

Data provided by:

* [Food and Agriculture Organization of the United Nations](http://www.fao.org/home/en/) (FAO).

Raw data obtained from:

* [FAO: Suite of Food Security Indicators](http://www.fao.org/faostat/en/#data/FS)

Additional information about the data sources can be found at:

* [FAO: Sustainable Development Goals: Indicator 2.1.1 - Prevalence of Undernourishment](http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/)
* [FAO: Food Security Indicators](http://www.fao.org/economic/ess/ess-fs/ess-fadata/en/#.XrXa5Wgzack)
* [Our World in Data: Hunger and Undernourishment](https://ourworldindata.org/hunger-and-undernourishment)

---

## Data Dictionary

|variable                                   |description                                                                                           |type       |category       |
|:------------------------------------------|:-----------------------------------------------------------------------------------------------------|:----------|:--------------|
|`country`                                  |The country being recorded                                                                            |identifier |identifier     |
|`year`                                     |The year of record                                                                                    |identifier |identifier     |
|`prevalence_of_undernourishment`           |Prevalence of undernourishment (percent) (3-year average)                                             |target     |target         |
|`percentage_of_arable_land`                |Percent of arable land equipped for irrigation (percent) (3-year average)                             |other      |agriculture    |
|`avg_value_of_food_production`             |Average value of food production (constant 2004-2006 I$/cap) (3-year average)                         |other      |economics      |
|`cereal_import_dependency_ratio`           |Cereal import dependency ratio (percent) (3-year average)                                             |other      |economics      |
|`food_imports_as_share_of_merch_exports`   |Value of food imports in total merchandise exports (percent) (3-year average)                         |other      |economics      |
|`gross_domestic_product_per_capita_ppp`    |Gross domestic product per capita, PPP, dissemination (constant 2011 international $)                 |other      |economics      |
|`food_production_variability`              |Per capita food production variability (constant 2004-2006 thousand int$ per capita)                  |other      |food security  |
|`food_supply_variability`                  |Per capita food supply variability (kcal/cap/day)                                                     |other      |food security  |
|`avg_dietary_adequacy`                     |Average dietary energy supply adequacy (percent) (3-year average)                                     |other      |food security  |
|`avg_protein_supply`                       |Average protein supply (g/cap/day) (3-year average)                                                   |other      |food security  |
|`avg_supply_of_protein_of_animal_origin`   |Average supply of protein of animal origin (g/cap/day) (3-year average)                               |other      |food security  |
|`caloric_energy_from_cereals_roots_tubers` |Share of dietary energy supply derived from cereals, roots and tubers (kcal/cap/day) (3-year average) |other      |food security  |
|`number_moderate_food_insecurity`          |Number of moderately or severely food insecure people (million) (3-year average)                      |other      |food security  |
|`number_severe_food_insecurity`            |Number of severely food insecure people (million) (3-year average)                                    |other      |food security  |
|`prevalence_moderate_food_insecurity`      |Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)   |other      |food security  |
|`prevalence_severe_food_insecurity`        |Prevalence of severe food insecurity in the total population (percent) (3-year average)               |other      |food security  |
|`access_to_basic_drinking_water`           |Percentage of population using at least basic drinking water services (percent)                       |other      |health         |
|`access_to_basic_sanitation_services`      |Percentage of population using at least basic sanitation services (percent)                           |other      |health         |
|`prevalence_of_anemia`                     |Prevalence of anemia among women of reproductive age (15-49 years)                                    |other      |health         |
|`prevalence_of_obesity`                    |Prevalence of obesity in the adult population (18 years and older)                                    |other      |health         |
|`number_people_undernourished`             |Number of people undernourished (million) (3-year average)                                            |other      |health         |
|`children_affected_by_wasting`             |Percentage of children under 5 years affected by wasting (percent)                                    |other      |health         |
|`children_who_are_overweight`              |Percentage of children under 5 years of age who are overweight (percent)                              |other      |health         |
|`children_who_are_stunted`                 |Percentage of children under 5 years of age who are stunted (percent)                                 |other      |health         |
|`prevalence_of_breastfeeding_women`        |Prevalence of exclusive breastfeeding among infants 0-5 months of age                                 |other      |health         |
|`access_to_improved_drinking_water`        |Percentage of population using safely managed drinking water services (Percent)                       |other      |health         |
|`access_to_improved_sanitation_services`   |Percentage of population using safely managed sanitation services (Percent)                           |other      |health         |
|`prevalence_of_low_birthrate`              |Prevalence of low birthweight (percent)                                                               |other      |health         |
|`rail_line_density`                        |Rail lines density (total route in km per 100 square km of land area)                                 |other      |infrastructure |
|`political_stability`                      |Political stability and absence of violence/terrorism (index)                                         |other      |politics       |
