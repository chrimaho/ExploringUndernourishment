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
#    Edited     : 16/Oct/2020 - Add links for Article                          #
#                                                                              #
#==============================================================================#
-->

# Exploring Undernourishment

Exploring data from the United Nations to better understand undernourishment.

### Shiny App

The primary means of exploring this data:

* Shiny App deployed to: [Exploring Undernourishment](https://chrimaho.shinyapps.io/ExploringUndernourishment/).
* Shiny App source code: [`ExploringUndernourishment/`](https://github.com/chrimaho/ExploringUndernourishment/tree/master/ExploringUndernourishment).

### Exploring Undernourishment Article:

* Part 1 - [Introduction and Overview](https://medium.com/@chrimaho/exploring-undernourishment-part-1-introduction-and-overview-ff024fa7dd32?source=friends_link&sk=dc47b684aa91157137e18e2bc4e7d8f8)
* Part 2 - [Literature Review](https://medium.com/@chrimaho/exploring-undernourishment-part-2-literature-review-a1abfefb86b7?source=friends_link&sk=ee953d379bafa7e6f2bf552fe4d7481f)
* Part 3 - [Data Exploration](https://medium.com/@chrimaho/exploring-undernourishment-part-3-data-exploration-71f15e5778f3?source=friends_link&sk=5c5c2dc44248109e1c443cb731102773)
* Part 4 - [Research Area 1: General Trend](https://medium.com/@chrimaho/exploring-undernourishment-part-4-research-area-1-general-trend-cdb8ced5b0af?source=friends_link&sk=30922587311cccfd051e40e90e4bf3b8)
* Part 5 - [Research Area 2: Most Successful Countries](https://medium.com/@chrimaho/exploring-undernourishment-part-5-research-area-2-most-successful-countries-c0afd1504c71?source=friends_link&sk=8da658166ca6d80b3cd9613b26ab02f1)
* Part 6 - [Research Area 3: Surprising Trends](https://medium.com/@chrimaho/exploring-undernourishment-part-6-research-area-3-surprising-trends-962d653a08fc?source=friends_link&sk=d6ba983f77a0f08176d2d916033c4b52)
* Part 7 - [Research Area 4: Most Influential Indicator](https://medium.com/@chrimaho/exploring-undernourishment-part-7-research-area-4-most-influential-indicator-47ba76395f9b?source=friends_link&sk=5eab78561820fe6e0dd5b3ade028e1b5)
* Part 8 - [Recommendations and Conclusions](https://medium.com/@chrimaho/exploring-undernourishment-part-8-recommendations-and-conclusions-c5eb682bb3a5?source=friends_link&sk=d1b4e31a82e421f6e7924d46732a816f)

### Data Sources

Data provided by:

* [Food and Agriculture Organization of the United Nations](http://www.fao.org/home/en/) (FAO).

Raw data obtained from:

* [FAO: Suite of Food Security Indicators](http://www.fao.org/faostat/en/#data/FS)

Additional information about the data sources can be found at:

* [FAO: Sustainable Development Goals: Indicator 2.1.1 - Prevalence of Undernourishment](http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/)
* [FAO: Food Security Indicators](http://www.fao.org/economic/ess/ess-fs/ess-fadata/en/#.XrXa5Wgzack)
* [Our World in Data: Hunger and Undernourishment](https://ourworldindata.org/hunger-and-undernourishment)

---

### Data Dictionary

| variable                                  | description                                                                                          | type      | category      |
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
