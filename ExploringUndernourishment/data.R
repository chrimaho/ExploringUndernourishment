#==============================================================================#
#                                                                              #
#    Title      : Get Data for Shiny                                           #
#    Purpose    : Import and Clean all the data.                               #
#    Notes      : Ideally, it would be better to download directly from the    #
#                 web, however due to the method of extraction from the FAO    #
#                 website, this is not possible. Therefore, importing from     #
#                 csv is necessary.                                            #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : .                                                            #
#    Sources    : http://www.fao.org/faostat/en/#data/FS                       #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#


#------------------------------------------------------------------------------#
# Import Data                                                               ####
#------------------------------------------------------------------------------#

raw_DataPath <- find_rstudio_root_file("/ExploringUndernourishment/data/raw")
for (file in list.files(raw_DataPath, pattern="*.csv")) {
    filename <- str_remove(file, ".csv")
    assign(paste0("raw_",filename)
          ,read_csv(paste0(raw_DataPath, "/", file), col_types=cols()) %>% data.frame
          )
}


#------------------------------------------------------------------------------#
# Clean All Data                                                            ####
#------------------------------------------------------------------------------#

# All ----
for (data in ls(pattern="raw_*")) {
    if (!is.data.frame(get(data))) next
    assign(str_remove(data, "raw_")
          ,data %>% 
              get %>% 
              rename_all(tolower) %>% 
              rename_all(str_replace_all, " ", "_")
          )
}


#------------------------------------------------------------------------------#
# Clean Specific Data                                                       ####
#------------------------------------------------------------------------------#

# FaoStat Long ----
if (!exists("FaoStat_long")) {
    
    # Make FaoStat long
    FaoStat_long <- FaoStat %>% 
        
        #remove unnecessary columns
        select(-contains(c("domain","element","code","flag","note"))) %>% 
        
        #rename country column
        rename("country"="area") %>% 
        
        #map variable names 
        mutate(variable = case_when(
            item=="Average dietary energy supply adequacy (percent) (3-year average)" ~ "avg_dietary_adequacy"
            ,item=="Average value of food production (constant 2004-2006 I$/cap) (3-year average)" ~ "avg_value_of_food_production"
            ,item=="Share of dietary energy supply derived from cereals, roots and tubers (kcal/cap/day) (3-year average)" ~ "caloric_energy_from_cereals_roots_tubers"
            ,item=="Average protein supply (g/cap/day) (3-year average)" ~ "avg_protein_supply"
            ,item=="Average supply of protein of animal origin (g/cap/day) (3-year average)" ~ "avg_supply_of_protein_of_animal_origin"
            ,item=="Gross domestic product per capita, PPP, dissemination (constant 2011 international $)" ~ "gross_domestic_product_per_capita_ppp"
            ,item=="Prevalence of undernourishment (percent) (3-year average)" ~ "prv_prevalence_of_undernourishment"
            ,item=="Number of people undernourished (million) (3-year average)" ~ "num_people_undernourished"
            ,item=="Prevalence of severe food insecurity in the total population (percent) (3-year average)" ~ "prv_severe_food_insecurity"
            ,item=="Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)" ~ "prv_moderate_food_insecurity"
            ,item=="Number of severely food insecure people (million) (3-year average)" ~ "num_severe_food_insecurity"
            ,item=="Number of moderately or severely food insecure people (million) (3-year average)" ~ "num_moderate_food_insecurity"
            ,item=="Cereal import dependency ratio (percent) (3-year average)" ~ "cereal_import_dependency_ratio"
            ,item=="Percent of arable land equipped for irrigation (percent) (3-year average)" ~ "pct_arable_land"
            ,item=="Value of food imports in total merchandise exports (percent) (3-year average)" ~ "val_food_imports_as_share_of_merch_exports"
            ,item=="Political stability and absence of violence/terrorism (index)" ~ "val_political_stability"
            ,item=="Per capita food production variability (constant 2004-2006 thousand int$ per capita)" ~ "val_food_production_variability"
            ,item=="Per capita food supply variability (kcal/cap/day)" ~ "val_food_supply_variability"
            ,item=="Percentage of population using at least basic drinking water services (percent)" ~ "pct_access_to_drinking_water"
            ,item=="Percentage of population using at least basic sanitation services (percent)" ~ "pct_access_to_sanitation_services"
            ,item=="Percentage of children under 5 years affected by wasting (percent)" ~ "pct_children_affected_by_wasting"
            ,item=="Percentage of children under 5 years of age who are stunted (percent)" ~ "pct_children_who_are_stunted"
            ,item=="Percentage of children under 5 years of age who are overweight (percent)" ~ "pct_children_who_are_overweight"
            ,item=="Prevalence of obesity in the adult population (18 years and older)" ~ "prv_adults_who_are_overweight"
            ,item=="Prevalence of anemia among women of reproductive age (15-49 years)" ~ "prv_women_with_anemia"
            ,item=="Prevalence of exclusive breastfeeding among infants 0-5 months of age" ~ "prv_women_who_can_breastfeed"
            ,item=="Rail lines density (total route in km per 100 square km of land area)" ~ "val_rail_line_density"
            ,item=="Percentage of population using safely managed drinking water services (Percent)" ~ "pct_access_to_managed_drinking_water"
            ,item=="Percentage of population using safely managed sanitation services (Percent)" ~ "pct_access_to_managed_sanitation_services"
            ,item=="Prevalence of low birthweight (percent)" ~ "prv_low_birthrate"
        )) %>% 
        
        #fix units
        mutate(value=ifelse(unit=="%", value/100, value)
               ,value=ifelse(unit=="millions", value*1e+06, value)
               ,year=str_Right(year,4)
               ) %>% 
        
        #extract mapping
        (function(x){
            FaoStat_VariableMapping <<- x %>% select(variable,item) %>% distinct()
            return(x)
        }) %>% 
        
        #remove unnecessary columns
        select(country,year,variable,value,-contains(c("item","unit"))) %>% 
        
        #order data
        arrange(country,year,variable)
    
}


# FaoStat Wide ----

if (!exists("FaoStat_wide")) {
    
    # Make FaoStat wide ----
    FaoStat_wide <- FaoStat_long %>% 
        
        #make wider
        pivot_wider(names_from="variable", values_from="value") %>% 
        
        #order data
        arrange(country,year)

    }


#------------------------------------------------------------------------------#
# Save Data                                                                 ####
#------------------------------------------------------------------------------#

pro_DataPath <- find_rstudio_root_file("/ExploringUndernourishment/data/processed")

# Long ----
if (!file.exists(paste0(pro_DataPath, "/FaoStat_long.rds"))) {
    FaoStat_long %>% saveRDS(paste0(pro_DataPath, "/FaoStat_long.rds"))
}

# Wide ----
if (!file.exists(paste0(pro_DataPath, "/FaoStat_wide.rds"))) {
    FaoStat_wide %>% saveRDS(paste0(pro_DataPath, "/FaoStat_wide.rds"))
}
