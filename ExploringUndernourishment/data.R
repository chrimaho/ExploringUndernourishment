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
            
            # Target
            item=="Prevalence of undernourishment (percent) (3-year average)"                                               ~ "prevalence_of_undernourishment"
            
            # Agriculture
            ,item=="Percent of arable land equipped for irrigation (percent) (3-year average)"                              ~ "percentage_of_arable_land"
            
            # Economics
            ,item=="Cereal import dependency ratio (percent) (3-year average)"                                              ~ "cereal_import_dependency_ratio"
            ,item=="Average value of food production (constant 2004-2006 I$/cap) (3-year average)"                          ~ "avg_value_of_food_production"
            ,item=="Value of food imports in total merchandise exports (percent) (3-year average)"                          ~ "food_imports_as_share_of_merch_exports"
            ,item=="Gross domestic product per capita, PPP, dissemination (constant 2011 international $)"                  ~ "gross_domestic_product_per_capita_ppp"
            
            # Food Security
            ,item=="Average supply of protein of animal origin (g/cap/day) (3-year average)"                                ~ "avg_supply_of_protein_of_animal_origin"
            ,item=="Share of dietary energy supply derived from cereals, roots and tubers (kcal/cap/day) (3-year average)"  ~ "caloric_energy_from_cereals_roots_tubers"
            ,item=="Per capita food production variability (constant 2004-2006 thousand int$ per capita)"                   ~ "food_production_variability"
            ,item=="Per capita food supply variability (kcal/cap/day)"                                                      ~ "food_supply_variability"
            ,item=="Average dietary energy supply adequacy (percent) (3-year average)"                                      ~ "avg_dietary_adequacy"
            ,item=="Average protein supply (g/cap/day) (3-year average)"                                                    ~ "avg_protein_supply"
            ,item=="Prevalence of moderate or severe food insecurity in the total population (percent) (3-year average)"    ~ "prevalence_moderate_food_insecurity"
            ,item=="Number of moderately or severely food insecure people (million) (3-year average)"                       ~ "number_moderate_food_insecurity"
            ,item=="Prevalence of severe food insecurity in the total population (percent) (3-year average)"                ~ "prevalence_severe_food_insecurity"
            ,item=="Number of severely food insecure people (million) (3-year average)"                                     ~ "number_severe_food_insecurity"
            
            # Health
            ,item=="Percentage of population using at least basic drinking water services (percent)"                        ~ "access_to_basic_drinking_water"
            ,item=="Percentage of population using at least basic sanitation services (percent)"                            ~ "access_to_basic_sanitation_services"
            ,item=="Percentage of population using safely managed drinking water services (Percent)"                        ~ "access_to_improved_drinking_water"
            ,item=="Percentage of population using safely managed sanitation services (Percent)"                            ~ "access_to_improved_sanitation_services"
            ,item=="Prevalence of anemia among women of reproductive age (15-49 years)"                                     ~ "prevalence_of_anemia"
            ,item=="Prevalence of obesity in the adult population (18 years and older)"                                     ~ "prevalence_of_obesity"
            ,item=="Number of people undernourished (million) (3-year average)"                                             ~ "number_people_undernourished"
            ,item=="Prevalence of low birthweight (percent)"                                                                ~ "prevalence_of_low_birthrate"
            ,item=="Percentage of children under 5 years affected by wasting (percent)"                                     ~ "children_affected_by_wasting"
            ,item=="Percentage of children under 5 years of age who are stunted (percent)"                                  ~ "children_who_are_stunted"
            ,item=="Percentage of children under 5 years of age who are overweight (percent)"                               ~ "children_who_are_overweight"
            ,item=="Prevalence of exclusive breastfeeding among infants 0-5 months of age"                                  ~ "prevalence_of_breastfeeding_women"
            
            # Infrastructure
            ,item=="Rail lines density (total route in km per 100 square km of land area)"                                  ~ "rail_line_density"
            
            # Politics
            ,item=="Political stability and absence of violence/terrorism (index)"                                          ~ "political_stability"
            
        )) %>% 
        
        #fix units
        mutate(value=ifelse(unit=="%", value/100, value)
               ,value=ifelse(unit=="millions", value*1e+06, value)
               ,year=str_Right(year,4)
               ,country=as.factor(country)
               ,year=as.factor(year)
               ) %>% 
        
        #extract mapping
        (function(x){
            FaoStat_VariableMapping <<- x %>% 
                select(variable,item) %>% 
                rename("description"="item") %>% 
                distinct() %>% 
                rbind(c("country", "The country being recorded")) %>% 
                rbind(c("year", "The year of record"))
            return(x)
        }) %>% 
        
        #remove unnecessary columns
        select(country,year,variable,value,-contains(c("item","unit"))) %>% 
        
        #order data
        arrange(country,year,variable)
    
}


# FaoStat Wide ----

if (!exists("FaoStat_wide")) {
    
    # Make FaoStat_wide ----
    FaoStat_wide <- FaoStat_long %>% 
        
        # Make Wider
        pivot_wider(names_from="variable", values_from="value") %>% 
        
        # Add Completeness
        (function(x){
            x %>% 
                select(country, prevalence_of_undernourishment) %>% 
                group_by(country) %>% 
                summarise(num_complete=sum(!is.na(prevalence_of_undernourishment)),
                          avg_undernourishment=mean(prevalence_of_undernourishment, na.rm=TRUE)
                          ) %>% 
                ungroup() %>% 
                mutate(
                    pct_complete=num_complete/max(num_complete)*100,
                    cat_complete=case_when(
                        pct_complete==100 ~ "full",
                        pct_complete<=20  ~ "empty",
                        TRUE              ~ "partial"
                    )
                ) %>% 
                left_join(
                    x=x,
                    y=.,
                    by=c("country"="country")
                ) %>% 
                return()
        }) %>% 
        
        # Reorder Columns
        select(country, year, prevalence_of_undernourishment, everything()) %>% 
        
        # Reorder Rows
        arrange(country,year)

}


# FaoStat VariableMapping ----
FaoStat_VariableMapping <- FaoStat_wide %>% 
    select(-contains("_complete"), -contains("avg_undernourishment")) %>% 
    names %>% 
    data.frame("variable"=., stringsAsFactors=FALSE) %>% 
    left_join(y=FaoStat_VariableMapping, by=c("variable"="variable")) %>% 
    
    # Add Category mapping
    mutate(category=case_when(
        
        # Identifier
        variable %in% c("country", "year") ~ "identifier"
        
        # Target
        ,variable %in% c("prevalence_of_undernourishment") ~ "target"
        
        # Agriculture
        ,variable %in% c("percentage_of_arable_land") ~ "agriculture"
        
        # Economics
        ,variable %in% c("cereal_import_dependency_ratio"
                        ,"avg_value_of_food_production"
                        ,"food_imports_as_share_of_merch_exports"
                        ,"gross_domestic_product_per_capita_ppp"
                        ) ~ "economics"
        
        # Food Security
        ,variable %in% c("avg_supply_of_protein_of_animal_origin"
                        ,"caloric_energy_from_cereals_roots_tubers"
                        ,"food_production_variability"
                        ,"food_supply_variability"
                        ,"avg_dietary_adequacy"
                        ,"avg_protein_supply"
                        ,"prevalence_moderate_food_insecurity"
                        ,"number_moderate_food_insecurity"
                        ,"prevalence_severe_food_insecurity"
                        ,"number_severe_food_insecurity"
                        ) ~ "food security"
        
        # Health
        ,variable %in% c("access_to_basic_drinking_water"
                        ,"access_to_basic_sanitation_services"
                        ,"access_to_improved_drinking_water"
                        ,"access_to_improved_sanitation_services"
                        ,"prevalence_of_anemia"
                        ,"prevalence_of_obesity"
                        ,"number_people_undernourished"
                        ,"prevalence_of_low_birthrate"
                        ,"children_affected_by_wasting"
                        ,"children_who_are_stunted"
                        ,"children_who_are_overweight"
                        ,"prevalence_of_breastfeeding_women" 
                        ) ~ "health"
        
        # Infrastructure
        ,variable %in% c("rail_line_density") ~ "infrastructure"
        
        # Politics
        ,variable %in% c("political_stability") ~ "politics"
        
    )) %>% 
    mutate(type=case_when(
        variable %in% c("country", "year") ~ "identifier"
        ,variable %in% "prevalence_of_undernourishment" ~ "target"
        ,TRUE ~ "other"
    )) %>% 
    arrange(factor(category, levels=c("identifier", "target", "agriculture", "economics", "food security", "health", "infrastructure", "politics")))


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
