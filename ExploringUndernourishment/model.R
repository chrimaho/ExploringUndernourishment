#==============================================================================#
#                                                                              #
#    Title      : Model                                                        #
#    Purpose    : To model the data                                            #
#    Notes      : Currently set up to use the Gradient Boosted Machine (GBM)   #
#    Author     : chrimaho                                                     #
#    Created    : 01/Jun/2020                                                  #
#    References : References                                                   #
#    Sources    : Sources                                                      #
#    Edited     : 01/Jun/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#



#------------------------------------------------------------------------------#
#                                                                              #
#    Set Up                                                                 ####
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# . Data                                                                    ####
#------------------------------------------------------------------------------#

# . . Features ----
mod_data_Features <- FaoStat_VariableMapping %>% 
    filter(type=="independant") %>% 
    select(variable) %>% 
    pull()

# . . Raw ----
mod_data_Raw <- FaoStat_wide %>% 
    select(prevalence_of_undernourishment, all_of(mod_data_Features)) %>% 
    na.omit()

# . . Predictors ----
mod_data_Predictors <- mod_data_Raw %>% 
    select(-prevalence_of_undernourishment)

# . . Response ----
mod_data_Response <- mod_data_Raw %>% 
    select(prevalence_of_undernourishment) %>% 
    pull()


#------------------------------------------------------------------------------#
# . Control                                                                 ####
#------------------------------------------------------------------------------#

# . . Set ----
mod_gbm_TrainControl <- trainControl(
    method="adaptive_cv",
    number=3,
    repeats=10,
    search="grid",
    verboseIter=FALSE,
    allowParallel=FALSE
)


#------------------------------------------------------------------------------#
# . Grid                                                                    ####
#------------------------------------------------------------------------------#

# . . Set ----
mod_gbm_TuneGrid <- expand.grid(
    interaction.depth=c(5,10,30,50,100),
    n.trees=c(1000,2000,5000,10000),
    shrinkage=c(0.1,0.01,0.001,0.0001),
    n.minobsinnode=c(1,2,5,10)
)



#------------------------------------------------------------------------------#
#                                                                              #
#    Train                                                                  ####
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# . Logic                                                                   ####
#------------------------------------------------------------------------------#

# The process will check:
# 1. If the file exists in local directory
#   1. If Yes, then load and end
#   2. If No, then proceed to next step
# 2. If the model exists in the environment
#   1. If Yes, then end
#   2. If No, then proceed to next step
# 3. Run the model
# The main reason for this three-step process is because the model takes a long time to run.
# Once deployed to the world, it's better for Shiny to check these locations first, and prevent running the model from scratch.


#------------------------------------------------------------------------------#
# . Run                                                                     ####
#------------------------------------------------------------------------------#

# . . Check local directory ----
if (file.exists("./model/mod_gbm_Model.rds")) {
    
    # . . Load ----
    mod_gbm_Model <- read_rds("./model/mod_gbm_Model.rds")
    
} else {
    
    # . . Check local environment ----
    if (exists("mod_gbm_Model")) {
        
        # . . Re-Load ----
        mod_gbm_Model <- mod_gbm_Model
        
    } else {
    
        # . . Train ----
        tic("GBM Training")
        mod_gbm_Model <- train(
            x=mod_data_Predictors,
            y=mod_data_Response,
            method="gbm",
            trControl=mod_gbm_TrainControl,
            verbose=FALSE,
            metric="rmse",
            tuneGrid=mod_gbm_TuneGrid
        )
        toc()
        
        # . . Save ----
        write_rds(
            x=mod_gbm_Model,
            path="./model/mod_gbm_Model.rds",
            compress="none"
        )
    
    }
    
}



#------------------------------------------------------------------------------#
#                                                                              #
#    Extract                                                                ####
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# . Variable Importance                                                     ####
#------------------------------------------------------------------------------#

# . . Get Variables ----
mod_gbm_VariableImportance <- varImp(mod_gbm_Model)

# . . Variable Importance ----
if (!exists("plt_infl_VariableImportance")) {
    
    if (file.exists("./figure/plt_infl_VariableImportance.rds")) {
        
        # Load
        plt_infl_VariableImportance <<- read_rds("./figure/plt_infl_VariableImportance.rds")
        
    } else {
        
        # Make
        plt_infl_VariableImportance <<- mod_gbm_VariableImportance %>% 
            extract2("importance") %>% 
            rownames_to_column("variable") %>% 
            select("variable","score"="Overall") %>% 
            ggplot(aes(reorder(variable,score), score, label=round(score,2))) +
            geom_col(width=0.15, fill="darkgrey") +
            geom_point(size=4, colour="blue") +
            geom_label(hjust=-0.2) +
            scale_y_continuous(breaks=seq(0,100,10)) +
            theme(panel.grid.minor.x=element_blank()) +
            coord_flip() +
            labs(
                title="Variable Importance Plot",
                subtitle="Based on the output of a GBM Model",
                y="Importance",
                x="Variable"
            )
        
        # Save
        write_rds(
            x=plt_infl_VariableImportance,
            path="./figure/plt_infl_VariableImportance.rds",
            compress="xz"
        )
        
    }
    
}

# . . Partial Dependency ----
if (!exists("plt_infl_PartialDependancy")) {
    
    if (file.exists("./figure/plt_infl_PartialDependancy.rds")) {
        
        # Load
        plt_infl_PartialDependancy <<- read_rds("./figure/plt_infl_PartialDependancy.rds")
        
    } else {
        
        # Make
        plt_infl_PartialDependancy <<- plt_PartialDependencyPlots(
            mod_gbm_Model,
            mod_data_Raw,
            mod_gbm_VariableImportance
        )
        
        # Save
        write_rds(
            x=plt_infl_PartialDependancy,
            path="./figure/plt_infl_PartialDependancy.rds",
            compress="xz"
        )
        
    }
    
}



#------------------------------------------------------------------------------#
#                                                                              #
#    Clean Up                                                               ####
#                                                                              #
#------------------------------------------------------------------------------#

rm(
    mod_gbm_Model,
    mod_data_Raw,
    mod_data_Predictors,
    mod_data_Response
)
