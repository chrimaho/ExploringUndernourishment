#==============================================================================#
#                                                                              #
#    Title      : Shiny Server                                                 #
#    Purpose    : Define the function to be used as the server for Shiny App   #
#    Notes      : .                                                            #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : .                                                            #
#    Sources    : .                                                            #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#


#------------------------------------------------------------------------------#
# Define the Server                                                         ####
#------------------------------------------------------------------------------#

server <- function(input, output, session) {
    
    
    #------------------------------------------------------------------------------#
    # Info Page                                                                 ####
    #------------------------------------------------------------------------------#
    
    output$tbl_info_DataDictionary <- DT::renderDataTable(
        expr={
            FaoStat_VariableMapping
            },
        options=list(
            pageLength=100,
            dom="ft",
            autoWidth=TRUE,
            scrollX=TRUE,
            columnDefs=list(list(ClassName="dt-left", targets="_all"))
        )
    )
    
    
    #------------------------------------------------------------------------------#
    # Stat Page                                                                 ####
    #------------------------------------------------------------------------------#
    
    # comment ----
    output$plt_stat_PrevUndrOverall <- renderPlot(
        expr={
            FaoStat_wide %>% 
                ggplot() +
                geom_histogram(aes(prevalence_of_undernourishment), bins=30, fill="cornflowerblue", colour="black") +
                labs(title="Prevalence of Undernourishment",
                     subtitle="Histogram Plot",
                     x="Prevalence of Undernourishment",
                     y="Count",
                     caption="A neat, right-tailed histogram, with values between 0 and 0.7."
                )
        }
    )
    
    # comment ----
    output$plt_stat_MissingData <- renderPlot(
        expr={
            FaoStat_wide %>% 
                miss_var_summary() %>% 
                left_join(x=., y=FaoStat_VariableMapping %>% select(variable, category), by=c("variable"="variable")) %>% 
                ggplot(aes(x = stats::reorder(variable, pct_miss))) + 
                geom_bar(aes(y = pct_miss, colour=category, fill=category)
                         ,stat = "identity"
                         ,position = "dodge"
                         ,width = 0.1
                ) + 
                geom_point(aes(y = pct_miss, colour=category)
                           ,size=3
                ) + 
                coord_flip() + 
                scale_color_brewer(type="qual", palette="Dark2", aesthetics=c("colour", "fill")) +
                labs(title="Percentage of Missing Values"
                     ,subtitle="Ordered by percentage missing"
                     ,y="Percentage Missing"
                     ,x="Variables"
                     ,color="Category"
                     ,fill="Category"
                )
        }
    )
    
    # Heading3 ----
    output$plt_corr_AllVariables <- renderPlot(
        expr={
            FaoStat_wide %>% 
                select(-country, -year) %>% 
                cor(use="pairwise.complete.obs") %>% 
                ggcorrplot::ggcorrplot(method="square"
                                       ,type="upper"
                                       ,show.diag=FALSE
                                       ,colors = c("blue", "grey", "red")
                                       ,digits=1
                                       ,tl.srt=90
                                       ,lab=TRUE
                                       ,lab_col="black"
                                       ,lab_size=3
                                       ,insig="pch"
                                       ,pch="?"
                                       ,pch.cex=3
                )
        }
    )
    
    # Heading3 ----
    output$plt_hist_FeatureDistributions <- renderPlot(
        expr={
            FaoStat_wide %>% 
                plt_grob_MultipleHistograms(c("country", "year"))
        }
    )
    
    # comment ----
    output$tbl_stat_DataFrameStats <- DT::renderDataTable(
        expr={
            FaoStat_wide %>%
                get_DataFrameStatistics(signif=2) %>%
                # select(-c(length, class, type, mode, sum), -contains("null"))
                select(-c(sum), -contains("null"))
        },
        options=list(
            pageLength=40,
            dom="ft",
            scrollX=TRUE,
            autoWidth=TRUE,
            columnDefs=list(list(ClassName="dt-left", width="auto", targets="_all"))
        )
    )
    
}
