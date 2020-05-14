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
            bAutoWidth=TRUE,
            columnDefs=list(list(ClassName="dt-left", targets="_all"))
        )
    )
    
    
    #------------------------------------------------------------------------------#
    # Stat Page                                                                 ####
    #------------------------------------------------------------------------------#
    
    # comment ----
    output$plt_stat_PrevUndrOverall <- renderPlotly(
        expr={
            ggplotly(
                FaoStat_wide %>% 
                    ggplot() +
                    geom_histogram(aes(prevalence_of_undernourishment), bins=30, fill="cornflowerblue", colour="black") +
                    labs(title="Prevalence of Undernourishment",
                         subtitle="Histogram Plot",
                         x="Prevalence of Undernourishment",
                         y="Count",
                         caption="A neat, right-tailed histogram, with values between 0 and 0.7."
                    )
            )
        }
    )
    
    # comment ----
    output$plt_stat_MissingData <- renderPlotly(
        expr={
            ggplotly(
                FaoStat_wide %>% 
                    gg_miss_var(show_pct=TRUE) +
                    theme_bw() +
                    theme_update(plot.title = element_text(hjust=0.5)
                                 ,plot.subtitle = element_text(hjust=0.5)
                    )+
                    scale_y_continuous(limits=c(0,100)) +
                    labs(title="Percentage of Missing Values"
                         ,subtitle="Ordered by percentage missing"
                         ,y="Percentage Missing"
                    )
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
