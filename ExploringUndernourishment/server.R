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
    #                                                                              #
    #    Information                                                            ####
    #                                                                              #
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
    #                                                                              #
    #    Statistics > Total                                                     ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # Histogram Plot ----
    output$plt_stat_PrevUndrOverall <- renderPlot(
        expr={
            # Optimise for to save future load time.
            if (exists("plt_stat_PrevUndrOverall")) {
                plt_stat_PrevUndrOverall
            } else {
                # Make
                plt_stat_PrevUndrOverall <<- FaoStat_wide %>% 
                    ggplot() +
                    geom_histogram(aes(prevalence_of_undernourishment),
                                   bins=30, 
                                   fill="cornflowerblue", 
                                   colour="black",
                                   alpha=0.8,
                                   size=0.3
                    ) +
                    labs(title="Prevalence of Undernourishment",
                         subtitle="Histogram Plot",
                         x="Prevalence of Undernourishment",
                         y="Count",
                         caption="A neat, right-tailed histogram, with values between 0 and 0.7."
                    )
                
                # Return
                plt_stat_PrevUndrOverall
            }
        }
    )
    
    # MissingNess Plot ----
    output$plt_stat_MissingData <- renderPlot(
        expr={
            # Optimise for to save future load time.
            if (exists("plt_stat_MissingData")) {
                plt_stat_MissingData
            } else {
                # Make
                plt_stat_MissingData <<- FaoStat_wide %>% 
                    miss_var_summary() %>% 
                    left_join(x=., y=FaoStat_VariableMapping %>% select(variable, category), by=c("variable"="variable")) %>% 
                    ggplot(aes(x=stats::reorder(variable, pct_miss))) + 
                    geom_bar(aes(y=pct_miss, colour=category, fill=category),
                             stat="identity",
                             position="dodge",
                             width=0.1
                    ) + 
                    geom_point(aes(y = pct_miss, colour=category)
                               ,size=3
                    ) + 
                    coord_flip() + 
                    scale_color_brewer(type="qual", 
                                       palette="Dark2", 
                                       aesthetics=c("colour", "fill")
                    ) +
                    labs(title="Percentage of Missing Values",
                         subtitle="Ordered by percentage missing",
                         y="Percentage Missing",
                         x="Variables",
                         color="Category",
                         fill="Category"
                    )
                
                # Return
                plt_stat_MissingData
            }
        }
    )
    
    # Correlation Plot ----
    output$plt_corr_AllVariables <- renderPlot(
        expr={
            FaoStat_wide %>% 
                select(-country, -year) %>% 
                extract(ncol(.):1) %>% 
                cor(use="pairwise.complete.obs") %>% 
                corrplot(method="pie"
                         ,type="lower"
                         ,diag=FALSE
                         ,tl.col="black"
                )
        }
    )
    
    
    # GGRidges Plot ----
    output$plt_ridg_UndernourishmentByYear <- renderPlot(
        expr={
            FaoStat_wide %>% {
                ggplot(., aes(prevalence_of_undernourishment, reorder(year,desc(year)), fill=year)) +
                    geom_density_ridges() + 
                    scale_fill_manual(values=colorRampPalette(brewer.pal(9, "YlGn"))(nrow(unique(.["year"])))) +
                    labs(
                        title="Undernourishment Per Year",
                        subtitle="Ridge Plot",
                        y="Year",
                        x="Prevalence of Undernourishment"
                    )
            }
        }
    )
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Statistics > Features                                                  ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # Heading3 ----
    output$plt_hist_FeatureDistributions <- renderPlot(
        expr={
            # Optimise for to save future load time.
            if (exists("plt_hist_FeatureDistributions")) {
                plt_hist_FeatureDistributions
            } else {
                # Make
                plt_hist_FeatureDistributions <<- FaoStat_wide %>% 
                    plt_grob_MultipleHistograms(c("country", "year"))
                
                # Return
                plt_hist_FeatureDistributions
            }
        }
    )
    
    # comment ----
    output$tbl_stat_DataFrameStats <- DT::renderDataTable(
        expr={
            FaoStat_wide %>%
                get_DataFrameStatistics(signif=2) %>%
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
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Undernourishment                                                       ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
}
