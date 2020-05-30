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


server <- function(input, output, session) {
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Data Dictionary                                                        ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # . . Data Dictionary Table ----
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
    #    Undernourishment                                                       ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    #------------------------------------------------------------------------------#
    # . Dynamic Part                                                            ####
    #------------------------------------------------------------------------------#
    
    # . . Set Data ----
    dat_undr_dynm_InputData <- reactive({
        FaoStat_wide %>% 
            mutate(year=as.numeric(as.character(year))) %>% 
            filter(cat_complete!="empty") %>% 
            filter(between(year, input$undr_dynm_slid_SelectedYears[1], input$undr_dynm_slid_SelectedYears[2])) %>%
            {if (!"All" %in% input$undr_dynm_inbx_SelectedCountries) {filter(., country %in% input$undr_dynm_inbx_SelectedCountries)}} %>%
            return()
    })
    
    # . . Left Side ----
    output$undr_dynm_plot_ImprovementPerYear <- renderPlot(
        expr={
            dat_undr_dynm_InputData() %>% 
                ggplot() +
                geom_line(aes(year, prevalence_of_undernourishment, colour=country)) +
                scale_x_continuous(breaks=seq(input$undr_dynm_slid_SelectedYears[1], input$undr_dynm_slid_SelectedYears[2], 1)) +
                labs(
                    title="Country Improvement per Year",
                    subtitle="'Prevalence of Undernourishment' per 'Country' per 'Year'",
                    x="Year",
                    y="Prevalence of Undernourishment"
                )
        }
    )
    
    # . . Right Side ----
    output$undr_dynm_plot_DistributionPerCountry <- renderPlot(
        expr={
            dat_undr_dynm_InputData() %>% 
                {ggplot(., aes(prevalence_of_undernourishment, reorder(country, desc(avg_undernourishment)), fill=reorder(country, desc(avg_undernourishment)))) +
                        geom_density_ridges(alpha=0.8) + 
                        scale_fill_manual(values=colorRampPalette(brewer.pal(9, "Greens"))(nrow(unique(.["country"])))) +
                        theme(legend.position="none") +
                        scale_x_continuous(sec.axis=dup_axis()) +
                        labs(
                            title="Undernourishment Per Country",
                            subtitle="Average 'Prevalence of Undernourishment' per 'Country'",
                            y="Country",
                            x="Prevalence of Undernourishment"
                        )
                }
        }
    )
    
    
    #------------------------------------------------------------------------------#
    # . Static Part                                                             ####
    #------------------------------------------------------------------------------#
    
    # . . Left Side ----
    output$plt_undr_stat_Completeness <- renderPlot(
        expr={
            
            # Optimise to save future load time.
            if (!exists("plt_undr_stat_Completeness")) {
                
                # Make
                plt_undr_stat_Completeness <<- FaoStat_wide %>% 
                    select(country, pct_complete, cat_complete) %>%
                    distinct() %>% 
                    ggplot(aes(reorder(country, pct_complete), pct_complete, colour=cat_complete, fill=cat_complete)) +
                    geom_col(width=0.2, alpha=0.3, size=0) +
                    geom_point(size=3) +
                    coord_flip() +
                    scale_y_continuous(sec.axis=dup_axis()) +
                    theme(legend.position="top") +
                    labs(
                        title="Completeness of Records",
                        subtitle="'Prevalence of Undernourishment' per 'Country'",
                        y="Percentage of non-NA records",
                        x="Country",
                        fill="Completeness",
                        colour="Completeness"
                    )
                
            }
            
            # Return
            plt_undr_stat_Completeness %>% return()
            
        }
    )
    
    # . . Right Side ----
    output$plt_undr_stat_Ridges <- renderPlot(
        expr={
            
            # Optimise to save future load time.
            if (!exists("plt_undr_stat_Ridges")) {
                
                # Make
                plt_undr_stat_Ridges <<- FaoStat_wide %>% 
                    filter(cat_complete!="empty") %>% 
                    {ggplot(., aes(prevalence_of_undernourishment, reorder(country, desc(avg_undernourishment)), fill=reorder(country, desc(avg_undernourishment)))) +
                            geom_density_ridges(alpha=0.8) + 
                            scale_fill_manual(values=colorRampPalette(brewer.pal(9, "Greens"))(nrow(unique(.["country"])))) +
                            theme(legend.position="none") +
                            scale_x_continuous(sec.axis=dup_axis()) +
                            labs(
                                title="Undernourishment Per Country",
                                subtitle="Average 'Prevalence of Undernourishment' per 'Country'",
                                y="Country",
                                x="Prevalence of Undernourishment"
                            )
                    }
                
            }
            
            # Return
            plt_undr_stat_Ridges %>% return()
        }
    )
    
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Feature Interactions                                                   ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # . . Set Data ----
    dat_inta_dynm_InputData <- reactive({
        FaoStat_wide %>% 
            mutate(year=as.numeric(as.character(year))) %>% 
            filter(cat_complete!="empty") %>% 
            return()
    })
    
    # . . Plot Data ----
    output$plt_inta_MultiFeatures <- renderPlot(
        expr={
            
            # Make
            plt_inta_MultiFeatures <<- dat_inta_dynm_InputData() %>% 
                plt_comb_MultiFeaturesMultiPlots(
                    DataFrame=.,
                    Countries=input$inta_dynm_inbx_SelectedCountries,
                    x_Feature=input$inta_dynm_inbx_SelectedXFeature,
                    y_Feature=input$inta_dynm_inbx_SelectedYFeature
                )
            
            # Return
            plt_inta_MultiFeatures %>% return()
            
        }
    )
    
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Overall Statistics                                                     ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # . . Histogram Plot ----
    output$plt_stat_PrevUndrOverall <- renderPlot(
        expr={
            
            # Optimise to save future load time.
            if (!exists("plt_stat_PrevUndrOverall")) {
                
                # Make
                plt_stat_PrevUndrOverall <<- FaoStat_wide %>% 
                    ggplot() +
                    geom_histogram(
                        aes(prevalence_of_undernourishment),
                        bins=30, 
                        fill="cornflowerblue", 
                        colour="black",
                        alpha=0.8,
                        size=0.3
                    ) +
                    labs(
                        title="Prevalence of Undernourishment",
                        subtitle="Histogram Plot",
                        x="Prevalence of Undernourishment",
                        y="Count",
                        caption="A neat, right-tailed histogram, with values between 0 and 0.7."
                    )
            }
                
            # Return
            plt_stat_PrevUndrOverall
        }
    )
    
    # . . MissingNess Plot ----
    output$plt_stat_MissingData <- renderPlot(
        expr={
            
            # Optimise to save future load time.
            if (!exists("plt_stat_MissingData")) {
                
                # Make
                plt_stat_MissingData <<- FaoStat_wide %>% 
                    select(-contains("_complete"), -contains("avg_undernourishment")) %>% 
                    miss_var_summary() %>% 
                    left_join(x=., y=FaoStat_VariableMapping %>% select(variable, category), by=c("variable"="variable")) %>% 
                    ggplot(aes(x=stats::reorder(variable, pct_miss))) + 
                    geom_bar(
                        aes(y=pct_miss, colour=category, fill=category),
                        stat="identity",
                        position="dodge",
                        width=0.1
                    ) + 
                    geom_point(
                        aes(y = pct_miss, colour=category),
                        size=3
                    ) + 
                    coord_flip() + 
                    scale_color_brewer(
                        type="qual", 
                        palette="Dark2", 
                        aesthetics=c("colour", "fill")
                    ) +
                    labs(
                        title="Percentage of Missing Values",
                        subtitle="Ordered by percentage missing",
                        y="Percentage Missing",
                        x="Variables",
                        color="Category",
                        fill="Category"
                    )
            }
                
            # Return
            plt_stat_MissingData
            
        }
    )
    
    # . . Correlation Plot ----
    output$plt_corr_AllVariables <- renderPlot(
        expr={
            # Can't optimise this one because 'corrplot()' doesn't actually return a plot object...
            FaoStat_wide %>% 
                select(-country, -year, -contains("_complete"), -contains("avg_undernourishment")) %>% 
                extract(ncol(.):1) %>% 
                cor(use="pairwise.complete.obs") %>% 
                corrplot(method="pie"
                         ,type="lower"
                         ,diag=FALSE
                         ,tl.col="black"
                )
        }
    )
    
    
    # . . GGRidges Plot ----
    output$plt_ridg_UndernourishmentByYear <- renderPlot(
        expr={
            
            # Optimise to save future load time.
            if (!exists("plt_ridg_UndernourishmentByYear")) {
                
                # Make
                plt_ridg_UndernourishmentByYear <<- FaoStat_wide %>% {
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
            
            # Return
            plt_ridg_UndernourishmentByYear
            
        }
    )
    
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Feature Statistics                                                     ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # . . Distributions Per Feature ----
    output$plt_hist_FeatureDistributions <- renderPlot(
        expr={
            
            # Optimise to save future load time.
            if (!exists("plt_hist_FeatureDistributions")) {
                
                # Make
                plt_hist_FeatureDistributions <<- FaoStat_wide %>% 
                    select(-contains("_complete"), -contains("avg_undernourishment")) %>% 
                    plt_grob_MultipleHistograms(c("country", "year"))
                
            }
            
            # Return
            plt_hist_FeatureDistributions %>% return()
            
        }
    )
    
    # . . Statistics Per Feature ----
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
    
    
    
    
}
