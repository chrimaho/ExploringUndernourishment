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
    #    Introduction                                                           ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    #------------------------------------------------------------------------------#
    # . Links                                                                   ####
    #------------------------------------------------------------------------------#
    
    # . . GeneralTrend page ----
    observeEvent(
        {
            input$butn_intr_rese_GeneralTrend_GoToPage
            input$link_intr_rese_GeneralTrend_GoToPage
        }, 
        {
            updateTabItems(
                session,
                "SidebarMenu",
                "general_trend"
            )
        }
    )
    
    # . . MostSuccessful page ----
    observeEvent(
        {
            input$butn_intr_rese_MostSuccessful_GoToPage
            input$link_intr_rese_MostSuccessful_GoToPage
        },
        {
            updateTabItems(
                session,
                "SidebarMenu",
                "most_successful"
            )
        }
    )
    
    # . . SurprisingTrends page ----
    observeEvent(
        {
            input$butn_intr_rese_SurprisingTrends_GoToPage
            input$link_intr_rese_SurprisingTrends_GoToPage
        },
        {
            updateTabItems(
                session,
                "SidebarMenu",
                "surprising_trends"
            )
        }
    )
    
    # . . MostInfluential page ----
    observeEvent(
        {
            input$butn_intr_rese_MostInfluential_GoToPage
            input$link_intr_rese_MostInfluential_GoToPage
        },
        {
            updateTabItems(
                session,
                "SidebarMenu",
                "most_influential"
            )
        }
    )
    
    # . . DataDictionary page ----
    observeEvent(input$butn_intr_data_DataDictionary_GoToPage, {
        updateTabItems(
            session,
            "SidebarMenu",
            "dictionary"
        )
    })
    
    # . . Undernourishment page ----
    observeEvent(input$butn_intr_data_Undernourishment_GoToPage, {
        updateTabItems(
            session,
            "SidebarMenu",
            "undernourishment"
        )
    })
    
    # . . FeatureInteractions page ----
    observeEvent(input$butn_intr_data_FeatureInteractions_GoToPage, {
        updateTabItems(
            session,
            "SidebarMenu",
            "interactions"
        )
    })
    
    # . . OverallStatistics page ----
    observeEvent(input$butn_intr_data_OverallStatistics_GoToPage, {
        updateTabItems(
            session,
            "SidebarMenu",
            "stats_total"
        )
    })
    
    # . . FeatureStatistics page ----
    observeEvent(input$butn_intr_data_FeatureStatistics_GoToPage, {
        updateTabItems(
            session,
            "SidebarMenu",
            "stats_features"
        )
    })
    
    
    
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
    output$undr_dynm_plot_ImprovementPerYear <- renderPlotly(
        expr={
            dat_undr_dynm_InputData() %>% 
                ggplot() +
                geom_line(aes(year, prevalence_of_undernourishment, colour=country)) +
                scale_x_continuous(breaks=seq(input$undr_dynm_slid_SelectedYears[1], input$undr_dynm_slid_SelectedYears[2], 1)) +
                theme(
                    axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
                    panel.grid.minor.x=element_blank()
                ) +
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
            
            # Check if exists in local environment
            if (!exists("plt_undr_stat_Completeness")) {
                
                # Check if exists in local directory
                if (file.exists("./figure/plt_undr_stat_Completeness.rds")) {
                    
                    # Load
                    plt_undr_stat_Completeness <<- read_rds("./figure/plt_undr_stat_Completeness.rds")
                    
                } else {
                    
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
                    
                    # Save
                    write_rds(
                        x=plt_undr_stat_Completeness,
                        path="./figure/plt_undr_stat_Completeness.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            # Return
            return(plt_undr_stat_Completeness)
            
        }
    )
    
    # . . Right Side ----
    output$plt_undr_stat_Ridges <- renderPlot(
        expr={
            
            # Check if exists in local environment
            if (!exists("plt_undr_stat_Ridges")) {
                
                # Check if exists in local directory
                if (file.exists("./figure/plt_undr_stat_Ridges.rds")) {
                    
                    # Load
                    plt_undr_stat_Ridges <<- read_rds("./figure/plt_undr_stat_Ridges.rds")
                    
                } else {
                    
                    # Make
                    plt_undr_stat_Ridges <<- FaoStat_wide %>% 
                        filter(cat_complete!="empty") %>% 
                        {
                            ggplot(data=., aes(
                                    prevalence_of_undernourishment, 
                                    reorder(country, desc(avg_undernourishment)), 
                                    fill=reorder(country, desc(avg_undernourishment))
                                )) +
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
                    
                    # Save
                    write_rds(
                        x=plt_undr_stat_Ridges,
                        path="./figure/plt_undr_stat_Ridges.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            # Return
            return(plt_undr_stat_Ridges)
        }
    )
    
    
    #------------------------------------------------------------------------------#
    # . Features by Target                                                      ####
    #------------------------------------------------------------------------------#
    
    output$plt_undr_FeaturesByTarget <- renderPlot(
        expr={
            
            # Check if exists in local environment
            if (!exists("plt_undr_FeaturesByTarget")) {
                
                # Check if exists in local directory
                if (file.exists("./figure/plt_undr_FeaturesByTarget.rds")) {
                    
                    # Load
                    plt_undr_FeaturesByTarget <<- read_rds("./figure/plt_undr_FeaturesByTarget.rds")
                    
                } else {
                    
                    # Make
                    plt_undr_FeaturesByTarget <<- plt_FeatureCorrelationsByTarget(
                        FaoStat_wide, 
                        "prevalence_of_undernourishment", 
                        c("avg_undernourishment","pct_complete")
                    )
                    
                    # Save
                    write_rds(
                        x=plt_undr_FeaturesByTarget,
                        path="./figure/plt_undr_FeaturesByTarget.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            return(plt_undr_FeaturesByTarget)
            
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
            return(plt_inta_MultiFeatures)
            
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
            
            # Check if exists in local environment
            if (!exists("plt_stat_PrevUndrOverall")) {
                
                # Check if exists in local directory
                if (file.exists("./figure/plt_stat_PrevUndrOverall.rds")) {
                    
                    # Load
                    plt_stat_PrevUndrOverall <<- read_rds("./figure/plt_stat_PrevUndrOverall.rds")
                    
                } else {
                    
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
                    
                    # Save
                    write_rds(
                        x=plt_stat_PrevUndrOverall,
                        path="./figure/plt_stat_PrevUndrOverall.rds",
                        compress="xz"
                    )
                    
                }
                
            }
                
            # Return
            return(plt_stat_PrevUndrOverall)
        }
    )
    
    # . . MissingNess Plot ----
    output$plt_stat_MissingData <- renderPlot(
        expr={
            
            # Check if exists in local environment
            if (!exists("plt_stat_MissingData")) {
                
                # Check if exists in local directory
                if (file.exists("./figure/plt_stat_MissingData.rds")) {
                    
                    # Load
                    plt_stat_MissingData <<- read_rds("./figure/plt_stat_MissingData.rds")
                    
                } else {
                    
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
                        scale_y_continuous(breaks=seq(0,100,10), limits=c(0,100)) +
                        theme(panel.grid.minor.x=element_blank()) +
                        labs(
                            title="Percentage of Missing Values",
                            subtitle="Ordered by percentage missing",
                            y="Percentage Missing",
                            x="Variables",
                            color="Category",
                            fill="Category"
                        )
                        
                    # Save
                    write_rds(
                        x=plt_stat_MissingData,
                        path="./figure/plt_stat_MissingData.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            # Return
            return(plt_stat_MissingData)
            
        }
    )
    
    # . . Correlation Plot ----
    output$plt_corr_AllVariables <- renderPlot(
        expr={
            # Can't optimise this one because 'corrplot()' doesn't actually return a plot object...
            FaoStat_wide %>% 
                select(-country, -region, -year, -contains("_complete"), -contains("avg_undernourishment")) %>% 
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
            
            # Check if exists in local environment
            if (!exists("plt_ridg_UndernourishmentByYear")) {
                
                # Check if exists in local directory
                if (file.exists("./figure/plt_ridg_UndernourishmentByYear.rds")) {
                    
                    # Load
                    plt_ridg_UndernourishmentByYear <<- read_rds("./figure/plt_ridg_UndernourishmentByYear.rds")
                    
                } else {
                    
                    # Make
                    plt_ridg_UndernourishmentByYear <<- FaoStat_wide %>% 
                        {
                            ggplot(data=., aes(prevalence_of_undernourishment, reorder(year,desc(year)), fill=year)) +
                                geom_density_ridges() + 
                                scale_fill_manual(values=colorRampPalette(brewer.pal(9, "YlGn"))(nrow(unique(.["year"])))) +
                                labs(
                                    title="Undernourishment Per Year",
                                    subtitle="Ridge Plot",
                                    y="Year",
                                    x="Prevalence of Undernourishment"
                                )
                        }
                        
                    # Save
                    write_rds(
                        x=plt_ridg_UndernourishmentByYear,
                        path="./figure/plt_ridg_UndernourishmentByYear.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            # Return
            return(plt_ridg_UndernourishmentByYear)
            
        }
    )
    
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Feature Statistics                                                     ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    
    #------------------------------------------------------------------------------#
    # . Plots                                                                   ####
    #------------------------------------------------------------------------------#
    
    # . . Distributions Per Feature ----
    output$plt_hist_FeatureDistributions <- renderPlot(
        expr={
            
            # Check if exists in local environment
            if (!exists("plt_hist_FeatureDistributions")) {
                
                # Check if exists in local directory
                if (file.exists("./figure/plt_hist_FeatureDistributions.rds")) {
                    
                    # Load
                    plt_hist_FeatureDistributions <<- read_rds("./figure/plt_hist_FeatureDistributions.rds")
                    
                } else {
                    
                    # Make
                    plt_hist_FeatureDistributions <<- FaoStat_wide %>% 
                        select(-contains("_complete"), -contains("avg_undernourishment")) %>% 
                        plt_grob_MultipleHistograms(ExcludeFeatures=c("country", "region", "year"))
                    
                    # Save
                    write_rds(
                        x=plt_hist_FeatureDistributions,
                        path="./figure/plt_hist_FeatureDistributions.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            # Return
            return(plt_hist_FeatureDistributions)
            
        }
    )
    
    # . . Statistics Per Feature ----
    output$tbl_stat_DataFrameStats <- DT::renderDataTable(
        expr={
            FaoStat_wide %>%
                select(-c(num_complete,avg_undernourishment,pct_complete,cat_complete)) %>% 
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
    #    General Trends                                                         ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # . . Overall Trend ----
    output$plt_rese_genr_OverallTrend <- renderPlot(
        expr={
            
            # Optimise
            if (!exists("plt_rese_genr_OverallTrend")) {
                
                # Make
                plt_rese_genr_OverallTrend <<- FaoStat_wide %>% 
                    filter(!is.na(prevalence_of_undernourishment)) %>%
                    group_by(year) %>% 
                    summarise(avg_yearly_undernourishment=mean(prevalence_of_undernourishment)) %>% 
                    ungroup() %>% 
                    mutate(year=as.numeric(as.character(year))) %>% 
                    mutate(fit=lm(avg_yearly_undernourishment~year, data=.) %>% fitted.values()) %>% 
                    {
                        ggplot(data=., aes(x=year)) +
                            geom_line(
                                aes(y=avg_yearly_undernourishment, colour="PoU"),
                                size=1,
                                arrow=arrow(length=unit(0.3, "inches"))
                            ) +
                            geom_line(
                                aes(y=fit, colour="Trend"),
                                size=1,
                                arrow=arrow(length=unit(0.3, "inches"))
                            ) +
                            scale_x_continuous(breaks=seq(min(.["year"]), max(.["year"]))) +
                            scale_color_manual(values=c("forestgreen", "blue")) +
                            theme(panel.grid.minor.x=element_blank()) +
                            labs(
                                title="Prevalence of Undernourishment",
                                subtitle="Trend per Year",
                                y="Prevalence of Undernourishment",
                                x="Year",
                                colour="Value"
                            )
                    }
                
            }
            
            # Return
            plt_rese_genr_OverallTrend %>% return()
            
        }
    )
    
    # . . Regional Trend Data ----
    dat_rese_regi_InputData <- reactive({
        FaoStat_wide %>% 
            mutate(year=as.numeric(as.character(year))) %>% 
            filter(cat_complete!="empty") %>% 
            filter(region==input$rese_genr_inbx_SelectedRegions) %>% 
            filter(!is.na(prevalence_of_undernourishment)) %>% 
            group_by(region,year) %>% 
            summarise(avg_regional_undernourishment=mean(prevalence_of_undernourishment, na.rm=T)) %>% 
            ungroup() %>% 
            mutate(fit=lm(avg_regional_undernourishment~year, data=.) %>% fitted.values()) %>% 
            return()
    })
    
    # . . Regional Trend Plot ----
    output$plt_rese_genr_RegionalTrend <- renderPlot(
        expr={
            dat_rese_regi_InputData() %>% 
                {
                    ggplot(data=., aes(x=year)) +
                        geom_line(
                            aes(y=avg_regional_undernourishment, colour="PoU"),
                            size=1,
                            arrow=arrow(length=unit(0.3, "inches"))
                        ) +
                        geom_line(
                            aes(y=fit, colour="Trend"),
                            size=1,
                            arrow=arrow(length=unit(0.3, "inches"))
                        ) +
                        scale_x_continuous(breaks=seq(min(.["year"]), max(.["year"]))) +
                        scale_color_manual(values=c("darkorange", "blue")) +
                        theme(panel.grid.minor.x=element_blank()) +
                        labs(
                            title="Prevalence of Undernourishment",
                            subtitle=paste("Trend per Year","\n","For Region: ",input$rese_genr_inbx_SelectedRegions),
                            y="Prevalence of Undernourishment",
                            x="Year",
                            colour="Value"
                        )
                } %>% 
                return()
        }
    )
    
    # . . Country Trend Data ----
    dat_rese_ctry_InputData <- reactive({
        sel_country <- input$rese_genr_inbx_SelectedCountries %>% str_split(": ", simplify=T) %>% as.vector() %>% tail(1)
        FaoStat_wide %>% 
            mutate(year=as.numeric(as.character(year))) %>% 
            filter(cat_complete!="empty") %>% 
            filter(country==sel_country) %>% 
            filter(!is.na(prevalence_of_undernourishment)) %>% 
            group_by(country,year) %>% 
            summarise(avg_country_undernourishment=mean(prevalence_of_undernourishment, na.rm=T)) %>% 
            ungroup() %>% 
            mutate(fit=lm(avg_country_undernourishment~year, data=.) %>% fitted.values()) %>% 
            return()
    })
    
    # . . Country Trend Plot ----
    output$plt_rese_genr_CountryTrend <- renderPlot(
        expr={
            dat_rese_ctry_InputData() %>% 
                {
                    ggplot(data=., aes(x=year)) +
                        geom_line(
                            aes(y=avg_country_undernourishment, colour="PoU"),
                            size=1,
                            arrow=arrow(length=unit(0.3, "inches"))
                        ) +
                        geom_line(
                            aes(y=fit, colour="Trend"),
                            size=1,
                            arrow=arrow(length=unit(0.3, "inches"))
                        ) +
                        scale_x_continuous(breaks=seq(min(.["year"]), max(.["year"]))) +
                        scale_color_manual(values=c("magenta", "blue")) +
                        theme(panel.grid.minor.x=element_blank()) +
                        labs(
                            title="Prevalence of Undernourishment",
                            subtitle=paste("Trend per Year","\n","For Country: ",input$rese_genr_inbx_SelectedCountries %>% str_split(": ", simplify=T) %>% as.vector() %>% tail(1)),
                            y="Prevalence of Undernourishment",
                            x="Year",
                            colour="Value"
                        )
                } %>% 
                return()
        }
    )
    
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Most Successful                                                        ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # . . Set Data for Top Countries ----
    dat_rese_succ_TopCountries <- reactive({
        
        FaoStat_yearly %>% 
            {if (input$rese_succ_inbx_SelectedRegion != "All") {filter(., region==input$rese_succ_inbx_SelectedRegion)} else {.}} %>% 
            return()
    })
    
    # . . Set Plot for Top Countries ----
    output$plt_rese_succ_TopCountries <- renderPlotly(
        expr={
            dat_rese_succ_TopCountries() %>% 
                head(input$rese_succ_numb_NumberCountries) %>% 
                pivot_longer(contains("yr_"), names_to="year", values_to="prevalence_of_undernourishment") %>% 
                mutate(
                    year=str_replace_all(year, "yr_", ""),
                    year=as.character(year),
                    year=as.numeric(year)
                ) %>% 
                {
                    ggplot(data=., aes(x=year)) +
                        geom_line(
                            aes(y=prevalence_of_undernourishment, colour=country),
                            size=1,
                            alpha=0.5,
                            arrow=arrow(length=unit(0.2, "inches"))
                        ) +
                        scale_x_continuous(breaks=seq(min(.["year"]), max(.["year"]))) +
                        theme(
                            axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
                            panel.grid.minor.x=element_blank()
                        ) +
                        labs(
                            title=paste0(tags$b("Prevalence of Undernourishment"), "\n",
                                "Trend per Year per Country", "\n",
                                "For '", input$rese_succ_inbx_SelectedRegion, "' Region", "\n",
                                "For top '", input$rese_succ_numb_NumberCountries, "' Countries"
                            ),
                            y="Prevalence of Undernourishment",
                            x="Year",
                            colour="Country"
                        )
                }
        }
    )
    
    # . . Set Table ----
    output$tbl_succ_TopCountries <- DT::renderDataTable(
        expr={
            dat_rese_succ_TopCountries() %>% 
                return()
        },
        options=list(
            pageLength=input$rese_succ_numb_NumberCountries,
            # dom="ft",
            scrollX=TRUE,
            autoWidth=TRUE,
            columnDefs=list(list(ClassName="dt-left", width="auto", targets="_all"))
        )
    )
    
    # . . Set Data for Single Country ----
    dat_rese_succ_SingleCountry <- reactive({
        
        # Determine Country selection
        sel_country <- input$rese_succ_inbx_SelectedCountries %>% 
            str_split(": ", simplify=T) %>% 
            as.vector() %>% 
            tail(1)
        
        # Determine Predictor features
        val_predictors <- FaoStat_VariableMapping %>% 
            filter(type=="independant") %>% 
            select(variable) %>% 
            pull()
        
        # Set data
        FaoStat_wide %>% 
            mutate(year=as.numeric(as.character(year))) %>% 
            filter(country %in% sel_country) %>% 
            select(country, year, prevalence_of_undernourishment, all_of(val_predictors)) %>%
            pivot_longer(-c(country, year)) %>% 
            mutate(name=name %>% str_replace_all("_", " ") %>% str_to_title()) %>% 
            return()
        
    })
    
    # . . Set Plot for Sinlge Country ----
    output$plt_rese_succ_SingleCountry <- renderPlot(
        expr={
            
            # Determine Country selection
            sel_country <- input$rese_succ_inbx_SelectedCountries %>% 
                str_split(": ", simplify=T) %>% 
                as.vector() %>% 
                tail(1)
            
            # Make Plot
            dat_rese_succ_SingleCountry() %>% 
            {
                ggplot(data=., aes(x=year, colour=name, fill=name)) +
                    geom_area(
                        aes(y=value),
                        position="identity",
                        size=1,
                        alpha=0.1
                    ) +
                    scale_x_continuous(breaks=seq(min(.["year"]), max(.["year"]))) +
                    theme(
                        panel.grid.minor.x=element_blank(),
                        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
                        legend.position="none"
                    ) +
                    facet_wrap(~name, scales="free_y", ncol=3) +
                    labs(
                        title="Predictor Features over Time",
                        subtitle=paste0("For Countries: ", paste(sel_country, collapse=", ")),
                        y="Value",
                        x="Year",
                        colour="Country",
                        fill="Country"
                    )
            } %>% 
            return()
        }
    )
    
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Surprising Trends                                                      ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    # . . Increasing Country Tends ----
    output$plt_surp_AllCountries <- renderPlotly(
        expr={
            
            # Check if exists in local environment
            if (!exists("plt_surp_AllCountries")) {
                
                # Check if exists in local directory
                if (file.exists("./file/plt_surp_AllCountries.rds")) {
                    
                    # Load
                    plt_surp_AllCountries <<- read_rds("./figure/plt_surp_AllCountries.rds")
                    
                } else {
                    
                    # Make
                    plt_surp_AllCountries <<- FaoStat_wide %>% 
                        filter(country %in% {
                            FaoStat_yearly %>% 
                                filter(improvement>30) %>% 
                                select(country) %>% 
                                pull()
                        }) %>% 
                        mutate(year=as.numeric(as.character(year))) %>% 
                        {
                            ggplot(data=., aes(year, prevalence_of_undernourishment, colour=country)) +
                                geom_line() +
                                scale_x_continuous(
                                    breaks=seq(2001,2018),
                                    limits=c(2001,2018)
                                ) +
                                theme(
                                    axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
                                    panel.grid.minor.x=element_blank(),
                                    legend.position="none"
                                ) +
                                labs(
                                    title="Trends per Country",
                                    x="Year",
                                    y="Prevalence of Undernourishment",
                                    colour="Country"
                                )
                        }
                    
                    # Save
                    write_rds(
                        x=plt_surp_AllCountries,
                        path="./figure/plt_surp_AllCountries.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            # Return
            plt_surp_AllCountries %>% 
                ggplotly() %>% 
                return()
        }
    )
    
    # . . Arable Land ----
    output$plt_surp_ArableLand <- renderPlotly(
        expr={
            
            # Check if exists in local environment
            if (!exists("plt_surp_ArableLand")) {
                
                # Check if exists in local directory
                if (file.exists("./file/plt_surp_ArableLand.rds")) {
                    
                    # Load
                    plt_surp_ArableLand <<- read_rds("./figure/plt_surp_ArableLand.rds")
                    
                } else {
                    
                    # Make
                    plt_surp_ArableLand <<- FaoStat_wide %>% 
                        ggplot(aes(percentage_of_arable_land, prevalence_of_undernourishment, colour=country)) +
                        geom_point(alpha=0.4) +
                        theme(
                            legend.position="none"
                        ) +
                        labs(
                            title="Arable Land per Country",
                            x="Percentage of Arable Land",
                            y="Prevalence of Undernourishment"
                        )
                    
                    # Save
                    write_rds(
                        x=plt_surp_ArableLand,
                        path="./figure/plt_surp_ArableLand.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            # Return
            plt_surp_ArableLand %>% 
                ggplotly() %>% 
                return()
            
        }
    )
    
    # . . Change in GDP ----
    output$plt_surp_ChangeInGDP <- renderPlotly(
        expr={
            
            # Check if exists in local environment
            if (!exists("plt_surp_ChangeInGDP")) {
                
                # Check if exists in local directory
                if (file.exists("./file/plt_surp_ChangeInGDP.rds")) {
                    
                    # Load
                    plt_surp_ChangeInGDP <<- read_rds("./figure/plt_surp_ChangeInGDP.rds")
                    
                } else {
                    
                    # Make
                    plt_surp_ChangeInGDP <<- FaoStat_wide %>% 
                        {
                            ggplot(data=.) +
                            geom_point(
                                data=., 
                                aes(
                                    gross_domestic_product_per_capita_ppp, 
                                    prevalence_of_undernourishment, 
                                    colour=country
                                ), 
                                size=2, 
                                alpha=0.1
                            ) +
                            geom_line(
                                data=., 
                                aes(
                                    gross_domestic_product_per_capita_ppp, 
                                    prevalence_of_undernourishment, 
                                    colour=country
                                ), 
                                size=0.5, 
                                alpha=0.1
                            ) +
                            geom_point(
                                data=. %>% filter(country %in% c(
                                    "Eswatini",
                                    "Timor-Leste",
                                    "Zambia",
                                    "India"
                                )),
                                aes(
                                    gross_domestic_product_per_capita_ppp, 
                                    prevalence_of_undernourishment, 
                                    colour=country
                                ), 
                                size=3, 
                                alpha=0.6
                            ) +
                                geom_line(
                                    data=. %>% filter(country %in% c(
                                        "Eswatini",
                                        "Timor-Leste",
                                        "Zambia",
                                        "India"
                                    )),
                                    aes(
                                        gross_domestic_product_per_capita_ppp, 
                                        prevalence_of_undernourishment, 
                                        colour=country
                                    ), 
                                    size=1, 
                                    alpha=0.6
                                ) +
                            coord_cartesian(xlim=c(0,20000)) +
                            theme(
                                legend.position="none"
                            ) +
                            labs(
                                title="GDP vs PoU",
                                y="Prevalence of Undernourishment",
                                x="GDP per Capita"
                            )
                        }
                    
                    # Save
                    write_rds(
                        x=plt_surp_ChangeInGDP,
                        path="./figure/plt_surp_ChangeInGDP.rds",
                        compress="xz"
                    )
                    
                }
                
            }
            
            # Return
            plt_surp_ChangeInGDP %>% 
                ggplotly() %>% 
                return()
            
        }
    )
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Most Influential                                                       ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    
    #------------------------------------------------------------------------------#
    # . Links                                                                   ####
    #------------------------------------------------------------------------------#
    
    # . . To Data Dictionary page ----
    observeEvent(input$link_infl_over_DataDictionary_ToDataDictionary, {
        updateTabItems(
            session,
            "SidebarMenu",
            "dictionary"
        )
    })
    
    # . . To Undernourishment page ----
    observeEvent(input$link_infl_varimp_Undernourishment_ToUndernourishment, {
        updateTabItems(
            session,
            "SidebarMenu",
            "undernourishment"
        )
    })
    
    
    #------------------------------------------------------------------------------#
    # . Plots                                                                   ####
    #------------------------------------------------------------------------------#
    
    # . . Variable Importance ----
    output$plt_infl_VariableImportance <- renderPlot(
        expr={
                
            # Check if exists in local environment
            if (!exists("plt_infl_VariableImportance")) {
                
                # Check if exists in local directory
                if (file.exists("./file/plt_infl_VariableImportance.rds")) {
                    
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
            
            # Return
            return(plt_infl_VariableImportance)
            
        }
        
    )
    
    # . . Partial Dependency Plots ----
    output$plt_infl_PartialDependancy <- renderPlot(
        expr={
            
            # Check if exists in local environment
            if (!exists("plt_infl_PartialDependancy")) {
                
                # Check if exists in local directory
                if (file.exists("./figure/plt_infl_PartialDependancy.rds")) {
                    
                    # Load
                    plt_infl_PartialDependancy <<- read_rds("./figure/plt_infl_PartialDependancy.rds")
                    
                } else {
                    
                    # Run PDP function
                    plt_infl_PartialDependancy <<- plt_PartialDependencyPlots(
                        mod_gbm_Model,
                        mod_data_Raw,
                        mod_gbm_VariableImportance
                    )
                    
                }
                
                # Save file
                write_rds(
                    x=plt_infl_PartialDependancy,
                    path="./figure/plt_infl_PartialDependancy.rds",
                    compress="xz"
                )
                
            }
            
            # Render
            plt_infl_PartialDependancy %>% return()
            
        }
        
    )
    
    
    
    #------------------------------------------------------------------------------#
    #                                                                              #
    #    Conclusion                                                             ####
    #                                                                              #
    #------------------------------------------------------------------------------#
    
    #------------------------------------------------------------------------------#
    # . Links                                                                   ####
    #------------------------------------------------------------------------------#
    
    # . . GeneralTrend page ----
    observeEvent(
        {
            input$link_conc_conc_GeneralTrend_GoToPage
            input$link_conc_rese_GeneralTrend_GoToPage
        }, 
        {
            updateTabItems(
                session,
                "SidebarMenu",
                "general_trend"
            )
        }
    )
    
    # . . MostSuccessful page ----
    observeEvent(
        {
            input$link_conc_conc_MostSuccessful_GoToPage
            input$link_conc_rese_MostSuccessful_GoToPage
        }, 
        {
            updateTabItems(
                session,
                "SidebarMenu",
                "most_successful"
            )
        }
    )
    
    # . . SurprisingTrends page ----
    observeEvent(
        {
            input$link_conc_conc_SurprisingTrends_GoToPage
            input$link_conc_rese_SurprisingTrends_GoToPage
        }, 
        {
            updateTabItems(
                session,
                "SidebarMenu",
                "surprising_trends"
            )
        }
    )
    
    # . . MostInfluential page ----
    observeEvent(
        {
            input$link_conc_conc_MostInfluential_GoToPage
            input$link_conc_rese_MostInfluential_GoToPage
        }, 
        {
            updateTabItems(
                session,
                "SidebarMenu",
                "most_influential"
            )
        }
    )
    
    
}
