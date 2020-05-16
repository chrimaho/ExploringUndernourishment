#==============================================================================#
#                                                                              #
#    Title      : Shiny User Interface                                         #
#    Purpose    : Define the layout of the Shiny App Interface                 #
#    Notes      : Utilises Shiny Dashboard functionality.                      #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : https://rstudio.github.io/shinydashboard/structure.html      #
#    Sources    : .                                                            #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#


#------------------------------------------------------------------------------#
#                                                                              #
#    Header                                                                 ####
#                                                                              #
#------------------------------------------------------------------------------#

header <- dashboardHeaderPlus(
    title=tagList(
        span(class="logo-lg", icon("hand-holding-heart"), "Data Explorer"),
        icon("hand-holding-heart")
    )
    # ,left_menu=tagList(
    #     dropdownBlock(
    #         id = "mydropdown",
    #         title = "Dropdown 1",
    #         icon = icon("sliders"),
    #         sliderInput(
    #             inputId = "n",
    #             label = "Number of observations",
    #             min = 10, max = 100, value = 30
    #         ),
    #         prettyToggle(
    #             inputId = "na",
    #             label_on = "NAs keeped",
    #             label_off = "NAs removed",
    #             icon_on = icon("check"),
    #             icon_off = icon("remove")
    #         )
    #     )
    #     # menuItem("Case growth rate",
    #     #          tabName = "growth_total_tab", 
    #     #          icon = icon("line-chart")
    #     #          )
    # )
)



#------------------------------------------------------------------------------#
#                                                                              #
#    SideBar                                                                ####
#                                                                              #
#------------------------------------------------------------------------------#

sidebar <- dashboardSidebar(
    sidebarMenu(
        
        # Info 
        menuItem(
            "Introduction", 
            tabName="info",
            icon=icon("info-circle"),
            badgeLabel="info",
            badgeColor="green"
        ),
        
        # Overall Stats
        menuItem(
            "Overall Statistics",
            icon=icon("chart-pie"),
            tabName="stats_total",
            badgeLabel="big",
            badgeColor="blue"
        ),
        
        # Feature Stats
        menuItem(
            "Feature Statistics",
            icon=icon("chart-pie"),
            tabName="stats_features",
            badgeLabel="small",
            badgeColor="teal"
        ),
        
        # Undernourishment
        menuItem(
            "Undernourishment",
            tabName="undernourishment",
            icon=icon("seedling"),
            badgeLabel="food",
            badgeColor="orange"
        ),
        
        # Add Socials
        tags$hr(),
        tags$a("App By Chris Mahoney"), br(),
        tags$ol(tags$a(icon("linkedin"), "LinkedIn", href="https://www.linkedin.com/in/chrimaho/")),
        tags$ol(tags$a(icon("github"), "GitHub", href="https://github.com/chrimaho/ExploringUndernourishment/")),
        tags$ol(tags$a(icon("medium"), "Medium", href="https://medium.com/@chrimaho")),
        tags$ol(tags$a(icon("stack-overflow"), "StackOverflow", href="https://stackoverflow.com/users/12036005/chrimaho"))
        
    )
)



#------------------------------------------------------------------------------#
#                                                                              #
#    Body                                                                   ####
#                                                                              #
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Information                                                               ####
#------------------------------------------------------------------------------#

pag_InfoPage <- tabItem(
    tabName="info",
    h1("Exploring Undernourishment"),
    h3("A visual data exploration for our better understanding"),
    fluidRow(
        box(
            title="Data Sources",
            width=12,
            "Source data provided by:",
            tags$li(tags$a("Food and Agriculture Organization of the United Nations", href="http://www.fao.org/home/en/"), " (FAO)."),
            tags$br(),
            "Raw data obtained from:", 
            tags$li(tags$a("FAO: Suite of Food Security Indicators", href="http://www.fao.org/faostat/en/#data/FS")),
            tags$br(),
            "Additional information about the data sources can be found at:",
            tags$li(tags$a("FAO: Sustainable Development Goals: Indicator 2.1.1 - Prevalence of Undernourishment", href="http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/")),
            tags$li(tags$a("FAO: Food Security Indicators", href="http://www.fao.org/economic/ess/ess-fs/ess-fadata/en/#.XrXa5Wgzack")),
            tags$li(tags$a("Our World in Data: Hunger and Undernourishment", href="https://ourworldindata.org/hunger-and-undernourishment"))
        )
    ),
    fluidRow(
        box(
            title="Disclaimer",
            width=12,
            tags$div("The data sources are provided as Open Source, and explored as Open Source."),
            br(),
            tags$div("The Author has no affiliation with the UN or with FAO, other than personal interest.")
        )
    ),
    fluidRow(
        box(
            title="Data Dictionary",
            width=12,
            tags$div("A data dictionary is provided to ensure there is a description provided for each variable in the data table."),
            tags$br(),
            tags$br(),
            DT::dataTableOutput(outputId="tbl_info_DataDictionary")
        )
    )
)


#------------------------------------------------------------------------------#
# Overall Statistics                                                        ####
#------------------------------------------------------------------------------#

pag_StatTotalPage <- tabItem(
    tabName="stats_total",
    
    # Header
    h1("Overall Statistics"),
    
    # Distribution of target ----
    fluidRow(
        box(
            title="Histogram of Undernourishment",
            width=12,
            column(
                width=5,
                style="border: 1px double lightgrey;",
                tags$p("Section reserved for future comments.", style="color:red"),
            ),
            column(
                title="Graph",
                width=7,
                style="border: 1px double lightgrey;",
                plotOutput(
                    outputId="plt_stat_PrevUndrOverall"
                )
            )
        )
    ),
    
    # Percentage of missingness ----
    fluidRow(
        box(
            title="Percentage of Missing Data per Feature",
            width=12,
            column(
                width=5,
                style="border: 1px double lightgrey;",
                tags$p("Section reserved for future comments.", style="color:red"),
            ),
            column(
                title="Graph",
                width=7,
                style="border: 1px double lightgrey;",
                plotOutput(
                    outputId="plt_stat_MissingData",
                    height="6in"
                )
            )
        )
    ),
    
    # Correlation of all variables ----
    fluidRow(
        box(
            title="Correlation Plot of each Feature",
            width=12,
            column(
                width=5,
                style="border: 1px double lightgrey;",
                tags$p("Section reserved for future comments.", style="color:red"),
            ),
            column(
                title="Corrplot",
                width=7,
                style="border: 1px double lightgrey;",
                plotOutput(
                    outputId="plt_corr_AllVariables",
                    height="7in"
                )
            )
        )
    ),
    
    # Ridge Plot ----
    fluidRow(
        box(
            title="Ridge Plot of Undernourishment per Year",
            width=12,
            column(
                width=5,
                style="border: 1px double lightgrey;",
                tags$p("Section reserved for future comments.", style="color:red"),
            ),
            column(
                title="Ridge Plot",
                width=7,
                style="border: 1px double lightgrey;",
                plotOutput(
                    outputId="plt_ridg_UndernourishmentByYear", 
                    height="6in"
                )
            )
        )
    )
    
)


#------------------------------------------------------------------------------#
# Feature Statistics                                                        ####
#------------------------------------------------------------------------------#

pag_StatFaeturesPage <- tabItem(
    tabName="stats_features",
    
    # Header
    h1("Feature-Wise Statistics"),
    
    # Distribution of all variables ----
    fluidRow(
        box(
            title="This",
            width=12,
            tags$p("Section reserved for future comments.", style="color:red"),
            column(
                width=12,
                plotOutput(
                    outputId="plt_hist_FeatureDistributions",
                    height="20in"
                )
            )
        )
    ),
    
    # Statistics of all variables ----
    fluidRow(
        box(
            title="Data Frame Statistics",
            width=12,
            tags$div("The following output is a table of statistics for each field of the FAO data."),
            tags$br(),
            tags$div("Note the following:"),
            tags$li("The search bar has been included for ease of searching."),
            tags$li("There are more statistical features to the right!"),
            tags$li("The 'country' and 'year' features are both string type, and therefore do not have any statistical values."),
            tags$br(),
            tags$div("From this, the following information can be learnt:"),
            tags$li("First"),
            tags$li("Second"),
            DT::dataTableOutput(outputId="tbl_stat_DataFrameStats")
        )
    )
)


#------------------------------------------------------------------------------#
# Undernourishment                                                          ####
#------------------------------------------------------------------------------#

pag_Undernourishment <- tabItem(
    tabName="undernourishment",
    
    # Header ----
    h1("Aspects to Undernourishment"),
    
    # Dynamic part ----
    fluidRow(
        box(
            
            # Sub Header
            h2("Dynamic Part"),
            width=12,
            
            # Selections
            fluidRow(
                title=tags$b("Selections"),
                width=12,
                column(
                    width=3,
                    selectizeInput(
                        "undr_dynm_inbx_SelectedCountries",
                        h4("Select Countries"),
                        choices=FaoStat_wide %>% filter(cat_complete!="empty") %>% select(country) %>% distinct(),
                        selected="Thailand",
                        multiple=TRUE,
                    )
                ),
                column(
                    width=9,
                    sliderInput(
                        "undr_dynm_slid_SelectedYears",
                        h4("Select Years"),
                        min=2000,
                        max=2020,
                        value=c(2001,2019),
                        step=1,
                        sep="",
                        ticks=FALSE,
                        dragRange=TRUE
                    )
                )
            ),
            
            # Plots
            fixedRow(
                column(
                    title=tags$b("Improvement Per Year"),
                    width=6,
                    tags$p("Section reserved for future comments.", style="color:red"),
                    plotOutput(
                        outputId="undr_dynm_plot_ImprovementPerYear",
                        height="6in"
                    )
                ),
                column(
                    title=tags$b("Distribution Per Country"),
                    width=6,
                    tags$p("Section reserved for future comments.", style="color:red"),
                    plotOutput(
                        outputId="undr_dynm_plot_DistributionPerCountry",
                        height="6in"
                    )
                )
            )
        )
    ),
    
    
    # Static Part ----
    fluidRow(
        box(
            h2("Static Part"),
            width=12,
            column(
                title=tags$b("Completeness of Records"),
                width=6,
                style="border: 1px double lightgrey;",
                tags$p("Section reserved for future comments.", style="color:red"),
                plotOutput(
                    outputId="plt_undr_stat_Completeness",
                    height="20in"
                )
            ),
            column(
                title=tags$b("Ridges per country"),
                width=6,
                style="border: 1px double lightgrey;",
                tags$p("Section reserved for future comments.", style="color:red"),
                plotOutput(
                    outputId="plt_undr_stat_Ridges",
                    height="20in"
                )
            )
        )
    )
    
)

#------------------------------------------------------------------------------#
# Pull together                                                             ####
#------------------------------------------------------------------------------#

body <- dashboardBody(
    tabItems(
        pag_InfoPage,
        pag_StatFaeturesPage,
        pag_StatTotalPage,
        pag_Undernourishment
    )
)



#------------------------------------------------------------------------------#
#                                                                              #
#    Finalise                                                               ####
#                                                                              #
#------------------------------------------------------------------------------#

# Set the UI ----
ui <- dashboardPagePlus(
    header,
    sidebar,
    body
)

