#==============================================================================#
#                                                                              #
#    Title      : Shiny User Interface                                         #
#    Purpose    : Define the layout of the Shiny App Interface                 #
#    Notes      : Utilises Shiny Dashboard functionality.                      #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : https://rstudio.github.io/shinydashboard/structure.html      #
#               : https://rinterface.com/shiny/shinydashboardPlus/             #
#               : https://shiny.rstudio.com/gallery/selectize-examples.html    #
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

# !Note! Available badgeColor values are:
# red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black

sidebar <- dashboardSidebar(
    sidebarMenu(
        
        # Info 
        menuItem(
            text="Introduction",
            selected=TRUE,
            tabName="info",
            icon=icon("info-circle"),
            badgeLabel="info",
            badgeColor="green"
        ),
        
        # Disclaimer
        menuItem(
            text="Disclaimer",
            # selected=TRUE,
            icon=icon("exclamation-triangle"),
            tabName="disclaimer",
            badgeLabel="legal",
            badgeColor="red"
        ),
        
        # Data Description
        menuItem(
            "Data Description",
            icon=icon("chart-pie"),
            tabName="data_description",
            startExpanded=TRUE,
            
            # Dictionary
            menuItem(
                "Dictionary",
                tabName="dictionary",
                icon=icon("book"),
                badgeLabel="ref",
                badgeColor="blue"
            ),
            
            # Undernourishment
            menuItem(
                "Undernourishment",
                tabName="undernourishment",
                icon=icon("seedling"),
                badgeLabel="food",
                badgeColor="blue"
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
                badgeColor="blue"
            )
            
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
    
    # Name ----
    tabName="info",
    
    # Header ----
    h1("Exploring Undernourishment"),
    h2("A visual data exploration for our better understanding"),
    
    # Introduction ----
    fluidRow(
        box(
            title=tags$b("Introduction"),
            width=12,
            tags$p(HTML(sprintf(
                "The United Nations (UN) has placed a lot of emphasis on their Sustainable Development Goals (SDG) (%s). One of which is Goal 2: Zero Hunger (UN 2020b), from which the UN has set up the Food and Agriculture Organisation (FAO 2020A). This organisation has embarked on a journey to help understand and address the worlds needs for access to food. One of the indicators that they have set up is the Prevalence of Undernourishment (FAO 2020b), which is defined as “an estimate of the proportion of the population whose habitual food consumption is insufficient to provide the dietary energy levels that are required to maintain a normal active and healthy life” (FAO 2020b).",
                tags$a("UN 2020a", href="#UN_2020_SustainableDevelopmentGoals")
            )))
        )
    ),
    
    # Research Questions ----
    fluidRow(
        box(
            title=tags$b("Research Question 1"),
            width=6,
            h6("Question:"),
            div("What has been the trend of Undernourishment in the last 20 years?"), br(),
            div("Hypothesis:"),
            div("There has been a general trend to decrease the prevelance over the last two decades."), br()
        ),
        box(
            title=tags$b("Research Question 2"),
            width=6,
            div("Question:"),
            div("Which country is most successfully addressing undernourishment?"), br(),
            div("Hypothesis:"),
            div("This."), br()
        )
    ),
    fluidRow(
        box(
            title=tags$b("Research Question 3"),
            width=6,
            div("Question:"),
            div("Have there been any substantial increases (or decreases) in undernourishment; and if so, why?"), br(),
            div("Hypothesis:"),
            div(""), br()
        ),
        box(
            title=tags$b("Research Question 4"),
            width=6,
            div("Question:"),
            div("Which of the indicators from FAO is most indicative of the prevalence of undernourishment (most influential feature)?"), br(),
            div("Hypothesis:"),
            div(""), br()
        )
    ),
    fluidRow(
        box(
            title=tags$b("Research Question 5"),
            width=6,
            div("Question:"),
            div("Are there any other interesting learnings to be taken from this data set? Such as: is it more important to have arable land or optimal trade routes?"), br(),
            div("Hypothesis:"),
            div(""), br()
        )
    )
    
)


#------------------------------------------------------------------------------#
# Disclaimer                                                                ####
#------------------------------------------------------------------------------#

pag_DisclaimerPage <- tabItem(
    
    # Name ----
    tabName="disclaimer",
    
    # Header ----
    h1("Disclaimer"),
    h3("This is the important information"),
    
    # Data Sources ----
    fluidRow(
        box(
            title=tags$b("Data Sources"),
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
    
    # Disclaimer ----
    fluidRow(
        box(
            title=tags$b("Disclaimer"),
            width=12,
            tags$li("The data sources are provided as Open Source, and explored as Open Source."),
            tags$li("The Author has no affiliation with the UN or with FAO, other than personal interest.")
        )
    ),
    
    # References ----
    fluidRow(
        box(
            title=tags$b("References"),
            width=12,
            tags$li(
                id="AbafitaAndKim_2014_DeterminantsOfHouseholdFoodSecurity",
                "Abafita & Kim 2014, ‘Determinants of Household Food Security in Rural Ethiopia: An Empirical Analysis’, Journal of Rural Development, vol. 37, no. 2, pp. 129-57, DOI: 10.22004/ag.econ.196613."
            ),
            tags$li(
                id="FAO_2020_FoodAndAgricultureOrganisation",
                "FAO 2020, Food and Agriculture Organisation of the United Nations, viewed 11 May 2020, <http://www.fao.org/home/en/>."
            ),
            tags$li(
                id="FAO_2019_TheStateOfFoodSecurity",
                "FAO 2019, The State of Food Security and Nutrition in the World: Safeguarding Against Economic Slowdowns and Downturns, viewed 16 May 2020, <http://www.fao.org/3/ca5162en/ca5162en.pdf>."
            ),
            tags$li(
                id="FAO_2020_SustainableDevelopmentGoals",
                "FAO 2020, Sustainable Development Goals: Indicator 2.1.1 - Prevalence of undernourishment, viewed 11 May 2020, <http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/>."
            ),
            tags$li(
                id="FAO_2020_FaoStat",
                "FAO 2020, FAOStat, viewed 7 May 2020, <http://www.fao.org/faostat/en/#data/FS>."
            ),
            tags$li(
                id="FAO_2020_EnhancedParametricApproach",
                "FAO 2020, Enhanced Parametric Approach Including In-Depth Thematic Analysis of Underlying Factors and Drivers Behind Food Security and Nutrition Trends, viewed 16 May 2020, <https://unstats.un.org/sdgs/metadata/files/Metadata-02-01-01.pdf>."
            ),
            tags$li(
                id="FontellAndLuchsinger_2011_SustainableEffortsToEradicate",
                "Fontell & Luchsinger 2011, ‘Sustainable efforts to eradicate Global hunger, undernourishment and malnutrition’, Journal of Global Business Issues, vol. 5, no. 2, pp. 79-83, ProQuest central database."
            ),
            tags$li(
                id="HassirFryEtAl_2015_SocioEconomicDeterminants",
                "Harris-Fry et al. 2015, ‘Socio-economic determinants of household food security and womens dietary diversity in rural Bangladesh: a cross-sectional study’, Journal of Health, Population and Nutrition, vol. 33, ISSN: 16060997, DOI: 10.1186/s41043-015-0022-0."
            ),
            tags$li(
                id="MbolanyiEtAl_2017_DeterminantsOfHouseholdFoodSecurity",
                "Mbolanyi et al. 2017, ‘Determinants of household food security in a rangeland area of Uganda’, African Journal of Rural Development, vol. 2, no. 2, pp. 213-23, ISSN: 2415-2838, DOI: 10.22004/ag.econ.262839."
            ),
            tags$li(
                id="MughalAndFontanSers_2020_SerialProductionUndernourishment",
                "Mughal & Fontan-Sers 2020, ‘Cereal production, undernourishment, and food insecurity in South Asia, Review of Development Economics, vol. 24, no. 2, pp. 524-45, Wiley Online Library, <https://doi-org.ezproxy.lib.uts.edu.au/10.1111/rode.12659>."
            ),
            tags$li(
                id="UN_2020_SustainableDevelopmentGoals",
                "UN 2020a, Sustainable Development Goals, viewed 11 May 2020, <https://www.un.org/sustainabledevelopment/sustainable-development-goals/>."
            ),
            tags$li(
                id="UN_2020_GoalTwoZeroHunger",
                "UN 2020b, Goal 2: Zero Hunger, viewed 11 May 2020, <https://www.un.org/sustainabledevelopment/hunger/>."
            ),
        )
    )
    
)


#------------------------------------------------------------------------------#
# Dictionary                                                                ####
#------------------------------------------------------------------------------#

pag_DictionaryPage <- tabItem(
    
    # Name ----
    tabName="dictionary",
    
    # Header ----
    h1("Data Dictionary"),
    
    # Data Dictionary ----
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
    
    # Header ----
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

pag_StatFeaturesPage <- tabItem(
    
    # Name ----
    tabName="stats_features",
    
    # Header ----
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
    
    # Name ----
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
        pag_DisclaimerPage,
        pag_DictionaryPage,
        pag_StatFeaturesPage,
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

