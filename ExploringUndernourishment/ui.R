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
    ,left_menu=tagList(
        dropdownBlock(
            id = "mydropdown",
            title = "Dropdown 1",
            icon = icon("sliders"),
            sliderInput(
                inputId = "n",
                label = "Number of observations",
                min = 10, max = 100, value = 30
            ),
            prettyToggle(
                inputId = "na",
                label_on = "NAs keeped",
                label_off = "NAs removed",
                icon_on = icon("check"),
                icon_off = icon("remove")
            )
        )
        # menuItem("Case growth rate",
        #          tabName = "growth_total_tab", 
        #          icon = icon("line-chart")
        #          )
    )
)



#------------------------------------------------------------------------------#
#                                                                              #
#    SideBar                                                                ####
#                                                                              #
#------------------------------------------------------------------------------#

sidebar <- dashboardSidebar(
    sidebarMenu(
        
        # Info page
        menuItem(
            "Introduction", 
            tabName="info",
            icon=icon("info-circle"),
            badgeLabel="info",
            badgeColor="light-blue"
        ),
        
        # Stats page
        menuItem(
            "Overall Statistics",
            icon=icon("chart-pie"),
            tabName="stats",
            menuSubItem(
                "Total",
                tabName="stats_total"
            ),
            menuSubItem(
                "Features",
                tabName="stats_features"
            )
        ),
        
        #Undernourishment
        menuItem(
            "Undernourishment",
            tabName="under",
            icon=icon("")
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
# Individual Definitions                                                    ####
#------------------------------------------------------------------------------#


# Define Info Page ----
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


# Define Stats Total Page ----
pag_StatTotalPage <- tabItem(
    tabName="stats_total",
    
    # Header
    h1("Overall Statistics"),
    
    # Distribution of target
    fluidRow(
        box(
            title="Explanation",
            width=4,
            "Explanation...!"
        ),
        box(
            title="Graph",
            width=8,
            plotOutput(
                outputId="plt_stat_PrevUndrOverall"
            )
        )
    ),
    
    # Percentage of missingness
    fluidRow(
        box(
            title="Explanation",
            width=4,
            "Explanation...?"
        ),
        box(
            title="Graph",
            width=8,
            plotOutput(
                outputId="plt_stat_MissingData",
                height="6in"
            )
        )
    ),
    
    # Correlation of all variables
    fluidRow(
        box(
            title="Corrplot",
            width=4,
            "Explanation"
        ),
        box(
            title="Corrplot",
            width=8,
            plotOutput(
                outputId="plt_corr_AllVariables",
                height="7in"
            )
        )
    )
)

# Define Stats Features Page ----
pag_StatFaeturesPage <- tabItem(
    tabName="stats_features",
    
    # Header
    h1("Feature-Wise Statistics"),
    
    # Distribution of all variables
    fluidRow(
        box(
            title="This",
            width=12,
            tags$div("Explanation"),
            column(
                width=12,
                plotOutput(
                    outputId="plt_hist_FeatureDistributions",
                    height="20in"
                )
            )
        )
    ),
    
    # Statistics of all variables
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
# Pull together                                                             ####
#------------------------------------------------------------------------------#

body <- dashboardBody(
    tabItems(
        pag_InfoPage,
        pag_StatFaeturesPage,
        pag_StatTotalPage
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

