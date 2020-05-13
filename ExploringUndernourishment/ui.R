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
# Set Header                                                                ####
#------------------------------------------------------------------------------#

header <- dashboardHeaderPlus(
    title=tags$span(icon("seedling"), "Explorer")
    # ,fixed=TRUE
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
        ),
        # menuItem("Case growth rate",
        #          tabName = "growth_total_tab", 
        #          icon = icon("line-chart")
        #          )
    )
)


#------------------------------------------------------------------------------#
# Set Side Bar                                                              ####
#------------------------------------------------------------------------------#

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Information", 
                 tabName="info", 
                 icon=icon("info-circle")),
        menuItem("Widgets", 
                 icon=icon("th"), 
                 tabName="widgets",
                 badgeLabel="new", 
                 badgeColor="green")
    )
)


#------------------------------------------------------------------------------#
# Set Body                                                                  ####
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
            tags$li(tags$a("Food and Agriculture Organization of the United Nations", href="http://www.fao.org/home/en/"), " (FAO)."), br(),
            "Raw data obtained from:", 
            tags$li(tags$a("FAO: Suite of Food Security Indicators", href="http://www.fao.org/faostat/en/#data/FS")), br(),
            "Additional information about the data sources can be found at:",
            tags$li(tags$a("FAO: Sustainable Development Goals: Indicator 2.1.1 - Prevalence of Undernourishment", href="http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/")),
            tags$li(tags$a("FAO: Food Security Indicators", href="http://www.fao.org/economic/ess/ess-fs/ess-fadata/en/#.XrXa5Wgzack")),
            tags$li(tags$a("Our World in Data: Hunger and Undernourishment", href="https://ourworldindata.org/hunger-and-undernourishment"))
        )
    ),
    fluidRow(
        box(
            title="Disclaimer:",
            width=12,
            "The data sources are provided as Open Source, and explored as Open Source.", br(),
            "The Author has no affiliation with the UN or with FAO, other than personal interest."
        )
    ),
    fluidRow(
        box(
            title="Data Dictionary",
            width=12,
            column(12, align="left", tableOutput(outputId="dat_DataDictionary"))
        )
    )
)


body <- dashboardBody(
    tabItems(
        pag_InfoPage,
        tabItem(tabName="widgets",
            h2("Widgets tab content")
        )
    )
)


#------------------------------------------------------------------------------#
# Pull Together                                                             ####
#------------------------------------------------------------------------------#

# Set the UI ----
ui <- dashboardPagePlus(
    header,
    sidebar,
    body
)

