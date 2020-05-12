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

header <- dashboardHeader(
    title=tags$a(icon("seedling"),
                 "Explorer"
                 )
)

#------------------------------------------------------------------------------#
# Set Side Bar                                                              ####
#------------------------------------------------------------------------------#

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                 badgeLabel = "new", badgeColor = "green")
    )
)


#------------------------------------------------------------------------------#
# Set Body                                                                  ####
#------------------------------------------------------------------------------#

body <- dashboardBody(
    tabItems(
        tabItem(tabName="dashboard",
            h2("Dashboard tab content")
        ),
        tabItem(tabName="widgets",
            h2("Widgets tab content")
        )
    )
)


#------------------------------------------------------------------------------#
# Pull Together                                                             ####
#------------------------------------------------------------------------------#

# Set the UI ----
ui <- dashboardPage(
    header,
    sidebar,
    body
)

