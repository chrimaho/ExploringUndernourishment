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

# Define the Server ----
server <- function(input, output, session) {
    output$dat_DataDictionary <- renderTable(
        striped=TRUE,
        bordered=TRUE,
        hover=TRUE,
        spacing="xs",
        expr={
            FaoStat_VariableMapping
            }
        )
}
