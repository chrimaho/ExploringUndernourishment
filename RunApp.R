#------------------------------------------------------------------------------#
#                                                                              #
#    Title      : Run Shiny App                                                #
#    Purpose    : Trigger the App to run                                       #
#    Notes      : .                                                            #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : .                                                            #
#    Sources    : .                                                            #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#------------------------------------------------------------------------------#

# Trigger App ----
shiny::runApp(rprojroot::find_rstudio_root_file("/ExploringUndernourishment"))
