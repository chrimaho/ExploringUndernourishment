#==============================================================================#
#                                                                              #
#    Title      : Shiny Global                                                 #
#    Purpose    : Declare Global variables to be used in the Shiny App         #
#    Notes      : .                                                            #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : .                                                            #
#    Sources    : .                                                            #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#

# Load Functions ----
source("functions.R", local=TRUE)

# Import libraries ----
library(googlesheets4)
library(tidyverse)
library(shiny)
library(magrittr)
library(ggridges)
library(assertthat)

# Set defaults ----
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust=0.5)
            ,plot.subtitle = element_text(hjust=0.5)
            )
options(digits = 4)
options(scipen = 999)

# Process Data ----
source("data.R", local=TRUE)

# Define UI ----
source("ui.R", local=TRUE)

# Define Server ----
source("server.R", local=TRUE)
