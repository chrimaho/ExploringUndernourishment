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

# Toggle auto-reload (for design enhancements) ----
options(shiny.autoreload=TRUE)

# Load Functions ----
source("functions.R", local=TRUE)

# Import libraries ----

# Basics
library(tidyverse)
library(knitr)
library(stringr)
library(magrittr)
library(rprojroot)
library(assertthat)
library(naniar)
library(lubridate)
library(dynutils)

# Shiny
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

# Visualisations
library(plotly)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(GGally)
library(corrplot)
library(ggridges)

# Modeling
library(caret)
library(gbm)
library(pdp)
library(tictoc)

# Set defaults ----
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust=0.5)
            ,plot.subtitle = element_text(hjust=0.5)
            )
options(digits = 4)
options(scipen = 999)

# Process Data ----
source("data.R", local=TRUE)

# Process Model ----
source("model.R", local=TRUE)

# Define UI ----
source("ui.R", local=TRUE)

# Define Server ----
source("server.R", local=TRUE)
