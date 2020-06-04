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
        
        id="SidebarMenu",
        
        # . . Info ----
        menuItem(
            text="Introduction",
            # selected=TRUE,
            tabName="info",
            icon=icon("info-circle"),
            badgeLabel="info",
            badgeColor="blue"
        ),
        
        # . . Disclaimer ----
        menuItem(
            text="Disclaimer",
            # selected=TRUE,
            icon=icon("exclamation-triangle"),
            tabName="disclaimer",
            badgeLabel="legal",
            badgeColor="red"
        ),
        
        # . . Data Description ----
        menuItem(
            "Data Details",
            icon=icon("chart-pie"),
            tabName="data_description",
            startExpanded=TRUE,
            
            # Dictionary
            menuItem(
                "Data Dictionary",
                tabName="dictionary",
                # selected=TRUE,
                icon=icon("book"),
                badgeLabel="ref",
                badgeColor="olive"
            ),
            
            # Undernourishment
            menuItem(
                "Undernourishment",
                tabName="undernourishment",
                # selected=TRUE,
                icon=icon("seedling"),
                badgeLabel="food",
                badgeColor="olive"
            ),
            
            # Feature Interaction
            menuItem(
                "Feature Interactions",
                tabName="interactions",
                # selected=TRUE,
                icon=icon("project-diagram"),
                badgeLabel="corr",
                badgeColor="olive"
            ),
            
            # Overall Stats
            menuItem(
                "Overall Statistics",
                icon=icon("chart-pie"),
                tabName="stats_total",
                # selected=TRUE,
                badgeLabel="big",
                badgeColor="olive"
            ),
            
            # Feature Stats
            menuItem(
                "Feature Statistics",
                tabName="stats_features",
                # selected=TRUE,
                icon=icon("chart-pie"),
                badgeLabel="small",
                badgeColor="olive"
            )
            
        ),
        
        # . . Research Questions ----
        menuItem(
            "Research Questions",
            tabName="research_questions",
            icon=icon("graduation-cap"),
            startExpanded=TRUE,
            
            # General Trend
            menuItem(
                "General Trend",
                tabName="general_trend",
                # selected=TRUE,
                icon=icon("chart-line"),
                badgeLabel="general",
                badgeColor="aqua"
            ),
            
            # Most Successful
            menuItem(
                "Most Successful",
                tabName="most_successful",
                selected=TRUE,
                icon=icon("thumbs-up"),
                badgeLabel="good",
                badgeColor="aqua"
            ),
            
            # Surprising Trends
            menuItem(
                "Surprising Trends",
                tabName="surprising_trends",
                # selected=TRUE,
                icon=icon("surprise"),
                badgeLabel="wow",
                badgeColor="aqua"
            ),
            
            # Most Influential
            menuItem(
                "Most Influential",
                tabName="most_influential",
                # selected=TRUE,
                icon=icon("check-square"),
                badgeLabel="strong",
                badgeColor="aqua"
            )
            
        ),
        
        # . . Conclusion ----
        menuItem(
            "Conclusion",
            tabName="conclusion",
            icon=icon("hourglass-end"),
            badgeLabel="end",
            badgeColor="fuchsia"
        ),
        
        # . . Socials ----
        tags$hr(),
        tags$a("App Developer: Chris Mahoney"), br(),
        tags$ol(tags$a(icon("linkedin"), "LinkedIn", href="https://www.linkedin.com/in/chrimaho/")),
        tags$ol(tags$a(icon("github"), "GitHub", href="https://github.com/chrimaho/ExploringUndernourishment/")),
        tags$ol(tags$a(icon("medium"), "Medium", href="https://medium.com/@chrimaho")),
        tags$ol(tags$a(icon("stack-overflow"), "StackOverflow", href="https://stackoverflow.com/users/12036005/chrimaho"))
        
    )
)



#------------------------------------------------------------------------------#
#                                                                              #
#    Beginning Pages                                                        ####
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# . Information                                                             ####
#------------------------------------------------------------------------------#

pag_InfoPage <- tabItem(
    
    # . . Name ----
    tabName="info",
    
    # . . Header ----
    h1(tags$b("Exploring Undernourishment")),
    h2("A visual data exploration for our better understanding"),
    
    # . . Introduction ----
    fluidRow(
        box(
            title=tags$b(id="Intro", "Introduction"),
            width=12,
            tags$p(HTML(str_Format(
                "The {UN} (UN) has placed a lot of emphasis on their {SDG} (SDG), one of which is: {ZH}. To address this, the UN has set up the {FAO}. This organisation has embarked on a journey to help understand and address the worlds needs for access to food. One of the indicators that they have set up is the {PoU}, which is defined as: {def}.",
                UN=tags$a("United Nations", href="https://www.un.org/"),
                SDG=tags$a("Sustainable Development Goals", href="https://www.un.org/sustainabledevelopment/sustainable-development-goals/"),
                ZH=tags$a("Goal 2: Zero Hunger", href="https://www.un.org/sustainabledevelopment/hunger/"),
                FAO=tags$a("Food and Agriculture Oranisation", href="http://www.fao.org/home/en/"),
                PoU=tags$a("Prevalence of Undernourishment", href="http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/"),
                def=tags$i("an estimate of the proportion of the population whose habitual food consumption is insufficient to provide the dietary energy levels that are required to maintain a normal active and healthy life")
            ))),
            tags$p(HTML(sprintf(
                "This app uses data provided by the %s, and a number of exploratory data analysis techniques to investigate four reserach questions, which are detailed below.",
                tags$a("FAO", href="http://www.fao.org/faostat/en/#data/FS")
            )))
        )
    ),
    
    # . . Research Questions ----
    fluidRow(
        box(
            title=tags$b(id="RA1", "Research Area 1: General Trend"),
            width=6,
            tags$b("Question:"),
            tags$p("What has been the trend of Undernourishment in the last 20 years?"),
            tags$b("Hypothesis:"),
            tags$p("There has been a general trend to decrease the prevelance over the last two decades.")
        ),
        box(
            title=tags$b("Research Area 2: Most Successful Country"),
            width=6,
            tags$b("Question:"),
            tags$p("Which country is most successfully addressing undernourishment?"),
            tags$b("Hypothesis:"),
            tags$p("South-East Asian countries (for example, Vietnam) have made substantial progress in recent decades to break out of poverty; therefore there has been an associated success trend in their prevalence of undernourishment.")
        )
    ),
    fluidRow(
        box(
            title=tags$b("Research Area 3: Surprising Trends"),
            width=6,
            tags$b("Question:"),
            tags$p("Have there been any substantial increases (or decreases) in undernourishment?"),
            tags$b("Hypothesis:"),
            tags$p("Due to events happening in the Middle Eastern region of the world, this has lead to some negative results on some countries ability to address their Prevalence of Undernourishment score.")
        ),
        box(
            title=tags$b("Research Area 4: Most Influential Indicator"),
            width=6,
            tags$b("Question:"),
            tags$p("Which of the features in the FAO data set is most indicative of the prevalence of undernourishment (most influential feature)?"),
            tags$b("Hypothesis:"),
            tags$p("There will be an interesting trade-off between the self-sustaining indicators (such as amount of arable land and ability to grow crops), and the free-trade indicators (such as ).")
        )
    )
)


#------------------------------------------------------------------------------#
# . Disclaimer                                                              ####
#------------------------------------------------------------------------------#

pag_DisclaimerPage <- tabItem(
    
    # . . Name ----
    tabName="disclaimer",
    
    # . . Header ----
    h1("Disclaimer"),
    h3("This is the important information"),
    
    # . . Data Sources ----
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
    
    # . . Disclaimer ----
    fluidRow(
        box(
            title=tags$b("Disclaimer"),
            width=12,
            tags$li("The data sources are provided as Open Source, and explored as Open Source."),
            tags$li("The Author has no affiliation with the UN or with FAO, other than personal interest.")
        )
    ),
    
    # . . References ----
    fluidRow(
        box(
            title=tags$b("References"),
            width=12,
            tags$li(HTML(str_Format(
                "Abafita & Kim 2014, '{title}', {journal}, vol. 37, no. 2, pp. 129-57, DOI: 10.22004/ag.econ.196613.",
                title=tags$span("Determinants of Household Food Security in Rural Ethiopia: An Empirical Analysis"),
                journal=tags$i("Journal of Rural Development")
            ))),
            tags$li(HTML(str_Format(
                "FAO 2020, {title}, <{link}>.",
                title=tags$i("Food and Agriculture Organisation of the United Nations"),
                link=tags$a("http://www.fao.org/home/en/", href="http://www.fao.org/home/en/")
            ))),
            tags$li(HTML(str_Format(
                "FAO 2019, {title}, <{link}>.",
                title=tags$i("The State of Food Security and Nutrition in the World: Safeguarding Against Economic Slowdowns and Downturns"),
                link=tags$a("http://www.fao.org/3/ca5162en/ca5162en.pdf", href="http://www.fao.org/3/ca5162en/ca5162en.pdf")
            ))),
            tags$li(HTML(str_Format(
                "FAO 2020, {title}, <{link}>.",
                title=tags$i("Sustainable Development Goals: Indicator 2.1.1 - Prevalence of undernourishment"),
                link=tags$a("http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/", href="http://www.fao.org/sustainable-development-goals/indicators/2.1.1/en/")
            ))),
            tags$li(HTML(str_Format(
                "FAO 2020, {title}, <{link}>.",
                title=tags$i("FAOStat"),
                link=tags$a("http://www.fao.org/faostat/en/#data/FS", href="http://www.fao.org/faostat/en/#data/FS")
            ))),
            tags$li(HTML(str_Format(
                "FAO 2020, {title},  <{link}>.",
                title=tags$i("Enhanced Parametric Approach Including In-Depth Thematic Analysis of Underlying Factors and Drivers Behind Food Security and Nutrition Trends"),
                link=tags$a("https://unstats.un.org/sdgs/metadata/files/Metadata-02-01-01.pdf", href="https://unstats.un.org/sdgs/metadata/files/Metadata-02-01-01.pdf")
            ))),
            tags$li(HTML(str_Format(
                "Fontell & Luchsinger 2011, ‘{title}’, {journal}, vol. 5, no. 2, pp. 79-83, ProQuest central database.",
                title=tags$span("Sustainable efforts to eradicate Global hunger, undernourishment and malnutrition"),
                journal=tags$i("Journal of Global Business Issues")
            ))),
            tags$li(HTML(str_Format(
                "Harris-Fry et al. 2015, ‘{title}’, {journal}, vol. 33, ISSN: 16060997, DOI: 10.1186/s41043-015-0022-0.",
                title=tags$span("Socio-economic determinants of household food security and womens dietary diversity in rural Bangladesh: a cross-sectional study"),
                journal=tags$i("Journal of Health, Population and Nutrition")
            ))),
            tags$li(HTML(str_Format(
                "Mbolanyi et al. 2017, ‘{title}’, {journal}, vol. 2, no. 2, pp. 213-23, ISSN: 2415-2838, DOI: 10.22004/ag.econ.262839.",
                title=tags$span("Determinants of household food security in a rangeland area of Uganda"),
                journal=tags$i("African Journal of Rural Development")
            ))),
            tags$li(HTML(str_Format(
                "Mughal & Fontan-Sers 2020, ‘{title}’, {journal}, vol. 24, no. 2, pp. 524-45, Wiley Online Library.",
                title=tags$span("Cereal production, undernourishment, and food insecurity in South Asia"),
                journal=tags$i("Review of Development Economics")
            ))),
            tags$li(HTML(str_Format(
                "UN 2020a, {title}, <{link}>.",
                title=tags$i("Sustainable Development Goals"),
                link=tags$a("https://www.un.org/sustainabledevelopment/sustainable-development-goals/", href="https://www.un.org/sustainabledevelopment/sustainable-development-goals/")
            ))),
            tags$li(HTML(str_Format(
                "UN 2020b, {title}, <{link}>.",
                title=tags$i("Goal 2: Zero Hunger"),
                link=tags$a("https://www.un.org/sustainabledevelopment/hunger/", href="https://www.un.org/sustainabledevelopment/hunger/")
            )))
        )
    )
    
)



#------------------------------------------------------------------------------#
#                                                                              #
#    Data Details Pages                                                     ####
#                                                                              #
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# . Data Dictionary                                                         ####
#------------------------------------------------------------------------------#

pag_DictionaryPage <- tabItem(
    
    # . . Name ----
    tabName="dictionary",
    
    # . . Header ----
    h1("Data Dictionary"),
    
    # . . Data Dictionary ----
    fluidRow(
        box(
            title=tags$b("Data Dictionary"),
            width=12,
            tags$div("A data dictionary is provided to ensure there is a description provided for each variable in the data table."),
            tags$br(),
            tags$br(),
            DT::dataTableOutput(outputId="tbl_info_DataDictionary")
        )
    )
)


#------------------------------------------------------------------------------#
# . Undernourishment                                                  ####
#------------------------------------------------------------------------------#

pag_Undernourishment <- tabItem(
    
    # . . Name ----
    tabName="undernourishment",
    
    # . . Header ----
    h1("Aspects to Undernourishment"),
    
    # . . Dynamic part ----
    fluidRow(
        box(
            
            # Sub Header
            title=tags$b("Dynamic Part"),
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
                        choices=FaoStat_wide %>% filter(cat_complete!="empty") %>% select(country) %>% distinct() %>% pull(),
                        selected="Thailand",
                        multiple=TRUE
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
                    tags$p("The chart below shows the change in the Prevelance of Undernourishment per year, for the selected countries. Use the selections above to change the country and the years to focus in on relevant parts."),
                    tags$span("Some key call-outs include:"),
                    tags$li("Thailand has had a very steep descent between 2001 and 2007, followed by a slight plateau to 2010, then a steady decline to 2018."),
                    tags$li("Angola has had a steady and consistent decrease over all the years."),
                    tags$li("Afganistan has had a constant decrease from 2002 to 2011, followed by an increase to 2018 with a trend to plateau thereafter."),
                    plotlyOutput(
                        outputId="undr_dynm_plot_ImprovementPerYear",
                        height="6in"
                    )
                ),
                column(
                    title=tags$b("Distribution Per Country"),
                    width=6,
                    tags$p("Adjusting the controls above will update the chart below by adding more countries to the plot, or adjusting the yearly comparisons. Needless to say, each country will have a distinct shape for their Prevelance of Undernourishment, as they have all adopted different strategies and options for addressing the issue."),
                    tags$span("Some key call-outs include:"),
                    tags$li("Brunei shows a very strong, tight grouping close to zero, indicating they have a good score, and are doing well to maintain it."),
                    tags$li("Central Aftican Republic has an inconsistent and unstable score, spread out over a broad range of scores, indicating that there has been a broad-reaching change in their scores over time."),
                    tags$li("Bangladesh has a tight clustering, but not close to zero, indicating that there is not any change in their scores over time, and no effor to improve."),
                    plotOutput(
                        outputId="undr_dynm_plot_DistributionPerCountry",
                        height="6in"
                    )
                )
            )
        )
    ),
    
    
    # . . Static Part ----
    fluidRow(
        box(
            title=tags$b("Static Part"),
            width=12,
            fluidRow(
                column(
                    title=tags$b("Completeness of Records"),
                    width=6,
                    tags$p("Most of the countries will full scores are ones the ones that are expected to have low scores in this area (steriotypical 'third-world' countries); and most of the missing scores are from well-off countries (steriotypical 'first-world' countries'). This indicates toward a bias in the data collection strategy, assuming that these countries in the first-world do not have any issues with their Undernourishment. This is an unfair assumption when considering examples such as the Aboriginal Australians or the Indian Americans or the Native Africans in South Africa. Therefore attention needs to be paid to collect data in these countries."),
                    tags$p("Moreover, there are examples of 'third-world' countries that are still missing data. What about Palau or Grenada or Tajikistan? These countries also deserve to have data collected for them."),
                    plotOutput(
                        outputId="plt_undr_stat_Completeness",
                        height="20in"
                    )
                ),
                column(
                    title=tags$b("Ridges per country"),
                    width=6,
                    tags$p("There are many countries which show strong and consistent results, close to zero (for example, Belarus, Ukraine and Malaysia); indicating a strong and consistent effort to maintain a low PoU score. There are also some countries with a long and broad score (like Djibouti, Rwanda and Ethiopia), indicating big changes in their score over time; either positive or negative."),
                    tags$p("However, there also appears to be a third category, one where there is a distint bi-nomial pattern (such as Botswana, Peru and Lebanon), which indicates that there has either been a drop followed by a rise in scores, or lot of high scores followed by a steep drop and a lot of low scores. Either way, the patterns for these countries are intrigueing and worth further exploration."),
                    plotOutput(
                        outputId="plt_undr_stat_Ridges",
                        height="20in"
                    )
                )
            )
        )
    ),
    
    # . . Features by Target ----
    fluidRow(
        box(
            title=tags$b("Features by Target"),
            width=12,
            fluidRow(
                column(
                    width=4,
                    tags$p("This visualisation may appear overwhelming at first, but it is actually quite intuitive. It can be read as follows"),
                    tags$li(HTML(paste0("Each plot shows: The ", tags$code("Prevalence of Undernourishment"), " feature on the y-axis, and a different other feature on the x-axis."))),
                    tags$li(HTML(paste0("Each plot is a consistent correlogram showing the change in ", tags$code("Prevalence of Undernourishment"), ", as that particular feature changes."))),
                    tags$li("There is a blue line added to each plot, indicating the line of best fit for each plot."), tags$br(), 
                    tags$p(HTML(paste0("This information is helpful for understanding exactly how the ", tags$code("Prevalence of Undernourishment"), " changes with respect to each of the other features in the data set.")))
                ),
                column(
                    width=8,
                    tags$p("While some features appear to be a 'cloud of data points', others appear to show an intuitive, helpful pattern. For example:"),
                    tags$ul(
                        tags$li("These features appear to like a nervous firefly on the page:"),
                        tags$ul(
                            lapply(FUN=function(x) tags$li(tags$code(x)), c(
                                "Access To Basic Drinking Water",
                                "Access To Basic Sanitation Services",
                                "Prevalence of Anemia",
                                "Access To Improved Drinking Water",
                                "Access To Improved Sanitation Services",
                                "Prevalence Of Low Birth Rate"
                            ))
                        )
                    ),
                    tags$ul(
                        tags$li("These features appear like a cloud of data points:"),
                        tags$ul(
                            lapply(FUN=function(x) tags$li(tags$code(x)), c(
                                "Childern Affected By Wasting",
                                "Children Who Are Overweight",
                                "Children Who Are Stunted",
                                "Prevalence of Breastfeeding Women"
                            ))
                        )
                    ),
                    tags$ul(
                        tags$li("These features appear to be quite predictive and helpful:"),
                        tags$ul(
                            lapply(FUN=function(x) tags$li(tags$code(x)), c(
                                "Avg Dietary Adequacy",
                                "Avg Supply Of Protein Of Animal Origin",
                                "Avg Protein Supply"
                            ))
                        )
                    ),
                )
            ),
            fluidRow(
                column(
                    width=12,
                    plotOutput(
                        outputId="plt_undr_FeaturesByTarget",
                        height="20in"
                    )
                )
            )
        )
    )
    
)


#------------------------------------------------------------------------------#
# . Feature Interactions                                                    ####
#------------------------------------------------------------------------------#

pag_StatFeatureInteractionsPage <- tabItem(
    
    # . . Name ----
    tabName="interactions",
    
    # . . Header ----
    h1("Feature Interactions"),
    
    # . . Dynamic Plotting ----
    fluidRow(
        box(
            
            title=tags$b("Dynamic Plotting"),
            width=12,
            
            # Comments
            fluidRow(
                column(
                    width=12,
                    tags$p("This plot provies an interactive way for the data to be explored."),
                    tags$p("In order to use the charts effectively, note the following:"),
                    tags$ul(
                        tags$li("There are three charts, including one Dot-Plot in the middle, which is the main plot, one Density-Plot at the top, and one Violin-Plot to the right."),
                        tags$li("The main Dot-Plot in the middle has the below features:"),
                        tags$ul(
                            tags$li("Each dot represents a data point that is the intersection between the X-Feature and the Y-Feature."),
                            tags$li("The wavey-lines around the outside represent a 3D-Density plot. The lines encompass the data points in the same way that altitude lines encircle mountains on a map."),
                            tags$li("The different colours represent different countries, which is consistent with the other plots.")
                        ),
                        tags$li("The Density-Plot at the top has the following features:"),
                        tags$ul(
                            tags$li("The colours are different per country."),
                            tags$li("The lines represent the count of the number of data points, as consistent with the dot-plot below."),
                            tags$li("The more data points, the 'higher' the lines.")
                        ),
                        tags$li("The Violin-Plot to the right has the following features:"),
                        tags$ul(
                            tags$li("The colours are different per country."),
                            tags$li("The shapes are similar to the density plot, except they are flipped on the side, and are representing the density of data points, as consistent with the main Dot-Plot."),
                            tags$li("The more data points, the 'fatter' the Violin.")
                        )
                    ),
                    tags$p("This plot helps to understand hiw every single feature affects all other features; as grouped by each country.")
                )
            ),
            
            # Selections
            fluidRow(
                title=tags$b("Selections"),
                width=12,
                column(
                    width=4,
                    selectizeInput(
                        "inta_dynm_inbx_SelectedCountries",
                        h4("Select Countries"),
                        choices=FaoStat_wide %>% filter(cat_complete!="empty") %>% select(country) %>% distinct(),
                        selected=c("Thailand", "Viet Nam"),
                        multiple=TRUE,
                        options=list(maxItems=5)
                    )
                ),
                column(
                    width=4,
                    selectizeInput(
                        "inta_dynm_inbx_SelectedYFeature",
                        h4("Select Y Feature"),
                        choices=FaoStat_wide %>% select(-c("year", "region", "country", "num_complete", "avg_undernourishment", "pct_complete", "cat_complete")) %>% names(),
                        selected="prevalence_of_undernourishment",
                        multiple=FALSE
                    )
                ),
                column(
                    width=4,
                    selectizeInput(
                        "inta_dynm_inbx_SelectedXFeature",
                        h4("Select X Feature"),
                        choices=FaoStat_wide %>% select(-c("year", "region", "country", "num_complete", "avg_undernourishment", "pct_complete", "cat_complete")) %>% names(),
                        selected="avg_value_of_food_production",
                        multiple=FALSE
                    )
                )
            ),
            
            # Plotting
            fluidRow(
                column(
                width=12,
                    plotOutput(
                        outputId="plt_inta_MultiFeatures",
                        height="7in"
                    )
                )
            )
        )
    )
)


#------------------------------------------------------------------------------#
# . Overall Statistics                                                      ####
#------------------------------------------------------------------------------#

pag_StatTotalPage <- tabItem(
    
    # . . Name ----
    tabName="stats_total",
    
    # . . Header ----
    h1("Overall Statistics"),
    
    # . . Distribution of target ----
    fluidRow(
        box(
            title=tags$b("Histogram of Undernourishment", id="MyRef"),
            width=12,
            column(
                width=5,
                tags$p(HTML(paste0("The plot to the right shows the distribution of scores for the ", tags$code("Prevalence of Undernourishment"), " feature."))),
                tags$p("As seen:"),
                tags$ul(
                    tags$li("There is a sharp peak at a score of 0.04."),
                    tags$li("It has a long right-tail out to a score of 0.7."),
                    tags$li("There are some bumps and inconsistencies, but nothing to indicate any multi-modal distribution.")
                )
            ),
            column(
                title="Graph",
                width=7,
                plotOutput(
                    outputId="plt_stat_PrevUndrOverall"
                )
            )
        )
    ),
    
    # . . Percentage of missingness ----
    fluidRow(
        box(
            title=tags$b("Percentage of Missing Data per Feature"),
            width=12,
            column(
                width=5,
                tags$p("The plot to the right is a Lollipop-Plot, indicating the percentage of NA records for each feature."),
                tags$p("The further the lollipop is to the right, the more NA records there are for that feature."),
                tags$p("The colours of the plot indicate a differentiate between the category of each of the features, as determined by the Data Dictionary."),
                tags$p("As seen:"),
                tags$ul(
                    tags$li("The top four features are all Food Security features."),
                    tags$li("The following four top features are all health features."),
                    tags$li("40% of the features are missing over 50% of their data; which are mostly all health or food security features."),
                    tags$li("This level of missingness could be indicative of:"),
                    tags$ul(
                        tags$li("These features have only recently begun being recorded"),
                        tags$li("There is a level of inconsistency in the countries for the collection of this data."),
                        tags$li("Not every country is mandated to collect this data and report it back to the FAO or UN.")
                    ),
                    tags$li("Only the identifier columns have 100% of data collected for them.")
                )
            ),
            column(
                title="Graph",
                width=7,
                plotOutput(
                    outputId="plt_stat_MissingData",
                    height="6in"
                )
            )
        )
    ),
    
    # . . Correlation of all variables ----
    fluidRow(
        box(
            title=tags$b("Correlation Plot of each Feature"),
            width=12,
            column(
                width=5,
                tags$p("For all the variables, it's important to understand how each one interacts with one another. The Plot to the right illustrates this correlation."),
                tags$p("Note the following:"),
                tags$ul(
                    tags$li(HTML(paste0("The ", tags$code("Prevalence Of Undernourishment"), " feature is at the very bottom, so it is convenient to scroll the eyes along to see each variable."))),
                    tags$li("The Red colour indicates variables that have a strong negative correlation, while the blue colours indicate variables with a strong positive correlation."),
                    tags$li("In other words:"),
                    tags$ul(
                        tags$li("The \"Red'er\" it is, the stronger the Negative correlation."),
                        tags$li("The \"Blue'er\" it is, the stronger the Positive correlation.")
                    ),
                    tags$li("The coser to white the colour is, the closer the correlation is to Zero'"),
                    tags$li("The level of completeness of the pie correlates to the strength of the colour."),
                    tags$li("The Question Marks indicate variables which have a substantially large number of NA scores, and the algorithm is unable to calculate the correlation score. These variables are in-line with the above listed variables with a large number of NA values."),
                    tags$li("Only the bottom half of the matrix is included, as only half is needed to determine the correlation of each variable-pair.")
                )
            ),
            column(
                title="Corrplot",
                width=7,
                plotOutput(
                    outputId="plt_corr_AllVariables",
                    height="7in"
                )
            )
        )
    ),
    
    # . . Ridge Plot ----
    fluidRow(
        box(
            title=tags$b("Ridge Plot of Undernourishment per Year", id="MyTest"),
            width=12,
            column(
                width=5,
                tags$p("This plot is perhaps the most indicative of all the plots on this page. It indicates the level of Undernourishment per year, from the earliest measurements at the top and the latest measurements at the bottom."),
                tags$p("Note the following:"),
                tags$ul(
                    tags$li("The colour of the plots are just arbitrary, simply used to differentiate each year from the next."),
                    tags$li("Each year, the scores are distinctly getting 'tighter' together, as seen by the 2001 scores being quite spread out and diverse but the scores for 2018 being quite tight and clustered close to Zero."),
                    tags$li("Each Plot is a density distribution, showing the number of scores for that year, in a similar way a histogram also shows a distribution."),
                    tags$li("The Plot is a little bit misleading in it's indication of negative scores. Most notably:"),
                    tags$ul(
                        tags$li("Each of the scores indicate it is possible to have scores below Zero."),
                        tags$li("Contrary to what this plot indicates, it is actually not possible to have a negative PoU score."),
                        tags$li("Each PoU score should be between 0 and 1."),
                        tags$li("The error is caused by the algorithm used to calculate the Density diagram, and that it 'normalises' scores on either side of the min & max scores.")
                    ),
                    tags$li("Overall, the plot indicates an overall strong positive trend, indicating that the FAO and UN have been doing a positive job.")
                )
            ),
            column(
                title="Ridge Plot",
                width=7,
                plotOutput(
                    outputId="plt_ridg_UndernourishmentByYear", 
                    height="6in"
                )
            )
        )
    )
    
)


#------------------------------------------------------------------------------#
# . Feature Statistics                                                      ####
#------------------------------------------------------------------------------#

pag_StatFeaturesPage <- tabItem(
    
    # . . Name ----
    tabName="stats_features",
    
    # . . Header ----
    h1("Feature-Wise Statistics"),
    
    # . . Distribution of all variables ----
    fluidRow(
        box(
            title=tags$b("Distribution of Each Feature", id="FeatStats"),
            width=12,
            fluidRow(
                column(
                    width=3,
                    tags$p("Each of the features included in the data set each have their own distribution; their own shape. In order to fully understand the data that is being dealt with, reviewing their distrubution is necessary."),
                    tags$p(
                        tags$span("This is a test"),
                        actionLink(
                            "link_feat_VariableDistributions_ToOverallStatistics",
                            "of a link"
                        ),
                        tags$span("to another page.")
                    )
                ),
                column(
                    width=4,
                    tags$p("The following information can be obtained from reviewing the distributions:"),
                    tags$ul(
                        tags$li("There are multiple features that appear to have a neat, normal distribution. Including:"),
                        tags$ul(
                            lapply(FUN=function(x) tags$li(tags$code(x)), c(
                                "Political Stability",
                                "Avg Dietary Adequacy",
                                "Avg Protein Supply",
                                "Avg Supply Of Protein Of Animal Origin",
                                "Caloric Energy From Cereals Roots Tubers",
                                "Choldren Who Are Stunted",
                                "Prevalence of Breastfeeding Women"
                            ))
                        ),
                        tags$li("There are a few features which appear to have a bi-nomial distribution, which is worthy of furhter exploration:"),
                        tags$ul(
                            lapply(FUN=function(x) tags$li(tags$code(x)), c(
                                "Prevalence of Anemia",
                                "Prevalence of Obesity",
                                "Avg Protein Supply",
                                "Cereal Import Dependency Ratio",
                                "Access To Improved Sanitation Services"
                            ))
                        )
                    )
                ),
                column(
                    width=4,
                    tags$p("The following information can be obtained from reviewing the distributions:"),
                    tags$ul(
                        tags$li("There are a number of features with very long right-tails, indicating a positive skew:"),
                        tags$ul(
                            lapply(FUN=function(x) tags$li(tags$code(x)), c(
                                "Prevalence Of Undernourishment",
                                "Food Production Variability",
                                "Food Supply Variability",
                                "Avg Value Of Food Production",
                                "Food Imports As Share Of Merch Exports",
                                "Number People Undernourished",
                                "Gross Domestic Product Per Capita Ppp",
                                "Children Affected By Wasting",
                                "Children Who Are Overweight",
                                "Number Moderate Food Insecurity",
                                "Number Severe Food Insecurity",
                                "Prevelance Moderate Food Insecurity",
                                "Prevalence Severe Food Insecurity"
                            ))
                        ),
                        tags$li("There are also some features with left-tail distributions, indicating a negative skew:"),
                        tags$ul(
                            lapply(FUN=function(x) tags$li(tags$code(x)), c(
                                "Access To Basic Drinking Water",
                                "Access To Basic Sanitation Services",
                                "Access To Improved Drinking Water",
                                "Access To Improved Sanitation Services"
                            ))
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    width=12,
                    plotOutput(
                        outputId="plt_hist_FeatureDistributions",
                        height="20in"
                    )
                )
            )
        )
    ),
    
    # . . Statistics of all variables ----
    fluidRow(
        box(
            title=tags$b("Data Frame Statistics"),
            width=12,
            fluidRow(
                column(
                    width=12,
                    tags$p("The following output is a table of statistics for each field of the FAO data."),
                    tags$p("Note the following:"),
                    tags$ul(
                        tags$li("The search bar has been included for ease of searching."),
                        tags$li("There are more statistical features to the right!"),
                        tags$li(HTML(paste("The", tags$code("country"), ",", tags$code("region"), "and", tags$code("year"), "features are all string type, and therefore do not have any statistical values."))),
                        tags$li("Each of the important statistical measurements are included."),
                    ),
                    tags$p("It's important to have this level of detail available, because it will inform later analysis. It also places a numeric value to the above distrubutions, allowing the quantification of the visualisations."),
                )
            ),
            fluidRow(
                column(
                    width=12,
                    DT::dataTableOutput(outputId="tbl_stat_DataFrameStats")
                )
            )
        )
    )
)



#------------------------------------------------------------------------------#
#                                                                              #
#    Research Question Pages                                                ####
#                                                                              #
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# . General Trend                                                           ####
#------------------------------------------------------------------------------#

pag_ReseGeneralTrend <- tabItem(
    
    # . . Name ----
    tabName="general_trend",
    
    # . . Header ----
    h1("General Trend"),
    
    # . . Overall Trend ----
    fluidRow(
        box(
            title=tags$b("Overall Trend"),
            width=12,
            column(
                width=4,
                tags$p(
                    "The aggregated score for all countries combined is displayed to the right. The actual scores for the {PoU} are indicated in the {green} colour, while the linear trend is displayed in {blue} colour. The actual trend indicates a steady and consistent decrease between 2001 and 2012, showing an overall decrease of aproximately {score}. The tend plateaued between 2012 and 2016, with minimal decrease in the score. Then, 2017 saw an increase in the score in the first time since the data collection began; and 2018 saw an even higher increase. This trend is expected to continue in 2019." %>% 
                    str_Format(
                        PoU=code("Prevalence of Undernourishment"),
                        green=span("green", style="color:forestgreen"),
                        blue=span("blue", style="color:blue"),
                        score=code("0.03")
                    ) %>% 
                    HTML()
                )
            ),
            column(
                width=8,
                plotOutput(
                    outputId="plt_rese_genr_OverallTrend"
                )
            )
        )
    ),
    
    # . . Regional Trend ----
    fluidRow(
        box(
            title=tags$b("Regional Trend"),
            width=12,
            column(
                width=5,
                tags$p(
                    "When segmented by Region, the data tells a different story. In the plot to the right, the linear trend is maintained in {blue} while the actual values for the selected region are indicated in {orange}. The drop-down box at the top allows for the selection of different regions, so that the trend for just that region can be viewed." %>% 
                    str_Format(
                        blue=span("blue", style="color:blue"),
                        orange=span("orange", style="color:darkorange")
                    ) %>% 
                    HTML()
                ),
                tags$p(
                    tags$span("The following observations can be made:"),
                    tags$ul(
                        tags$li(
                            "The {} region indicates the most consistent and steady decline over time, with no indicated increase in scores." %>% 
                            str_Format(
                                code("Asia & Pacific")
                            ) %>% 
                            HTML()
                        ),
                        tags$li(
                            "The {} region also shows no sign of increasing. Albeit there have been some years along the way that have seen some minor increases, the overall trend for the region is in a positive downward direction." %>% 
                            str_Format(
                                code("Europe")
                            ) %>% 
                            HTML()
                        ),
                        tags$li(
                            "The {} region shows an impressive decline before an incredibly sharp increase between 2012 and 2013, then the general trend continues to 2015 before plateauing and then increasing in 2018. This step change can either indicate a change in the data collection methods, or a change in the countries being recorded (an addition or removal of a country)" %>% 
                            str_Format(
                                code("Arab States")
                            ) %>% 
                            HTML()
                        ),
                        tags$li(
                            "The {} region shows the smoothest transition of all regions, with a healthy decrease until 2014 before swinging to a steady increase since." %>% 
                            str_Format(
                                code("Africa")
                            ) %>% 
                            HTML()
                        ),
                        tags$li(
                            "The {} region shows a trend typical to the others, with a steady increase to 2017 before plateauing in 2018." %>% 
                            str_Format(
                                code("South/Latin America")
                            ) %>% 
                            HTML()
                        ),
                        tags$li(
                            "The {} region is the most shocking of all. It shows a sharp, drastic drop in the score to 2004, before showing a sigmoidal shape with its trough at 2012, then showing a strong, steady, unhealthy increase every year since. This shape indicates a lack of political willingness to change this curve, and poor governmental policy across the region as a whole." %>% 
                            str_Format(
                                code("Middle east")
                            ) %>% 
                            HTML()
                        )
                    )
                )
            ),
            column(
                width=7,
                fluidRow(
                    column(width=1),
                    column(
                        width=6,
                        selectizeInput(
                            inputId="rese_genr_inbx_SelectedRegions",
                            h4("Select Region"),
                            choices=FaoStat_wide %>% filter(cat_complete!="empty") %>% select(region) %>% distinct() %>% pull(),
                            selected=c("Asia & Pacific"),
                            multiple=FALSE
                        )
                    ),
                    column(width=5)
                ),
                fluidRow(
                    plotOutput(
                        outputId="plt_rese_genr_RegionalTrend"
                    )
                )
            )
        )
    ),
    
    # . . Country Trend ----
    fluidRow(
        box(
            title=tags$b("Country Trend"),
            width=12,
            column(
                width=4,
                tags$p("This Plot shows the trend specific for each individual country. The drop-down selector can change the country as required."),
                tags$p("Some key call-outs include:"),
                tags$ul(
                    tags$li(
                        "{} indicates a strong sigmoidal oscillation between 0.3 and 0.26 with wavelengh of 10 years." %>% 
                            str_Format(code("Iraq")) %>% HTML()
                    ),
                    tags$li(
                        "{} indicates a consistent upwards trend every single year." %>% 
                            str_Format(code("Lebanon")) %>% HTML()
                    ),
                    tags$li(
                        "{} Has seen a dramatic increase since 2012. It is this score that has substantially influenced the score for the rest of the {} region" %>% 
                            str_Format(code("Yemen"), code("Middle east")) %>% HTML()
                    ),
                    tags$li(
                        "Each of the countries in {} region show positive decreses in scores. Exept for countries like {}, {}, {}, {}, {}, and {} which indicate unstable and inconsistent scores." %>% 
                            str_Format(
                                code("Asia & Pacific"),
                                code("Brunei"),
                                code("Taiwan"),
                                code("DPRK"),
                                code("Malaysia"),
                                code("Maldives"),
                                code("New Caledonia"),
                                code("Vanuatu")
                            ) %>% HTML()
                    ),
                    tags$li(
                        "In the {} region, most of the countries have kept a consistent and impressive decreas in their score over time. Except for countries like {}, {}, {}, {}. These countries have seen an increas in their scores over time, and some remain quite high indeed." %>% 
                            str_Format(
                                code("Central African Republic"),
                                code("Chat"),
                                code("Congo"),
                                code("Eswatini"),
                                code("Gabon")
                            ) %>% 
                            HTML()
                    )
                )
            ),
            column(
                width=8,
                fluidRow(
                    column(width=1),
                    column(
                        width=6,
                        selectizeInput(
                            inputId="rese_genr_inbx_SelectedCountries",
                            h4("Select Country"),
                            choices=FaoStat_wide %>% filter(cat_complete!="empty") %>% select(region,country) %>% distinct() %>% mutate(value=paste(region, country, sep=": ")) %>% select(value) %>% arrange(value) %>% pull(),
                            selected=c("Asia & Pacific: Viet Nam"),
                            multiple=FALSE
                        )
                    ),
                    column(width=5)
                ),
                fluidRow(
                    column(
                        width=12,
                        plotOutput(
                            outputId="plt_rese_genr_CountryTrend"
                        )
                    )
                )
            )
        )
    )
    
)


#------------------------------------------------------------------------------#
# . Most Successful                                                         ####
#------------------------------------------------------------------------------#

pag_ReseMostSuccessful <- tabItem(
    
    # . . Name ----
    tabName="most_successful",
    
    # . . Header ----
    h1("Most Successful Countries"),
    
    # . . Comment and Plot ----
    fluidRow(
        box(
            title=tags$b("Overall"),
            width=12,
            column(
                width=4,
                tags$p("Credit where it is due, there are some very impressive reductions in the Prevalence of Undernourishment over time. This data can be explored in the chart to the right. To see the details of the each line, simply over over it."),
                tags$p(
                    "Countries like {}, {}, {}, and {} have begun with a score of over {}, and have each reduced their score by at least {} of their original value." %>% 
                        str_Format(
                            code("Angola"),
                            code("Ethiopia"),
                            code("Myanmar"),
                            code("Dominican Republic"),
                            code("0.2"),
                            code("50%")
                            ) %>% HTML()
                )
            ),
            column(
                width=8,
                fluidRow(
                    width=8,
                    column(
                        width=6,
                        selectizeInput(
                            inputId="rese_succ_inbx_SelectedRegion",
                            h4("Group by Region"),
                            choices=FaoStat_wide %>% filter(cat_complete!="empty") %>% select(region) %>% distinct() %>% pull() %>% c("All", .),
                            selected=c("All"),
                            multiple=FALSE
                        )
                    ),
                    column(
                        width=6,
                        numericInput(
                            inputId="rese_succ_numb_NumberCountries",
                            h4("Select Number of Countries"),
                            value=10
                        )
                    )
                ),
                fluidRow(
                    plotlyOutput(
                        outputId="plt_rese_succ_TopCountries"
                    )
                )
            )
        )
    ),
    
    # . . Table ----
    fluidRow(
        box(
            title=tags$b("Table"),
            width=12,
            fluidRow(
                column(
                    width=12,
                    tags$p(
                        "This table provides a pivoted overview of each country per year, including their overall score."
                    ),
                    tags$p(
                        "Noting the following:"
                    ),
                    tags$ul(
                        tags$li("Each line represents a different country"),
                        tags$li("Each year is included as a different country, reading from left to right, oldest to newest."),
                        tags$li("The overall improvement for each country is calculated as a percentage difference between the first column (2001), and the last column (2018). This score is included in the 'imprvement' column."),
                        tags$li("The data is then ordered by this 'improvement' column, showing the countries with the highest decrease at the top, and countries with the least amount of decrease in PoU at the bottom.")
                    ),
                    br()
                )
            ),
            fluidRow(
                column(
                    width=12,
                    DT::dataTableOutput(outputId="tbl_succ_TopCountries")
                )
            )
        )
    ),
    
    # . . Predictor Features ----
    fluidRow(
        box(
            title=tags$b("Predictor Fratures"),
            width=12,
            fluidRow(
                column(
                    width=6,
                    tags$p("Again looking at the scores for each country, the below plots allow for easy exploration of the data for each country."),
                    tags$p("Note that:"),
                    tags$ul(
                        tags$li("Each plot is a different feature of the original data. Which is effectively a different column of the original data."),
                        tags$li("Each plot shows the x-axis as time."),
                        tags$li("Each of the colours are simply to help easy differentiation between the variables."),
                        tags$li("All of the plots are only showing for one country, which can be changed using the drop down box.")
                    )
                ),
                column(
                    width=6,
                    selectizeInput(
                        inputId="rese_succ_inbx_SelectedCountries",
                        h4("Select Country"),
                        choices=FaoStat_wide %>% filter(cat_complete!="empty") %>% select(region,country) %>% distinct() %>% mutate(value=paste(region, country, sep=": ")) %>% select(value) %>% arrange(value) %>% pull(),
                        selected=c("Asia & Pacific: Viet Nam"),
                        multiple=FALSE
                    )
                )
            ),
            fluidRow(
                column(
                    width=12,
                    plotOutput(
                        outputId="plt_rese_succ_SingleCountry",
                        height="8in"
                    )
                )
            )
        )
    )
)


#------------------------------------------------------------------------------#
# . Surprising Trends                                                       ####
#------------------------------------------------------------------------------#

pag_ReseSurprisingTrends <- tabItem(
    
    # . . Name ----
    tabName="surprising_trends",
    
    # . . Header ----
    h1("Some Surprising Trends In The Data"),
    
    # . . Increasing Country Trends ----
    fluidRow(
        box(
            title=tags$b("Increasing Trends per Country"),
            width=12,
            column(
                width=4,
                tags$p("Reserved for comments.")
            ),
            column(
                width=8,
                plotlyOutput(
                    outputId="plt_surp_AllCountries",
                    height="4in"
                )
            )
        )
    ),
    
    # . . Arable Land ----
    fluidRow(
        box(
            title=tags$b("Arable Land per Country"),
            width=12,
            column(
                width=4,
                tags$p("Reserved for comments.")
            ),
            column(
                width=8,
                plotlyOutput(
                    outputId="plt_surp_ArableLand",
                    height="4in"
                )
            )
        )
    ),
    
    # . . Change in GDP ----
    fluidRow(
        box(
            title=tags$b("Change in GDP"),
            width=12,
            column(
                width=4,
                tags$p("Reserved for comments.")
            ),
            column(
                width=8,
                plotlyOutput(
                    outputId="plt_surp_ChangeInGDP",
                    height="4in"
                )
            )
        )
    )
    
)


#------------------------------------------------------------------------------#
# . Most Influential                                                        ####
#------------------------------------------------------------------------------#

pag_ReseMostInfluential <- tabItem(
    
    # . . Name ----
    tabName="most_influential",
    
    # . . Header ----
    h1("Most Influential Features"),
    
    # . . Comment and Plot ----
    fluidRow(
        box(
            title=tags$b("Overview"),
            width=12,
            column(
                width=4,
                tags$p("Reserved for comments.")
            ),
            column(
                width=8,
                plotOutput(
                    outputId="plt_infl_VariableImportance"
                )
            )
        )
    ),
    
    # . . Partial Dependancy Plots ----
    fluidRow(
        box(
            title=tags$b("Partial Dependancy Plots"),
            width=12,
            fluidRow(
                column(
                    width=12,
                    tags$p("Reserved for comments.")
                )
            ),
            fluidRow(
                column(
                    width=12,
                    plotOutput(
                        outputId="plt_infl_PartialDependancy",
                        height="12in"
                    )
                )
            )
        )
    )
    
)



#------------------------------------------------------------------------------#
#                                                                              #
#    Conclusion                                                             ####
#                                                                              #
#------------------------------------------------------------------------------#

# . . Conclusion ----
pag_Conclusion <- tabItem(
    
    # . . Name ----
    tabName="conclusion",
    
    # . . Header ----
    h1("Conclusion"),
    
    fluidRow(
        box(
            title=tags$b("Conclusion"),
            width=12,
            column(
                width=6,
                tags$p("Reserved for comments.")
            ),
            column(
                width=6,
                tags$p("More comments.")
            )
        )
    )
    
)


#------------------------------------------------------------------------------#
#                                                                              #
#    Finalise                                                               ####
#                                                                              #
#------------------------------------------------------------------------------#

# . . Pull together ----
body <- dashboardBody(
    
    # Define header tags
    tags$head(
        tagList(
            # useShinyjs(),
            # extendShinyjs("www/app.js", functions = c("updateHistory")),
            tags$link(
                rel="stylesheet",
                type="text/css",
                href="app.css"
            )
        )
    ),
    
    # Define page items
    tabItems(
        
        # Beginning
        pag_InfoPage,
        pag_DisclaimerPage,
        
        # Data Details
        pag_DictionaryPage,
        pag_Undernourishment,
        pag_StatFeatureInteractionsPage,
        pag_StatFeaturesPage,
        pag_StatTotalPage,
        
        # Research Questions
        pag_ReseGeneralTrend,
        pag_ReseMostSuccessful,
        pag_ReseSurprisingTrends,
        pag_ReseMostInfluential,
        
        # Conclusion
        pag_Conclusion
        
    )
)

# . . Set the UI ----
ui <- dashboardPagePlus(
    header,
    sidebar,
    body
)

