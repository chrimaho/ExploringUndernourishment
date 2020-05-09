#==============================================================================#
#                                                                              #
#    Title      : Shiny Functions                                              #
#    Purpose    : Declare the custom functions to be used in the Shiny App     #
#    Notes      : .                                                            #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : .                                                            #
#    Sources    : .                                                            #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#


#------------------------------------------------------------------------------#
# Programmatic Tools                                                        ####
#------------------------------------------------------------------------------#

str_Left <- function(string, num_chars) {
    #' @title Subset Left
    #' @description Subset the `string` argument to only include the left-most `num_chars` number of characters.
    #' Is similar to SQL and VB function LEFT().
    #' @param string character. The text string you want to select from; must be an character type.
    #' @param num_chars numeric. The number of characters that you want to select; must be an atomic numeric type.
    #' @return A text string of length 'num_chars' that corresponds to the left most number of characters from the 'string' option.
    
    # Validations
    assert_that(is.character(string))
    assert_that(is.numeric(num_chars))
    assert_that(length(num_chars)==1)
    
    # Do work
    return <- str_sub(string, end=num_chars)
    
    # Return
    return(return)
    
}


str_NotLeft <- function(string, num_chars) {
    #' @title Subset Not Left
    #' @description Subset the `string` argument excluding the left-most `num_chars` number of characters.
    #' Is similar to SQL and VB function LEFT().
    #' @param string character. The text string you want to select from; must be an character type.
    #' @param num_chars numeric. The number of characters that you want to select; must be an atomic numeric type.
    #' @return A text string of length 'num_chars' that corresponds to the left most number of characters from the 'string' option.
    
    # Validations
    assert_that(is.character(string))
    assert_that(is.numeric(num_chars))
    assert_that(length(num_chars)==1)
    
    # Do work
    return <- str_sub(string, start=num_chars-1)
    
    # Return
    return(return)
    
}


str_Mid <- function(string, start_num, num_chars) {
    #' @title Subset Left
    #' @description Subset the mid-point in a string, starting from a specified position and extends to a specified length.
    #' Is similar to SQL and VB function MID().
    #' @param string character. The text string you want to select from; must be an atopic string.
    #' @param start_num numeric. The starting position of the mid-text string you want to select from; must be an atomic numeric type.
    #' @param num_chars numeric. The number of characters that you want to select; must be an atomic numeric type.
    #' @return A text string of length 'num_chars' that corresponds to the characters from the 'start_num' starting position from the 'string' option.
    
    # Validations
    assert_that(is.character(string))
    assert_that(is.character(string)) 
    assert_that(is.numeric(start_num))
    assert_that(length(start_num)==1) 
    assert_that(is.numeric(num_chars))
    assert_that(length(num_chars)==1) 
    
    # Do work
    return <- str_sub(string, start_num, start_num + num_chars - 1)
    
    # Return
    return(return)
    
}


str_Right <- function(string, num_chars) {
    #' @title Subset Right
    #' @description Subset the `string` argument to only include the right-most `num_chars` number of characters.
    #' Is similar to SQL and VB function RIGHT().
    #' @param string character. The text string you want to select from; must be an character type.
    #' @param num_chars numeric. The number of characters that you want to select; must be an atomic numeric type.
    #' @return A text string of length 'num_chars' that corresponds to the right most number of characters from the 'string' option.
    
    # Validations
    assert_that(is.character(string))
    assert_that(is.numeric(num_chars))
    assert_that(length(num_chars)==1)
    
    # Do work
    return <- str_sub(string, start=-num_chars)
    
    # Return
    return(return)
    
}


str_NotRight <- function(string, num_chars) {
    #' @title Subset Not Right
    #' @description Subset the `string` argument excluting the right-most `num_chars` number of characters.
    #' Is similar to SQL and VB function RIGHT().
    #' @param string character. The text string you want to select from; must be an character type.
    #' @param num_chars numeric. The number of characters that you want to select; must be an atomic numeric type.
    #' @return A text string of length 'num_chars' that corresponds to the right most number of characters from the 'string' option.
    
    # Validations:
    assert_that(is.character(string))
    assert_that(is.numeric(num_chars))
    assert_that(length(num_chars)==1)
    
    # Do work
    return <- str_sub(string, end=-num_chars-1)
    
    # Return
    return(return)
    
}



#------------------------------------------------------------------------------#
# Object Details                                                            ####
#------------------------------------------------------------------------------#


get_PrintDataReturn <- function(DataFrame) {
    #' @title Print Then Return
    #' @description Print the `data.frame`, then return it. Best to use this in the middle of a `dplyr` pipe.
    #' @note Probably the easyiest, yet most useful function I've ever written.
    #' @param DataFrame data.frame. The `data.frame` you want printed.
    #' @return The original `data.frame`.
    
    # Validations ----
    assert_that(is.data.frame(DataFrame))
    
    # Do work ----
    print(DataFrame)
    
    # Return ----
    return(DataFrame)
}


get_PrintStatReturn <- function(DataFrame) {
    #' @title Print Stats Then Return
    #' @description Print dataframe statistics, then return the original dataframe.
    #' @note Best used within a dplyr pipe.
    #' @param DataFrame data.frame. The dataframe you want checked.
    #' @return The original dataframe.
    
    # Validations ----
    assert_that(is.data.frame(DataFrame))
    
    # Do work ----
    print(get_DataFrameStatistics(DataFrame))
    
    # Return ----
    return(DataFrame)
}


get_DataFrameStatistics <- function(DataFrame, p_val=0.95, signif=5) {
    #' @title Get `data.frame` Statistics.
    #' @description Get some key statistics from a `data.frame`.
    #' @note Requires the `e1071` and `gmodels` packages.
    #' @param DataFrame data.frame. The dataframe which will have the details generated from.
    #' @param p_val numeric. The P-Value from which to draw the confidence-interval from.
    #' @param signif numeric. The level of significant digits that the data should be rounded to.
    #' @return A `data.frame` containing the information about `DataFrame`.
    
    # Load packages:
    require(e1071)
    require(gmodels)
        
    # Assertions:
    assert_that(is.data.frame(DataFrame))
    assert_that("e1071" %in% .packages(), msg="'e1071' is not loaded.")
    assert_that("gmodels" %in% .packages(), msg="'gmodels' is not loaded.")
    assert_that(is.numeric(p_val))
    assert_that(between(p_val, 0, 1), msg="'p_val' must be between '0' and '1'.")
    assert_that(is.numeric(signif))
    assert_that(signif %% 1 == 0, msg="'signif' must be an integer.")
    
    # Do work:
    dat <- data.frame( length       = nrow(DataFrame)
                       , class        = sapply(DataFrame, function(x) class(x))
                       , type         = sapply(DataFrame, function(x) typeof(x))
                       , mode         = sapply(DataFrame, function(x) mode(x))
                       , num_distinct = sapply(DataFrame, function(x) sum(!is.na(unique(x))))
                       , pct_distinct = sapply(DataFrame, function(x) sum(!is.na(unique(x))) / nrow(DataFrame))
                       , num_na       = sapply(DataFrame, function(x) sum(is.na(x)))
                       , pct_na       = sapply(DataFrame, function(x) sum(is.na(x))/nrow(DataFrame))
                       , num_null     = sapply(DataFrame, function(x) sum(is.null(x)))
                       , pct_null     = sapply(DataFrame, function(x) sum(is.null(x))/nrow(DataFrame))
                       , mean         = sapply(DataFrame, function(x) mean(x, na.rm=T))
                       , std.dev      = sapply(DataFrame, function(x) sd(x, na.rm=T))
                       , ci_mean      = sapply(DataFrame, function(x) if(is.character(x) | is.logical(x)) NA else x %>% data.frame %>% na.omit %>% pull %>% ci(p_val) %>% .[c("CI lower", "CI upper")] %>% round(signif) %>% str_c(collapse=", ") %>% paste0("[",.,"]"))
                       , median       = sapply(DataFrame, function(x) median(x, na.rm=T)) %>% as.numeric()
                       , max          = sapply(DataFrame, function(x) max(x, na.rm=T) %>% as.numeric())
                       , min          = sapply(DataFrame, function(x) min(x, na.rm=T) %>% as.numeric())
                       , sum          = sapply(DataFrame, function(x) if(is.character(x)) NA else sum(x, na.rm=T))
                       , num_range    = sapply(DataFrame, function(x) max(x, na.rm=T) %>% as.numeric() - min(x, na.rm=T) %>% as.numeric())
                       , val_range    = sapply(DataFrame, function(x) range(x, na.rm=T) %>% str_c(collapse=", ") %>% paste0("[",.,"]"))
                       , skewness     = sapply(DataFrame, function(x) if(is.character(x)) NA else skewness(x, na.rm=T))
                       , kurtosis     = sapply(DataFrame, function(x) if(is.character(x)) NA else kurtosis(x, na.rm=T))
                       , norm_test    = sapply(DataFrame, function(x) if(length(x)>=5000 || is.character(x) || is.logical(x)) NA else x %>% data.frame %>% na.omit %>% pull %>% shapiro.test %>% extract2("statistic"))
    ) %>% 
        rownames_to_column("variable") %>% 
        mutate_at(c("pct_distinct","pct_na","mean","std.dev","skewness","kurtosis","norm_test"), round, signif)
    
    # Return:
    return (dat)
    
}



#------------------------------------------------------------------------------#
# Data Visualisation Tools                                                  ####
#------------------------------------------------------------------------------#


# Review Distribution function ----
RevDist <- function(dat, revwcols=NA, exclcols=NA, label=NA, bins=NA, groupby=NA){
    
    # Data types ----
    #dat=data.frame
    #revwcols=character vector
    #exclcols=character vector
    #label=character atomic
    
    # Check packages ----
    for (package in c("ragtop","dplyr","ggplot2","RColorBrewer")) {
        suppressPackageStartupMessages(
            require(package
                    ,character.only=TRUE)
        )
    }
    
    # Account for vectors ----
    if (is.vector(dat)) {
        dat <- dat[!is.na(dat)]
        dat <- data.frame(dat, stringsAsFactors=FALSE)
        revwcols <- names(dat)[!names(dat) %in% exclcols]
    }
    
    # Account for blank revwcols ----
    if (is.blank(revwcols)) {
        revwcols <- names(dat)[!names(dat) %in% exclcols & !names(dat) %in% label]
    }
    
    # Set loopnames ----
    loopnames <- revwcols[!revwcols %in% exclcols]
    
    # Run loop to loop through each column ----
    for (i in loopnames) {
        
        # Set groupby ----
        if (is.blank(groupby)) { 
            groupvar <- 1
        } else {
            groupvar <- dat[,groupby] %>% n_distinct()
            #colourCount = length(unique(mtcars$hp))
        }
        
        # Set Colour Palette ----
        ColourPalette <- c("#008000","#0000ff","#ff0000","#00ffff","#ff00ff","#ffff00")
        
        # If there is only one variable in the groupvar, then do the following #
        if (groupvar==1) {
            
            # Account for blank bins ----
            if(is.blank(bins)){
                bins <- length(unique(get(i)[!is.na(get(i))]))
                if(bins>30){bins<-30}
            }
            
            # Set Stats ----
            bw <- (max(dat[,i],na.rm=T) - min(dat[,i],na.rm=T))/(bins+1)
            avg <- mean(dat[,i],na.rm=T)
            std <- sd(dat[,i],na.rm=T)
            num <- length(dat[!is.na(dat[,i]),i])
            
            # Set initial plot ----
            h <- dat %>%
                ggplot(aes_string(i)) +
                geom_histogram(aes(y=..count..), fill=ColourPalette(1), color=ColourPalette(1), alpha=0.4, bins=bins) +
                stat_function(fun = function(x) dnorm(x, mean=avg, sd=std) * num * bw, color=ColourPalette(1), size=1.3) +
                labs(
                    title=paste0("Histogram of","\n",i)
                    ,subtitle=paste0("num=",round(num),"   ","avg=",round(avg,3),"   ","std=",round(std,3),"   ","normtest=",round(shapiro.test(dat[,i])$statistic,3))
                )
            
            # Resize bins ----
            if(bins<20){
                h <- h + scale_x_continuous(breaks=round(seq(min(dat),max(dat),by=1)))
            }
            
            # If there are multiple variables in the groupvar, then do the following ----
        } else if (groupvar>1) {
            
            # Exit the function if the groupby parameter is blank ----
            if(is.blank(label)){
                stop("When using the GroupBy parameter, the Label parameter cannot be blank")
            }
            
            # Set up the bins ----
            if(is.blank(bins)){
                bins <- dat[!is.na(dat[,label]),] %>% select(label) %>% distinct() %>% nrow()
                if(bins>30){bins<-30}
            }
            
            # Set up the plot ----
            h <- dat %>%
                ggplot(aes_string(label, fill=groupby, color=groupby)) + 
                labs(title=paste0("Histogram of","\n",i)) +
                geom_histogram(aes(y=..count..), alpha=0.2, bins=bins, position="identity")
            
            # Need to account for the fact there are only six colours in my palette... ----
            if (groupvar <= 6) {
                h <- h +
                    scale_fill_manual(values=head(ColourPalette,groupvar)) +
                    scale_color_manual(values=head(ColourPalette,groupvar))
            } else if (groupvar > 6) {
                ColourPalette <- colorRampPalette(ColourPalette)
                h <- h + 
                    scale_fill_manual(values=ColourPalette(groupvar)) +
                    scale_color_manual(values=ColourPalette(groupvar))
            }
            
            # Loop through each of the variables in the groupby clause ----
            jnum <- 0
            for (j in unique(dat[,groupby])) {
                
                # Set the stats #
                jnum <- jnum + 1
                biw <- (max(dat[dat[,groupby]==j,label],na.rm=T) - min(dat[dat[,groupby]==j,label],na.rm=T))/(bins+1)
                avg <- mean(dat[dat[,groupby]==j,label],na.rm=T)
                std <- sd(dat[dat[,groupby]==j,label],na.rm=T)
                num <- length(dat[!is.na(dat[dat[,groupby]==j,label]),label])
                
                # Because ggplot is lazy, need to set the normal curve as a text string, then parse it to the object ----
                txt <- paste0("stat_function(fun = function(x) dnorm(x, mean=",avg,", sd=",std,") * ",num," * ",biw,", color=ColourPalette[",jnum,"], size=1.3, linetype='longdash', show.legend=TRUE)")
                h <- h + eval(parse(text=txt))
                
            }
            
        }
        
        # Set theme ----
        h <- h +
            theme_bw() +
            theme(plot.title = element_text(hjust=0.5)) +
            theme(plot.subtitle = element_text(hjust=0.5)) +
            theme(panel.grid.major.x = element_blank())
        
        # Final Return ----
        return(h)
        
        # d <- dat %>%
        #     ggplot(aes_string(i,"evictions")) +
        #     geom_point(color="cornflowerblue",alpha=0.5) +
        #     geom_density_2d(color="blue") +
        #     geom_smooth(color="red") +
        #     ggtitle(paste("Dot plot of","\n",i,"\n","vs.","\n","evictions"))
        # 
        # grid.arrange(h,d,ncol=2)
        
    }
}

# Render Evals Ui ----
sub_RenderEvals <- function(output, show=FALSE) {
    
    # Use:
    # - Render the UI Options for the Evals section of the main page
    
    # Input: 
    # - 'show' : A logical string for whether or not to show or hide the panels on the page
    
    # Output:
    # - Nothing. This is an operational function, and does not return anything.
    # - Perhaps it would be good to return a logical value as returned from a TryCatch process...
    
    # Validations ----
    stopifnot(is.logical(show))
    
    # Set render height ----
    if (show==TRUE) {
        OverallHeight <- "400px"
        ComparisonHeight <- "600px"
        OutputHeight <- "1px"
    } else if (show==FALSE) {
        OverallHeight <- "1px"
        ComparisonHeight <- "1px"
        OutputHeight <- "1px"
    } else {
        stop("'show' should be a logical value with 'TRUE' or 'FALSE'")
    }
    
    # Display Evals Overall Row ----
    output$tag_Evals_UiOverallCharts <<- renderUI({
        fluidRow(
            id="row_Evals_OverallCharts"
            ,h2("Overall Charts")
            ,column(4, plotOutput(height=OverallHeight, outputId="plt_OverallRating"))
            ,column(4, plotOutput(height=OverallHeight, outputId="plt_OverallAddValue"))
            ,column(4, plotOutput(height=OverallHeight, outputId="plt_OverallLeadership"))
        )
    })
    
    # Display Evals Comparison Row ----
    output$tag_Evals_UiComparisonCharts <<- renderUI({
        fluidRow(
            id="row_Evals_ComparisonCharts"
            ,h2("Comparison Charts")
            ,column(6, plotOutput(height=ComparisonHeight, outputId="plt_PresenterComparison"))
            ,column(6, plotOutput(height=ComparisonHeight, outputId="plt_ActivityComparison"))
        )
    })
    
    # Display Evals Presentation Charts Row ----
    output$tag_Evals_UiPresentationCharts <<- renderUI({
        fluidRow(
            id="row_PresentationCharts"
            ,column(4, plotOutput(height=OutputHeight, outputId="plt_PresenterPerformance"))
            ,column(4, plotOutput(height=OutputHeight, outputId="plt_PresenterInteraction"))
            ,column(4, plotOutput(height=OutputHeight, outputId="plt_PresenterKeep"))
        )
    })
    
    # Display Evals Activity Charts Row ----
    output$tag_Evals_UiActivityCharts <<- renderUI({
        fluidRow(
            id="row_ActivityCharts"
            ,column(6, plotOutput(height=OutputHeight, outputId="plt_ActivityChallenge"))
            ,column(6, plotOutput(height=OutputHeight, outputId="plt_LeadershipOpportunity"))
        )
    })
    
    # Display Evals Dynamic Charts Row ----
    output$tag_Evals_UiDynamicCharts <<- renderUI({
        fluidRow(
            id="row_ActivityCharts"
            ,column(6, plotOutput(height=OutputHeight, outputId="plt_CspChart"))
            ,column(6, plotOutput(height=OutputHeight, outputId="plt_LicChart"))
        )
    })
    
    # Display Evals Comments Row ----
    output$tag_Evals_UiAdditionalComments <<- renderUI({
        fluidRow(
            id="row_Evals_AdditionalComments"
            ,h2("Additional Comments")
            ,tableOutput(outputId="tbl_AdditionalComments")
        )
    })
    
    # Return ----
    return(TRUE)
    
}

# Plot Leadership Score ----
plt_LeadershipScore <- function(Data, Variable= "") {
    
    # Input:
    # - 'Data' : A Data Frame
    # - 'Variable' : The name of a variable within 'Data' that is to be plotted
    
    # Output:
    # - A GGPlot object
    
    # Validations ----
    if(!is.data.frame(Data)){stop("'Data' must be a data.frame.")}
    if(!is.character(Variable)){stop("'Variable' must be a string.")}
    if(!is.atomic(Variable)){stop("'Variable' must be atomic.")}
    if(!Variable %in% names(Exit)){stop(paste0("'", Variable, "' is not a variable in 'Data'."))}
    
    # Generate data ----
    Data <- Data %>% 
        select(Variable) %>% 
        filter(!is.na(.)) %>% 
        rename_all(str_replace_all, "genr", "") %>% 
        rename_all(str_replace_all, "over", "") %>% 
        rename_all(str_replace_all, "_", "") %>% 
        rename_all(str_replace_all, "Rating", "Score") %>% 
        mutate_at(1, as.numeric)
    
    # Assign variables ----
    Var <- names(Data)[1]
    
    # Set Stats ----
    LeadershipScore_av <- Data %>% pull %>% mean(na.rm=T) %>% round(2)
    LeadershipScore_sd <- Data %>% pull %>% sd(na.rm=T) %>% round(2)
    bins <- Data %>% filter(!is.na(.[1])) %>% distinct %>% pull %>% length
    bw <- (Data %>% pull %>% max(na.rm=T) - Data %>% pull %>% min(na.rm=T)) / (bins+1)
    avg <- Data %>% pull %>% mean(na.rm=T)
    std <- Data %>% pull %>% sd(na.rm=T)
    num <- Data %>% filter(!is.na(.[1])) %>% pull %>% length
    
    # Factorise data ----
    Data %<>% mutate_at(Var, factor, levels=1:10)
    
    # Plot ----
    Plot <- Data %>% 
        ggplot(aes_string(Var, fill=Var)) +
        geom_bar(colour="darkgrey") +
        scale_fill_manual("legend", values=c("1"="#d73027", "2"="#d73027", "3"="#d73027", "4"="#d73027", "5"="#d73027"
                                             , "6"="#fc8d59", "7"="#fee08b", "8"="#d9ef8b", "9"="#91cf60", "10"="#1a9850"
        )
        ) +
        scale_x_discrete(drop=F) +
        stat_function(fun = function(x) dnorm(x, mean=avg, sd=std) * num * bw, color="#0000ff", size=1) +
        geom_text(stat="count", aes(label=..count.., vjust=-0.4)) +
        theme(legend.position="none"
              ,panel.grid.minor.x=element_blank()
        ) +
        labs(title="Overall Leadership Score"
             ,x="Score"
             ,y="Count"
             # ,subtitle= paste0("Average: '", over_LeadershipScore_av, "'    Variance: '", over_LeadershipScore_sd, "'")
        )
    
    # Return ----
    return(Plot)
    
}
