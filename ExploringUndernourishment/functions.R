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
    #' @author chrimaho
    
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
    #' @author chrimaho
    
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
    #' @author chrimaho
    
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
    #' @author chrimaho
    
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
    #' @author chrimaho
    
    # Validations:
    assert_that(is.character(string))
    assert_that(is.numeric(num_chars))
    assert_that(length(num_chars)==1)
    
    # Do work
    return <- str_sub(string, end=-num_chars-1)
    
    # Return
    return(return)
    
}


str_Format <- function(string, ...) {
    #' @title String Formatter
    #' @description Take an input string, and substitute in-string variables.
    #' @note This is similar to the Python `string.foramt()` method.
    #' @param string string. The string to be re-formatted. Note, each of the named arguments must be surrounded in curly brackets.
    #' @param ... variables. A list of named variables. Note, each of these arguments must align to the variables in curly brackets from the `string` argument. These will be combined in to a list.
    #' @return A formatted string
    #' @example str_Format("Sammy the {animal} {verb} a {noun}.", animal="shark", verb="made", noun="car")
    #' @references https://stackoverflow.com/questions/44763056/is-there-an-r-equivalent-of-pythons-string-format-function#answer-44763659
    #' @author chrimaho
    
    # Import packages ----
    require(stringr)
    
    # Validations ----
    assert_that(is.string(string))
    assert_that(c("stringr") %all_in% .packages(), msg="The packages 'stringr' must be mounted.")
    
    # Get arguments ----
    envir <- as.environment(list(...))
    parent.env(envir) <- .GlobalEnv
    
    # Perform substitution
    string <- str_replace_all(string, "\\{", "${")
    str_return <- str_interp(string=string, env=envir)
    
    # Return ----
    return(str_return)
    
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
    #' @author chrimaho
    
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
    #' @author chrimaho
    
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
    dat <- data.frame(length       = nrow(DataFrame)
                      ,class        = sapply(DataFrame, function(x) class(x))
                      ,type         = sapply(DataFrame, function(x) typeof(x))
                      ,mode         = sapply(DataFrame, function(x) mode(x))
                      ,num_distinct = sapply(DataFrame, function(x) sum(!is.na(unique(x))))
                      ,pct_distinct = sapply(DataFrame, function(x) sum(!is.na(unique(x))) / nrow(DataFrame))
                      ,num_na       = sapply(DataFrame, function(x) sum(is.na(x)))
                      ,pct_na       = sapply(DataFrame, function(x) sum(is.na(x))/nrow(DataFrame))
                      ,num_null     = sapply(DataFrame, function(x) sum(is.null(x)))
                      ,pct_null     = sapply(DataFrame, function(x) sum(is.null(x))/nrow(DataFrame))
                      ,mean         = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {mean(x, na.rm=T)}})
                      ,std.dev      = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {sd(x, na.rm=T)}})
                      ,ci_mean      = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {
                         x %>%
                             data.frame %>%
                             na.omit %>%
                             pull %>%
                             ci(p_val) %>%
                             .[c("CI lower", "CI upper")] %>%
                             round(signif) %>%
                             str_c(collapse=", ") %>%
                             paste0("[",.,"]")
                         }})
                      ,median       = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {median(x, na.rm=T) %>% as.numeric()}})
                      ,max          = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {max(x, na.rm=T) %>% as.numeric()}})
                      ,min          = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {min(x, na.rm=T) %>% as.numeric()}})
                      ,sum          = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {sum(x, na.rm=T)}})
                      ,num_range    = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {max(x, na.rm=T) %>% as.numeric() - min(x, na.rm=T) %>% as.numeric()}})
                      ,val_range    = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {range(x, na.rm=T) %>% str_c(collapse=", ") %>% paste0("[",.,"]")}})
                      ,skewness     = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {skewness(x, na.rm=T)}})
                      ,kurtosis     = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {kurtosis(x, na.rm=T)}})
                      ,norm_test    = sapply(DataFrame, function(x) {if (is.character(x) | is.logical(x) | is.factor(x)) {NA} else {
                         x %>%
                             data.frame %>%
                             na.omit %>%
                             pull %>%
                             shapiro.test %>%
                             extract2("statistic")
                         }})
    ) %>% 
        rownames_to_column("variable") %>% 
        mutate_at(c("pct_distinct","pct_na","mean","std.dev","skewness","kurtosis","norm_test"), round, signif)
    
    # Return:
    return (dat)
    
}



#------------------------------------------------------------------------------#
# Data Visualisation Tools                                                  ####
#------------------------------------------------------------------------------#


plt_hist_SingleFeature <- function(Feature, Name=NA, Bins=NA) {
    #' @title Add function title
    #' @description Add function description.
    #' @note Add a note for the developer.
    #' @param Feature data.frame or vector. The Feature to be visualised.
    #' @param Name character. The Name of the Feature, only used if `Feature` is a vector.
    #' @param ColourPallette vector. A vector of colours to be used.
    #' @param Bins numeric. Number of bins to be used in the histogram.
    #' @return What is being returned?
    #' @author chrimaho
    
    # Validations ----
    assert_that(is.data.frame(Feature) | is.vector(Feature))
    assert_that(is.string(Name) | is.na(Name))
    assert_that(is.number(Bins) | is.na(Bins))
    if (is.data.frame(Feature)) {
        assert_that(ncol(Feature)==1, msg="'Feature' must be a single-column data frame.")
        Name <- names(Feature)
    }
    if (is.vector(Feature)) {
        assert_that(!is.na(Name), msg="If 'Feature' is a vector, 'Name' must contain the name of the Feature.")
        Feature <- data.frame(Name = Feature)
    }
    
    # Check ----
    if (is.na(Bins)) {
        Bins <- Feature %>% extract(!is.na(.)) %>% unique %>% length
        if (Bins>30) {Bins <- 30}
    }
    
    # Clean ----
    Feature <- Feature %>% filter(!is.na(.))
    
    # Set Stats ----
    binwid <- (max(Feature) - min(Feature)) / (Bins+1)
    avg <- Feature %>% extract2(1) %>% mean
    std <- Feature %>% extract2(1) %>% sd
    num <- Feature %>% extract2(1) %>% length
    
    # Set initial plot ----
    plt <- Feature %>%
        ggplot(aes_string(Name)) +
        geom_histogram(aes(y=..count..), fill="cornflowerblue", color="cornflowerblue", alpha=0.4, bins=Bins) +
        stat_function(fun = function(x) dnorm(x, mean=avg, sd=std) * num * binwid, color="cornflowerblue", size=1.3) +
        labs(title=paste0("Histogram of", "\n", "'", Name, "'")
            ,subtitle=paste0("num=", round(num)
                            ,"   "
                            ,"avg=", round(avg,3)
                            ,"   "
                            ,"std=", round(std,3)
                            ,"   "
                            ,"normtest=", Feature %>% extract2(1) %>% shapiro.test() %>% extract2("statistic") %>% round(3)
                            )
        )
    
    # Resize bins ----
    if(Bins<30){
        plt <- plt + scale_x_continuous(breaks=round(seq(min(Feature),max(Feature),by=1)))
    }
    
    # Set theme ----
    plt <- plt +
        theme_bw() +
        theme(plot.title = element_text(hjust=0.5)) +
        theme(plot.subtitle = element_text(hjust=0.5)) +
        theme(panel.grid.major.x = element_blank())
    
    # Return ----
    return(plt)
    
}


plt_grob_MultipleHistograms <- function(DataFrame, ExcludeFeatures=NA) {
    #' @title Add function title
    #' @description Add function description.
    #' @note Add a note for the developer.
    #' @param Input1Name Input1Type. What is Input1?
    #' @param Input2Name Input2Type. What is input2?
    #' @return What is being returned?
    #' @author chrimaho
    
    # Validations ----
    assert_that(is.data.frame(DataFrame))
    if (!is.na(ExcludeFeatures)) {
        assert_that(is.character(ExcludeFeatures))
    }
    
    # Set Up ----
    iter <- 0
    objs <<- list()
    
    # Do work ----
    for (feature in DataFrame %>% names){
        if (feature %in% ExcludeFeatures) next
        iter <- iter + 1
        plot <- DataFrame[feature] %>% plt_hist_SingleFeature()
        assign("temp", 
            DataFrame[feature] %>% plt_hist_SingleFeature()
        )
        objs[[iter]] <<- temp
    }
    
    # Create Grob ----
    grob <- arrangeGrob(grobs=objs, ncol=4)
    
    # Create Plot ----
    plot <- grob %>% as_ggplot()
    
    # Return ----
    return(plot)
    
}


plt_dot_DualFeature <- function(DataFrame, Target=names(DataFrame)[1], Feature=names(DataFrame)[2], GroupBy=NA, Smooth=FALSE) {
    #' @title Add function title
    #' @description Add function description.
    #' @note Add a note for the developer.
    #' @param DataFrame data.frame. The Table
    #' @param Target string. Name
    #' @param Feature string. Name
    #' @return What is being returned?
    #' @author chrimaho
    
    # Validations ----
    assert_that(is.data.frame(DataFrame))
    assert_that(is.string(Target))
    assert_that(is.string(Feature))
    assert_that(c(Target, Feature) %all_in% names(DataFrame), msg=paste0("The values for 'Target' and 'Feature (which are ", Target, "' and '", Feature, "', respectively), must be valid column names in 'DataFrame'."))
    if (!is.na(GroupBy)) {
        assert_that(c(GroupBy) %in% names(DataFrame), msg=paste0("The value for 'GroupBy' (which is '", GroupBy, "') must be a valid column name in 'DataFrame'."))
        assert_that(isFALSE(Smooth), msg="If you define a 'GroupBy' variable, it is illogical to include a smooth line.")
    }
    
    # Set Up ----
    plt <- DataFrame %>% 
        ggplot(aes_string(Feature, Target))
    
    # Add Grouping ----
    if (is.na(GroupBy)) {
        plt <- plt + 
            geom_point()
    } else {
        plt <- plt + 
            geom_point(aes_string(colour=GroupBy), alpha=0.5) +
            theme(legend.position="none")
    }
    
    # Add Smooth ----
    if (isTRUE(Smooth)) {
        plt <- plt +
            geom_smooth(colour="blue", fill="cornflowerblue")
    }
    
    # Add Labels ----
    plt <- plt +
        labs(
            title=paste0("Dot Plot"),
            subtitle=paste0("'", Target, "' by '", Feature, "'")
        )
    if (!is.na(GroupBy)) {
        plt <- plt +
            labs(
                caption=paste0("With a colour grouping by '", GroupBy, "'")
            )
    }
    
    # Return ----
    return(plt)
}


plt_comb_FeatureAndTarget <- function(DataFrame, Target=NA, Feature=NA, GroupBy=NA) {
    #' @title Add function title
    #' @description Add function description.
    #' @note Add a note for the developer.
    #' @param DataFrame Input1Type. What is Input1?
    #' @param Target Input2Type. What is input2?
    #' @return What is being returned?
    #' @author chrimaho
    
    # Validations ----
    assert_that(is.data.frame(DataFrame))
    assert_that(is.string(Target) | is.na(Target))
    assert_that(is.string(Feature) | is.na(Feature))
    assert_that(is.string(GroupBy) | is.na(GroupBy))
    assert_that(c(Target, Feature) %all_in% names(DataFrame), msg=paste0("The values for 'Target' and 'Feature (which are ", Target, "' and '", Feature, "', respectively), must be valid column names in 'DataFrame'."))
    if (!is.na(GroupBy)) {
        assert_that(c(GroupBy) %in% names(DataFrame), msg=paste0("The value for 'GroupBy' (which is '", GroupBy, "') must be a valid column name in 'DataFrame'."))
    }
    
    # Create Distribution ----
    hist <- plt_hist_SingleFeature(DataFrame[Feature])
    
    # Create comparison ----
    comb <- plt_dot_DualFeature(DataFrame, Target, Feature, GroupBy)
    
    # Combine ----
    plot <- arrangeGrob(hist, comb, nrow=1)
    
    # Return ----
    return(plot)
}


plt_comb_MultiFeaturesMultiPlots <- function(DataFrame, Countries, x_Feature, y_Feature) {
    #' @title Plot Multiple Features on Multiple Plots
    #' @description Uses three plots: Density, Point and Violin. Can only input two features at a time (x & y dimensions).
    #' @note Awesome and convenient plotting skillz.
    #' @param DataFrame data.frame. The data frame to be checked. Make sure it's in Wiiiide format.
    #' @param Countries character vector. The countris to be compared
    #' @param x_Feature string. The feature to be plot on the x-axis.
    #' @param y_Feature string. The feature to be plot on the y-axis.
    #' @return A ggplot object containing all the relevant info.
    #' @reference https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
    #' @author chrimaho
    
    # Validations ----
    assert_that(is.data.frame(DataFrame))
    assert_that("country" %in% names(DataFrame), msg="'DataFrame' must contain one feature called 'country'.")
    assert_that(is.character(Countries))
    assert_that(is.string(x_Feature))
    assert_that(is.string(y_Feature))
    assert_that(length(Countries)<6, msg="Please limit to max 5 countries.")
    assert_that(x_Feature %in% names(DataFrame), msg=paste0("The feature '", x_Feature, "' must be a feature of 'DataFrame'."))
    assert_that(y_Feature %in% names(DataFrame), msg=paste0("The feature '", y_Feature, "' must be a feature of 'DataFrame'."))
    assert_that(Countries %all_in% unique(FaoStat_wide[, "country", drop=T]), msg="All of the countries provided in 'Countries' must be legitimate countries, as provided in DataFrame['country'].")
    
    # Prep
    data <- DataFrame %>% 
        filter(country %in% sel_Countries) %>% 
        select(country, x_Feature, y_Feature)
    
    # Histogram
    hist <- data %>%
        select(country, x_Feature) %>% 
        na.omit() %>% 
        ggplot(aes_string(x=x_Feature, colour="country", fill="country")) +
        geom_density(aes(y=..count..*10), alpha=0.2) +
        theme(
            legend.position="None",
            axis.title.x=element_blank()
        ) +
        labs(
            y="Count"
        )
    
    # Dot
    dots <- data %>% 
        ggplot(aes_string(x=x_Feature, y=y_Feature, colour="country")) +
        geom_point(size=1, alpha=0.5) +
        theme(
            legend.position="None"
        ) +
        labs(
            x=x_Feature %>% str_replace_all("_", " ") %>% str_to_title(),
            y=y_Feature %>% str_replace_all("_", " ") %>% str_to_title()
        )
    
    # Violin
    viol <- data %>% 
        select(country, y_Feature) %>% 
        na.omit() %>% 
        ggplot(aes_string(x="country", y=y_Feature, colour="country", fill="country")) +
        geom_violin(alpha=0.2) +
        theme(
            axis.title.y=element_blank()
        ) +
        labs(
            fill="Country",
            colour="Country",
            x="Country"
        )
    
    # Check
    print(hist)
    print(dots)
    print(viol)
    
    # Create
    plt_Return <- arrangeGrob(
        hist, dots, viol, 
        layout_matrix=rbind(
            c(1,1,NA),
            c(2,2,3),
            c(2,2,3)
        ),
        top="Title",
        heights=c(1,2,2)
    )
    
    # Fix
    plt_Return %<>% as_ggplot()
    
    # Return ----
    return(plt_Return)
    
}


donut_percentage <- function(value, labels, title){
    ######################################################################################################
    # https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
    # Create test data.
    data <- data.frame(
        category=labels,
        count=c(value, (100 - value))
    )
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, "\n value: ", data$count, '%')
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
        scale_fill_brewer(palette=4) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none") +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5))
}

plot_confusion <- function(confusion, title){
    ########################################################################################
    # https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot #
    # Expects a confusion matrix produced by caret::confusionMatrix                        #
    ########################################################################################
    table <- as.data.frame(confusion$table)
    plotTable <- table %>%
        mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
        group_by(Reference) %>%
        mutate(prop = Freq/sum(Freq))
    
    ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
        geom_tile() +
        geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
        scale_fill_manual(values = c(good = "green", bad = "red")) +
        theme_bw() +
        xlim(rev(levels(table$Reference))) +
        ggtitle(title)
}
