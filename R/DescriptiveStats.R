#'
#' Automatically Calculate a wide variety of Statistics for any Dataset (tibble). When called make sure to assign this function's output to a variable, i.e.: MyDFStats <- DescriptiveStats(MyDF, TRUE)
#'
#' DESCRIPTIVES:
#' -Numerical Descriptives as a Matrix of Variables' Name, Min, Q1, Mean, Median, Q3, Max, St.Dev., IQR, Observations, NAs
#' -Categorical Descriptives as a Matrix of Observations, NAs, Number of Unique values, and some of those values followed by their percentage of occurrance
#' DESCRIPTIVE PLOTS:
#' -Categorical Distributions as Bar charts
#' -Numerical Distributions as Boxplots with dots overlaid to show actual concentration; dots' colour can optionally be used to display a 2nd numerical dimension (another variable)
#' -Numerical Distributions also as Histograms with a Density plot overlaid and the mean value plotted as a vertical red dotted line
#' CORRELATIONS:
#' -Correlation Matrix (Pearson and Spearman) where Columns can be optionally reordered by Clustering techniques like PCA
#' -Correlation p-values Matrix (Pearson and Spearman) to show is the aforementioned correlation is statistically significant at a user-defined significance level
#' -Correlation Plots with Red colour shows strong positive correlation, all the way to Blue showing strong negative correlation, with non-statistically-significant correlations being crossed out by an 'X'
#' STATISTICAL INFERENCE: Plotting the Categorical variables VS the Dependent Variable
#' If the Dependent variable is Numerical
#' -Plotting the Categorical variables VS the Dependent as Boxplots per the different levels of the Categorical Variable, optionally overlaying a 3rd dimension as coloured dots on the boxplots
#' -Plotting the Numerical variables vs the Dependent as a scatter plot, optionally overlaying a 3rd dimension as colour for the scatterplot's dots
#' IF the Dependent variable is Categorical
#' -Plotting the Categorical variables VS the Dependent as Stacked Bar plots, 1 bar per level of the Dependent variable and 1 colour per level of the Independent one
#' -Plotting the Numerical variables vs the Dependent as Boxplots per the different levels of the Dependent variable, optionally overlaying a 3rd dimension as coloured dots on the boxplots
#' TIMESERIES:
#' -Timeseries visualisation as a scatterplot with a Line chart overlaid where X-axis is time and Y-Axis is the variable's time, optionally overlaying a 3rd dimension as coloured dots on the scatterplot
#' PER GROUP ANALYSIS:
#' -Numerical Variables VS the Group's levels plotted as differently coloured Boxplots
#' -Histograms with Density plots overlaid, juxtaposed per Group's levels as well
#' -Categorical variables VS the group's levels displayed as a Stacked Bat Chart
#' If the Dependent variable is Numerical
#' -Categorical VS Dependent Per Group displayed as 1 plot per Group's level, with each plot being juxtaposed Boxplots of the Dependent variable's distribution for each Categorical Independent variable's level
#' -Numerical VS Dependent Per Group displayed as 1 plot per Group's level, with each plot being a scatterplot of the Dependent variable VS the Numerical Independent variable
#' If the Dependent variable is Categorical
#' -Categorical VS Dependent Per Group displayed as 1 plot per Group's level, with each plot being a Stacked Barchart with 1 bar per level of the Dependent variable and 1 colour per level of the Independent one
#' -Numerical VS Dependent PEr Group displayed as 1 plot per Group's level,  with each plot being juxtaposed Boxplots 1 bar per level of the Dependent variable
#' And lastly, there's a Per-Group folder where everything talked so far is done again in a recursive manner for rows corresponding to each group's level, and removing the Group variable
#'
#' @param VarDF A Tibble (data.frame) This is the Dataset to be analysed. Ensure that Categorical variables are set as factor(), texts as character(), Integers as integer(), Booleans as logical()
#' @param CalculateGraphs Boolean. If FALSE then only Descriptives and Pearson Correlations are calculated
#' @param IncludeInteger Boolean. If FALSE then no statistics will occur for Integer variables
#' @param RoundAt Integer. Descriptive Statistics values like Min, Max, etc. will be rounded to this decimal place
#' @param AbbrevStrLevelsAfterNcount Integer. String and Factor unique values (levels) will be displayed up to this number, i.e. 'Colours: Red (3\%), Cyan (0.8\%), ..., Black (8\%)' for a value of 3
#' @param AllHistsOn1Page Boolean. If ShowGraphs==TRUE, then if TRUE, instead of creating a new plot per histogram, just 1 plot will be created, encompassing all histograms juxtaposed inside
#' @param AllBoxplotsOn1Page Boolean. Instead of creating 1 plot per variable, all of them are inside 1 plot. However, when there are differences in the value range, the low-range plots are practically invisible
#' @param AllBarChartsOn1Page Boolean. If ShowGraphs==TRUE, then if TRUE, instead of creating a new plot per Boxplot, just 1 plot will be created, encompassing all histograms juxtaposed inside
#' @param DependentVar String. If there is a variable to be considered as Target/Dependent, then mention its name here
#' @param ShowGraphs Boolean. If TRUE, Certain graphs will be displayed at the time of calculation before you can access them via the variable
#' @param BoxplotPointsColourVar String or Numeric Vector. Either a string indicating the Variable name inside VarDF, or a numerical vector to be used as colour
#' @param NoPrints Boolean. If TRUE, not even the Descriptive Statistics Matrices will be displayed on the time of the calculation. Everything will be accessible from the variable this function's output is assigned to
#' @param IsTimeSeries Boolean. If FALSE then no Timeseries specific statistics or plots will be calculated as it would show nonsense if the dataset is not really a time-series
#' @param GroupBy String. If you want to get statistics per group in addition to the general ones, then mention which Column of VarDF should be used as the grouping variable
#' @param TimeFlowVar String or Date/Numeric Vector. The variable name to be used as X-Axis on TimeFlow plots. The Variable corresponding to this string can be Date or Numeric
#' @param CorrVarOrder String. AB - Alphabetical, hclust - Order based on Hierarchical cluster analysis, BEA - Bond Energy Algorithm to maximize the measure of effectiveness (ME), PCA - First principal component or angle on the projection on the first two principal components, TSP - Travelling sales person solver to maximize ME
#' @param TimeseriesMaxLag Integer. Max lag for Auto Correlation/Covariance plots.
#' @param BoxPlotPointSize Numeric. How big or small you want the dots on the Boxplots to be. Usually a value between 0.1 and 1. The more the rows, the less the value here
#' @param BoxPlotPointAlpha Numeric. How transparent you want the dots on the Boxplots to be. Usually a value between 0.1 and 1. The more the rows, the less the value here
#' @param SampleIfNRowGT Integer. How many rows to keep for the plots. The more the rows, the greater the time it takes to plot everything. ggplot is not optimised for big data, so subsample for plots
#' @param SeedForSampling Integer. Doesn't really matter as subsampling is only for plots, but you can set a seed for the subsampling procedure
#' @param CalcPValues Boolean. Whether or not to calculate (and show on plots) the p-values for Pearson and Spearman correlations
#' @param SignificanceLevel Numeric. The Significance Level to be used for the p-values for Pearson and Spearman correlations
#' @param DatesToNowMinusDate Boolean. If TRUE then Dates are transformed into integers reflecting how many seconds have passed since the time on the date
#' @param DatesToCyclicMonth Boolean. If TRUE then Dates are transformed into numeric variables containing the cyclic sin and cos of the Month
#' @param DatesToCyclicDayOfWeek Boolean. If TRUE then Dates are transformed into numeric variables containing the cyclic sin and cos of the Day-of-week
#' @param DatesToCyclicDayOfMonth Boolean. If TRUE then Dates are transformed into numeric variables containing the cyclic sin and cos of the Day-of-month
#' @param DatesToCyclicDayOfYear Boolean. If TRUE then Dates are transformed into numeric variables containing the cyclic sin and cos of the Day-of-year
#' @param DatesToYearCat Boolean. If TRUE then Dates are transformed into categorical variables containing the Year of the date
#' @param DatesToMonthCat Boolean. If TRUE then Dates are transformed into categorical variables containing the Month of the date
#' @param DatesToDayOfWeekCat Boolean. If TRUE then Dates are transformed into categorical variables containing the Day-of-week of the date
#' @param DatesToDayOfMonthCat Boolean. If TRUE then Dates are transformed into categorical variables containing the Day-of-month of the date
#' @param DatesToDayOfYearCat Boolean. If TRUE then Dates are transformed into categorical variables containing the Day-of-Year of the date
#' @param DatesToHourCat Boolean. If TRUE then Dates are transformed into categorical variables containing the Hour of the date
#' @param DatesToMinuteCat Boolean. If TRUE then Dates are transformed into categorical variables containing the Minute of the date
#' @param ExcludeTaperedAutocor Boolean. Only counts if IsTimeSeries==TRUE. If TRUE then the TaperedAutocorrelation and TaperedPartialAutocorrelation will not be computed
#' @param MaxTaperedRows Integer. Probably a good idea to not increase it as the time it takes is excessive then
#' @param Verbose Numeric. If there are many columns, calculations can take a long time so we might wanna know when each part finishes and perhaps disable some parts
#' @param VarsToExcludeFromTimeseries String Array. The names of the variables which we don't want to include in Time-series analysis (if any)
#' @param ExludeCovariances Boolean. If TRUE, Cross-Covariance and Auto-Covariance will not be calculated.
#' @keywords Descriptive Statistics DescriptiveStats DescrStats
#' @export
#' @examples
#' #Loading the famous mtcars dataset
#' library(dplyr)
#' DS <- mtcars %>% mutate(vs = as.factor(vs), am = as.factor(am), gear = as.factor(gear), carb = as.factor(carb)) %>% as_tibble()
#'
#' #Seeing and understanding the Dataset
#' print(DS)
#'
#' #Creating the Variable which holds all the Matrices and Plots
#' MTCarsStats <- DS %>% DescriptiveStats(CalculateGraphs = TRUE, DependentVar = "mpg", IsTimeSeries = TRUE, GroupBy = "gear", CorrVarOrder = "PCA")
DescriptiveStats <- function(VarDF, CalculateGraphs, IncludeInteger = TRUE, RoundAt = 2, AbbrevStrLevelsAfterNcount = 5,
                             AllHistsOn1Page = TRUE, AllBoxplotsOn1Page = FALSE, AllBarChartsOn1Page = TRUE, DependentVar = NULL, ShowGraphs = FALSE, BoxplotPointsColourVar = NULL,
                             NoPrints = FALSE, IsTimeSeries = FALSE, GroupBy = NULL, TimeFlowVar = NULL,
                             BoxPlotPointSize = 0.4, BoxPlotPointAlpha = 0.1, SampleIfNRowGT = 10000, SeedForSampling = NULL,
                             CalcPValues = TRUE, SignificanceLevel = 0.01, CorrVarOrder = "PCA", TimeseriesMaxLag = NULL, DatesToNowMinusDate = FALSE,
                             DatesToCyclicMonth = FALSE, DatesToCyclicDayOfWeek = FALSE, DatesToCyclicDayOfMonth = FALSE, DatesToCyclicDayOfYear = FALSE,
                             DatesToYearCat = FALSE, DatesToMonthCat = FALSE, DatesToDayCat = FALSE, DatesToDayOfWeekCat = FALSE, DatesToDayOfMonthCat = FALSE,
                             DatesToDayOfYearCat = FALSE, DatesToHourCat = FALSE, DatesToMinuteCat = FALSE,
                             ExcludeTaperedAutocor = FALSE, MaxTaperedRows = 250, VarsToExcludeFromTimeseries = NULL, ExludeCovariances = TRUE,
                             Verbose = NULL) {
  #VarDF <- tibble(a = runif(100), b = rnorm(100), c = rhyper(100, 50, 40, 20), d = if_else(runif(100) < 0.5, "Less", "More"), e = if_else(rnorm(100) < 0.5, "Low", "High"), f = 5)

  #TODO Scatterplot of Timeseries variables at t VS t-lag
  #TODO GroupBy should be able to be a vector and so Per Group statistics should be calculated for each Group in GroupBy()
  #TODO Create a Variance/Covariance Matrix / Plot
  #TODO When Dependent variable is Ordered Factor or Logical, perhaps the "VS Dependent" plots should be done for both the Numerical version of it and the actual Categorical version
  #TODO When saving folders like "Boxplot Graphs Per Group" it should say "Per {VarName}"
  #TODO Another set of Correlations should be produced that include One-Hot-Encoded Factor Variables
  #TODO For time-series: Correlation with lag-variables (lag variables can be a vector for more than 1) both as matrices and as scatterplots
  #TODO Variance/Covariance matrix for numeric (ordinal, logical) variables
  #TODO Categorical Descriptives: Mode
  #TODO Choose Upper or Lower Triangle for Correlations

  #Making sure the names of the Dataframe as valid (no duplicates and only valid characters inside)
  if (is.not.null(DependentVar)) DependentVarIdx <- (names(VarDF) == DependentVar) %>% {ifelse(sum(.) > 0, which.max(.), numeric(0))} else DependentVarIdx <- NULL
  if (is.not.null(BoxplotPointsColourVar) && (NROW(BoxplotPointsColourVar) == 1 && NCOL(BoxplotPointsColourVar) == 1 && typeof(BoxplotPointsColourVar) == "character")) BoxplotPointsColourVarIdx <- (names(VarDF) == BoxplotPointsColourVar) %>% {ifelse(sum(.) > 0, which.max(.), numeric(0))} else BoxplotPointsColourVarIdx <- NULL
  if (is.not.null(GroupBy)) GroupByIdx <- (names(VarDF) == GroupBy) %>% {ifelse(sum(.) > 0, which.max(.), numeric(0))} else GroupByIdx <- NULL
  if (is.not.null(TimeFlowVar) && (NROW(TimeFlowVar) == 1 && NCOL(TimeFlowVar) == 1 && typeof(TimeFlowVar) == "character" && class(TimeFlowVar) == "character")) TimeFlowVarIdx <- (names(VarDF) == TimeFlowVar) %>% {ifelse(sum(.) > 0, which.max(.), numeric(0))} else TimeFlowVarIdx <- NULL
  names(VarDF) <- make.names(names(VarDF), unique = TRUE)
  if (is.not.null(DependentVar) && is.not.na(DependentVarIdx)) DependentVar <- names(VarDF)[DependentVarIdx]
  if (is.not.null(BoxplotPointsColourVar) && is.not.na(BoxplotPointsColourVarIdx) && (NROW(BoxplotPointsColourVar) == 1 && NCOL(BoxplotPointsColourVar) == 1 && typeof(BoxplotPointsColourVar) == "character")) BoxplotPointsColourVar <- names(VarDF)[BoxplotPointsColourVarIdx]
  if (is.not.null(GroupBy) && is.not.na(GroupByIdx)) GroupBy <- names(VarDF)[GroupByIdx]
  if (is.not.null(TimeFlowVar) && is.not.na(TimeFlowVarIdx) && (NROW(TimeFlowVar) == 1 && NCOL(TimeFlowVar) == 1 && typeof(TimeFlowVar) == "character" && class(TimeFlowVar) == "character")) TimeFlowVar <- names(VarDF)[TimeFlowVarIdx]

  if (is.not.null(GroupBy) && !all(GroupBy %in% (VarDF %>% select_if(function(x) is.factor(x) | is.logical(x)) %>% names()))) {
    warning(paste0("GroupBy Variable [", GroupBy, "] is not a factor or logical/boolean variable on the dataset"))
    GroupBy <- NULL
  }
  if (is.null(Verbose)) Verbose <- NCOL(VarDF) > 20 #If there are many columns, calculations can take a long time so we might wanna know when each part finishes and perhaps disable some parts
  if (is.null(CalcPValues)) CalcPValues <- NCOL(VarDF) <= 20
  if (is.not.null(TimeFlowVar) && (NROW(TimeFlowVar) == 1 && NCOL(TimeFlowVar) == 1 && typeof(TimeFlowVar) == "character" && class(TimeFlowVar) == "character")) {
    TimeFlowVarName <- TimeFlowVar
    TimeFlowVar <- VarDF[[TimeFlowVar]] #If it's not a string, then we've been given the variable itself, so nothing else to do.
  } else {
    TimeFlowVarName <- NULL #If TimeFlowVar is the variable itself, then it won't be added on plots, but if it it's a string, we don't need the Autocorrelation of it, etc.
  }

  if (is.not.null(BoxplotPointsColourVar)) {
    if (NROW(BoxplotPointsColourVar) == 1 && NCOL(BoxplotPointsColourVar) == 1 && typeof(BoxplotPointsColourVar) == "character") {
      ColourVarName <- BoxplotPointsColourVar
      BoxplotPointsColourVar <- VarDF[[ColourVarName]]
    } else if (NCOL(BoxplotPointsColourVar) == 1 && NROW(names(BoxplotPointsColourVar)) > 0) {
      ColourVarName <- names(BoxplotPointsColourVar)[[1]]
      ColourVarClass <- class(BoxplotPointsColourVar)
      BoxplotPointsColourVar <- as.vector(BoxplotPointsColourVar) %>% set_class(ColourVarClass)
    } else {
      cat("BoxplotPointsColourVar wasn't the name of the column on the data and it wasn't a named vector either so name is irretrievable!\n")
      ColourVarName <- ""
    }
  }


  TimeseriesMaxLag <- ifelse(is.null(TimeseriesMaxLag), min(NROW(VarDF), 30), TimeseriesMaxLag)

  PerGroupDescrStats <- NULL
  if (is.not.null(GroupBy)) {
    GroupByGroups <- VarDF %>% pull(!!GroupBy) %>% as.factor() %>% unique() %>% {as.character(.)[as.character(.) %in% levels(.)]} #TODO! Changes the order of ordered factors (is.ordered = TRUE)

    CurGroupFilterIndx <- NULL
    for (CurGroup in GroupByGroups) {
      CurGroupFilterIndx[[CurGroup]] <- VarDF[[GroupBy]] == CurGroup
    }

    PerGroupDescrStats <-
      lapply(GroupByGroups, function(CurGroup) {
        print(paste0("CurGroup: ", CurGroup))
        CurPerGroupDescrStats <-
          DescriptiveStats(VarDF = VarDF %>% filter(CurGroupFilterIndx[[CurGroup]]) %>% select(-c(!!sym(GroupBy))),
                             CalculateGraphs = CalculateGraphs, IncludeInteger = IncludeInteger, RoundAt = RoundAt, AbbrevStrLevelsAfterNcount = AbbrevStrLevelsAfterNcount,
                             AllHistsOn1Page = AllHistsOn1Page, AllBoxplotsOn1Page = AllBoxplotsOn1Page, AllBarChartsOn1Page = AllBarChartsOn1Page, DependentVar = DependentVar,
                           ShowGraphs = FALSE,
                           BoxplotPointsColourVar = if(is.not.null(BoxplotPointsColourVar)) BoxplotPointsColourVar[CurGroupFilterIndx[[CurGroup]]] %>% setNames(ColourVarName) else NULL,
                           NoPrints = TRUE,
                             IsTimeSeries = IsTimeSeries,
                           GroupBy = NULL,
                           TimeFlowVar = if (is.null(TimeFlowVarName)) TimeFlowVar[CurGroupFilterIndx[[CurGroup]]] else TimeFlowVarName,
                             BoxPlotPointSize = BoxPlotPointSize, BoxPlotPointAlpha = BoxPlotPointAlpha, SampleIfNRowGT = SampleIfNRowGT, SeedForSampling = SeedForSampling,
                             CalcPValues = CalcPValues, SignificanceLevel = SignificanceLevel, CorrVarOrder = CorrVarOrder, TimeseriesMaxLag = TimeseriesMaxLag,DatesToNowMinusDate = FALSE,
                           DatesToCyclicMonth = FALSE, DatesToCyclicDayOfWeek = FALSE, DatesToCyclicDayOfMonth = FALSE, DatesToCyclicDayOfYear = DatesToCyclicDayOfYear,
                           DatesToYearCat = DatesToYearCat, DatesToMonthCat = DatesToMonthCat, DatesToDayCat = DatesToDayCat, DatesToDayOfWeekCat = DatesToDayOfWeekCat,
                             DatesToDayOfMonthCat = DatesToDayOfMonthCat,
                           DatesToDayOfYearCat = DatesToDayOfYearCat, DatesToHourCat = DatesToHourCat, DatesToMinuteCat = DatesToMinuteCat,
                           ExcludeTaperedAutocor = ExcludeTaperedAutocor, MaxTaperedRows = MaxTaperedRows, VarsToExcludeFromTimeseries = VarsToExcludeFromTimeseries,
                             ExludeCovariances = ExludeCovariances,
                           Verbose = Verbose
          )

        return(CurPerGroupDescrStats)
      })
    names(PerGroupDescrStats) <- GroupByGroups %>% as.character()
  }

  #######################
  #Categorical / Ordinal#
  #######################
  VarDF %<>%
    mutate_if(function(x) DatesToYearCat & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(Year = function(x) lubridate::year(x) %>% factor(levels = str_sort(unique(.), numeric = TRUE), ordered = TRUE))
    ) %>%
    mutate_if(function(x) DatesToMonthCat & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(Month = function(x) lubridate::month(x) %>% factor(levels = str_sort(unique(.), numeric = TRUE), ordered = TRUE))
    ) %>%
    mutate_if(function(x) DatesToDayOfYearCat & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(DOY = function(x) lubridate::yday(x) %>% factor(levels = str_sort(unique(.), numeric = TRUE), ordered = TRUE))
    ) %>%
    mutate_if(function(x) DatesToDayOfWeekCat & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(DOW = function(x) lubridate::wday(x) %>% factor(levels = str_sort(unique(.), numeric = TRUE), ordered = TRUE))
    ) %>%
    mutate_if(function(x) DatesToDayOfMonthCat & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(DOM = function(x) lubridate::mday(x) %>% factor(levels = str_sort(unique(.), numeric = TRUE), ordered = TRUE))
    )

  NonNumericDS <-
    VarDF %>%
    dplyr::select_if(function(Var) (!is.numeric(Var) & !is.Date(Var) & !is.POSIXct(Var) & !is.POSIXlt(Var) & !is.POSIXt(Var)))

  NonNumericDSColNames <-
    NonNumericDS %>%
    names()

  if(AbbrevStrLevelsAfterNcount < 4) AbbrevStrLevelsAfterNcount <- 4 #3, 2 or 1 items are too few to be shown with the special "..." format

  UniqueTexts <- NonNumericDSColNames %>% sapply(function(x) ifelse(is.factor(NonNumericDS[[x]]), NROW(levels(NonNumericDS[[x]])), NonNumericDS[[x]] %>% unique() %>% NROW())) %>% as.numeric()
  IsFactor <- NonNumericDSColNames %>% sapply(function(x) is.factor(NonNumericDS[[x]])) %>% as.vector() #Later on, we'd like to remove Text Vars with too many unique values, not all factor vars should remain

  #TODO: Rewrite this so that the resulting Tibble has columns: Name, Obs, NAs, Texts
  if (NROW(NonNumericDSColNames) > 0 && NCOL(NonNumericDSColNames) > 0) {
    CategoricalDescriptives <-
      NonNumericDSColNames %>% sapply(function(x) {
        c(Statistics = ifelse(is.factor(NonNumericDS[[x]]), #If it's Factor
                              levels(NonNumericDS[[x]]) %>% (function(y) {
                                if (NROW(y) > AbbrevStrLevelsAfterNcount) {
                                  paste0(NonNumericDS[[x]] %>% is.not.na() %>% sum(), " Obs, ",
                                         NonNumericDS[[x]] %>% is.na() %>% sum(), " NAs, ",
                                         NROW(y), " levels: ",
                                         Left(y[[1]], 20), " (", sum(NonNumericDS[[x]] == y[[1]]), " obs, ", (sum(NonNumericDS[[x]] == y[[1]]) / NROW(NonNumericDS[[x]]) * 100) %>% round(), "%", ")", ", ",
                                         Left(y[[2]], 20), " (", sum(NonNumericDS[[x]] == y[[2]]), " obs, ", (sum(NonNumericDS[[x]] == y[[2]]) / NROW(NonNumericDS[[x]]) * 100) %>% round(), "%", ")", ", ",
                                         ifelse(NROW(y) > 4, paste0(Left(y[[3]], 20), " (", sum(NonNumericDS[[x]] == y[[3]]), " obs, ", (sum(NonNumericDS[[x]] == y[[3]]) / NROW(NonNumericDS[[x]]) * 100) %>% round(), "%", ")", ", "), ""),
                                         ifelse(NROW(y) > 5, paste0(Left(y[[4]], 20), " (", sum(NonNumericDS[[x]] == y[[4]]), " obs, ", (sum(NonNumericDS[[x]] == y[[4]]) / NROW(NonNumericDS[[x]]) * 100) %>% round(), "%", ")", ", "), ""),
                                         "...", ", ",
                                         Left(y[[NROW(y)]], 20), " (", (sum(NonNumericDS[[x]] == y[[NROW(y)]]) / NROW(NonNumericDS[[x]]) * 100) %>% round(), "%", ")"
                                  )
                                } else {
                                  paste0(NonNumericDS[[x]] %>% is.not.na() %>% sum(), " Obs, ",
                                         NonNumericDS[[x]] %>% is.na() %>% sum(), " NAs, ",
                                         NROW(y), " levels: ",
                                         implode(
                                           y %>% sapply(function(z)
                                             paste0(Left(z, 20), " (", sum(NonNumericDS[[x]] == z), " obs, ", (sum(NonNumericDS[[x]] == z) / NROW(NonNumericDS[[x]]) * 100) %>% round(), "%", ")")
                                           ),
                                           sep = ", "
                                         )
                                  )
                                }
                              }),
                              NonNumericDS[[x]] %>% unique() %>% (function(y) { #Else, if it's Character
                                if (NROW(y) > AbbrevStrLevelsAfterNcount) {
                                  paste0(NonNumericDS[[x]] %>% is.not.na() %>% sum(), " Obs, ",
                                         NonNumericDS[[x]] %>% is.na() %>% sum(), " NAs, ",
                                         NROW(y), " texts: ", Left(y[[1]], 20), ", ",  Left(y[[2]], 20), ", ", ifelse(NROW(y) > 4, paste0(Left(y[[3]], 20), ", "), ""), ifelse(NROW(y) > 5, paste0(Left(y[[4]], 20), ", "), ""), "..., ", Left(y[[NROW(y)]], 20))
                                } else {
                                  paste0(NonNumericDS[[x]] %>% is.not.na() %>% sum(), " Obs, ",
                                         NonNumericDS[[x]] %>% is.na() %>% sum(), " NAs, ",
                                         NROW(y), " texts: ", implode(Left(y, 20), sep = ", "))
                                }
                              })
        ),
        DistinctFactorsNum = 0 #When only 1 column exists, the dataframe-transformation behaviour is weird and it doesn't work, so patching it for now with this constant
        )
      }) %>% as.data.frame() %>% #as.data.frame must remain because tibble doesn't have row names
      Transpose_DF() %>%
      select(1, 2) %>%
      rename(Name = rowname)
  } else {
    CategoricalDescriptives <- NULL
  }

  if (NCOL(NonNumericDS) > 0) {
    NonNumericColsToDrop <- NonNumericDSColNames[UniqueTexts <= 1]
    NonNumericDSColNames <- NonNumericDSColNames[UniqueTexts > 1 & (IsFactor | (UniqueTexts <= 15))] #There's no reason to waste time and CPU to plot data with 0 Variation, or if they have too many levels (unique values) and they are not factor
    NonNumericDS %<>% select(all_of(NonNumericDSColNames))
  } else {
    NonNumericColsToDrop <- NULL
    NonNumericDSColNames <- NULL
  }


  ######################
  #Continuous Variables#
  ######################
  VarDF %<>%
    mutate_if(function(x) DatesToCyclicMonth & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(
                SinMon = function(x) {
                  tmp = lubridate::as_date(x)
                  sin(2 * pi * lubridate::month(tmp) / 12)
                },
                CosMon = function(x) {
                  tmp = lubridate::as_date(x)
                  cos(2 * pi * lubridate::month(tmp) / 12)
                }
              )
    ) %>%
    mutate_if(function(x) DatesToCyclicDayOfWeek & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(
                SinDOW = function(x) {
                  tmp = lubridate::as_date(x)
                  sin(2 * pi * lubridate::wday(tmp) / 7)
                },
                CosDOW = function(x) {
                  tmp = lubridate::as_date(x)
                  cos(2 * pi * lubridate::wday(tmp) / 7)
                }
              )
    ) %>%
    mutate_if(function(x) DatesToCyclicDayOfMonth & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(
                SinDOM = function(x) {
                  tmp = lubridate::as_date(x)
                  sin(2 * pi * lubridate::mday(tmp) / as.numeric(lubridate::days_in_month(tmp)))
                },
                CosDOM = function(x) {
                  tmp = lubridate::as_date(x)
                  cos(2 * pi * lubridate::mday(tmp) / as.numeric(lubridate::days_in_month(tmp)))
                }
              )
    ) %>%
    mutate_if(function(x) DatesToCyclicDayOfYear & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)),
              list(
                SinDOY = function(x) {
                  tmp = lubridate::as_date(x)
                  sin(2 * pi * lubridate::year(tmp) / 365)
                },
                CosDOY = function(x) {
                  tmp = lubridate::as_date(x)
                  cos(2 * pi * lubridate::year(tmp) / 365)
                }
              )
    ) %>%
    mutate_if(function(x) DatesToNowMinusDate & (is.Date(x) | is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x)), function(y) time_length(lubridate::now(tzone = "UTC") - as_datetime(y), unit = "second")) # TODO Names of dates should be renamed to like TimeFrom{DateVarName}


  NumericDS <-
    VarDF %>%
    dplyr::select_if(function(Var) is.logical(Var) | (IncludeInteger & is.numeric(Var)) | (is.numeric(Var) & !IncludeInteger & !is.integer(Var)) | (is.factor(Var) & is.ordered(Var)) | (
      FALSE #(is.Date(Var) | is.POSIXct(Var) | is.POSIXlt(Var) | is.POSIXt(Var)) & (DatesToNowMinusDate | DatesToCyclicMonth | DatesToCyclicDayOfWeek | DatesToCyclicDayOfMonth | DatesToCyclicDayOfYear)
      )) %>%
    select_if(function(Var) !is.Date(Var) & !is.POSIXct(Var) & !is.POSIXlt(Var) & !is.POSIXt(Var)) %>%
    mutate_if(function(x) is.factor(x) | is.logical(x), as.numeric) #It's already ordered because of select_if()

  NumericDSColNames <-
    NumericDS %>%
    names()

  NumericDescriptives <-
    NumericDSColNames %>% sapply(function(NumVar) c(Min      =  NumericDS[[NumVar]] %>% min(na.rm = TRUE)                              %>% round(RoundAt),
                                                    Q1       = (NumericDS[[NumVar]] %>% quantile(probs = 0.25, na.rm = TRUE))[[1]]     %>% round(RoundAt),
                                                    Mean     =  NumericDS[[NumVar]] %>% mean(na.rm = TRUE)                             %>% round(RoundAt),
                                                    Median   =  NumericDS[[NumVar]] %>% median(na.rm = TRUE)                           %>% round(RoundAt),
                                                    Q3       = (NumericDS[[NumVar]] %>% quantile(probs = 0.75, na.rm = TRUE))[[1]]     %>% round(RoundAt),
                                                    Max      =  NumericDS[[NumVar]] %>% max(na.rm = TRUE)                              %>% round(RoundAt),
                                                    SD       =  NumericDS[[NumVar]] %>% sd(na.rm = TRUE) %>% {ifelse(is.na(.), 0, .)}  %>% round(RoundAt),
                                                    IQR      =  NumericDS[[NumVar]] %>% IQR(na.rm = TRUE)                              %>% round(RoundAt),
                                                    Kurtosis =  NumericDS[[NumVar]] %>% e1071::kurtosis(na.rm = TRUE)                  %>% round(RoundAt),
                                                    Skewness =  NumericDS[[NumVar]] %>% e1071::skewness(na.rm = TRUE)                  %>% round(RoundAt),
                                                    Obs      =  NumericDS[[NumVar]] %>% is.not.na() %>% sum(),
                                                    NAs      =  NumericDS[[NumVar]] %>% is.na() %>% sum(),
                                                    NAsRatio = (NumericDS[[NumVar]] %>% is.na() %>% sum() / NROW(NumericDS[[NumVar]])) %>% round(RoundAt)
    )
    ) %>%
    data.frame() %>%
    Transpose_DF %>% {
      if (NROW(.) > 0 | NCOL(.) > 0) (.) %>% rename(Name = rowname) else (.) %>% as_tibble()
    } %>%
    mutate(Present = ((1-NAsRatio) * 100) %>% sapply(function(x) paste0(implode(rep(" ", 3 - nchar(round(x,1)))), x, "%")))


  NumericColsToDrop <- NumericDSColNames[NumericDescriptives$SD == 0]
  NumericDSColNames <- NumericDSColNames[NumericDescriptives$SD > 0] %>% na.omit() %>% as.vector() #There's no reason to waste time and CPU to plot data with 0 Variation
  NumericDS %<>% select(all_of(NumericDSColNames))


  if (!NoPrints) {
    if (NROW(NumericDSColNames) > 0) {
      cat("Continuous Variables:\n")
      print(NumericDescriptives)
      cat("\n")
    }
    if (NROW(NonNumericDSColNames) > 0) {
      cat("Categorical Variables\n")
      print(CategoricalDescriptives)
    }
  }


  if (NROW(NumericColsToDrop) > 0) cat("\n[", NROW(NumericColsToDrop), "] Numeric columns were dropped because there was 0 Variation: ", implode(NumericColsToDrop, sep = ", "), "\n\n")
  if (NROW(NonNumericColsToDrop) > 0) cat("[", NROW(NonNumericColsToDrop), "] Categorical columns were dropped because there was 0 Variation: ", implode(NonNumericColsToDrop, sep = ","), "\n\n")

  if (is.not.null(DependentVar) && DependentVar %in% c(NumericColsToDrop, NonNumericColsToDrop)) {
    print(paste0("The dependent variable [", DependentVar, "] had zero variance and was removed, so statistics/plots that required a Dependent variable will not be calculated any after all."))
    DependentVar <- NULL #If the DependentVar happens to be amongst the Dropped Variables, then there's no Dependent Variable in the Dataset
  }


  CyclicDates <- c("SinMon", "CosMon", "SinDOW", "CosDOW", "SinDOM", "CosDOM", "SinDOY", "CosDOY")
  ExcludeFromTimeseries <- c(TimeFlowVarName, VarsToExcludeFromTimeseries, "Year", "Month", "DOY", "DOW", "DOM", CyclicDates)

  #############################
  ### Sampling if Necessary ###
  #############################
  if (SampleIfNRowGT > 0 && NROW(VarDF) > SampleIfNRowGT) {
    if (is.not.null(SeedForSampling)) set.seed(SeedForSampling)
    FilteredIndx <- rbernoulli(NROW(VarDF), SampleIfNRowGT/NROW(VarDF))
    NumericDS <- NumericDS[FilteredIndx, ]
    NonNumericDS <- NonNumericDS[FilteredIndx, ]
    if (is.not.null(BoxplotPointsColourVar)) BoxplotPointsColourVar <- BoxplotPointsColourVar[FilteredIndx]
    cat("\nAll Statistics and Plots are calculated using", sum(FilteredIndx), "randomly sampled rows from the original", NROW(VarDF), "row Dataset\n")
  } else {
    FilteredIndx <- TRUE
  }

  #===============#
  #== Per Group ==#
  #===============#
  if (is.not.null(GroupBy)) {
    for (CurGroup in GroupByGroups) { #If we ever need the whole unfiltered CurGroupIndx for VarDF manipulation, we can remove the '[FilteredIndx]' from here
      CurGroupFilterIndx[[CurGroup]] <- CurGroupFilterIndx[[CurGroup]][FilteredIndx] #and add if on top of 'CurGroupFilterIndx[[CurGroup]]' in every call later on
    }
  }

  PearsonCor <- NULL
  PearsonCorPlot <- NULL
  PearsonCorPVal <- NULL
  PearsonCorOrdered <- NULL
  PearsonCorOrderedPlot <- NULL
  PearsonCorOrderedPVal <- NULL
  SpearmanCor <- NULL
  SpearmanCorPlot <- NULL
  SpearmanCorPVal <- NULL
  SpearmanCorOrdered <- NULL
  SpearmanCorOrderedPlot <- NULL
  SpearmanCorOrderedPVal <- NULL
  ############################
  ### Correlation Analysis ###
  ############################
  CorDS <-
    NumericDS %>%
    rename_all(Left, 20) %>%
    setNames(make.names(names(.), unique = TRUE))


  if (NCOL(CorDS) > 1) {
    ####################
    #== Correlations ==#
    ####################
    NewOrder <- NULL
    tryCatch({
      if (is.not.null(CorrVarOrder) & NCOL(CorDS) > 1 & NROW(CorDS) > 0) {
        if (CorrVarOrder == "AB") {
          NewOrder <- (CorDS %>% names %>% sort(index.return = TRUE))$ix
        } else {
          NewOrder <- (CorDS %>% {
            if (is.not.null(GroupBy)) (.) %>% group_by(VarDF[FilteredIndx, ] %>% pull(!!sym(GroupBy))) else (.)
          } %>%
            {
              if (IsTimeSeries) {
                (.) %>% tidyr::fill(everything(), .direction = "downup") #Timeseries should be ordered ascending, so the oldest value is 1st, and you fill down missing values, then up
              } else {
                (.) %>% mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
              }
            } %>%
            ungroup() %>%
            select_if(is.numeric) %>%
            as.matrix() %>%
            seriate(CorrVarOrder, margin = 2)
                       )[[1]] %>% as.numeric()
        }
      }
    }, error = function(e) {
      NewOrder <- NULL
      cat(paste0("An error occurred when trying to get a cluster Correlation Order for the correlation matrix:\n", e, "\n"))
    })

    #== Pearson ==#
    if (Verbose) cat(toString(now()), "Calculating Pearson Correlations\n")
    #Performing a Pearson's Correlation on the Continuous Variables
    PearsonCor <- CorDS %>% cor(method = "pearson", use = "pairwise.complete.obs") %>% round(2)
    PearsonCorPlot <- PearsonCor
    #The Upper and Lower Triangles have the same values, so Let's only keep the results once
    PearsonCor[lower.tri(PearsonCor)] <- NA
    PearsonCor %<>% as_tibble()

    if (is.not.null(CorrVarOrder) & is.not.null(NewOrder)) {
      PearsonCorOrdered <- PearsonCorPlot[NewOrder, NewOrder]
      PearsonCorOrderedPlot <- PearsonCorOrdered
      PearsonCorOrdered[lower.tri(PearsonCorOrdered)] <- NA
      PearsonCorOrdered %<>% as_tibble()
    }

    if (CalcPValues == TRUE) {
      if (Verbose) cat(toString(now()), "Calculating Pearson Correlation p.values\n")

      tryCatch({
        # PearsonCorPVal <- cor.mtest(CorDS, conf.level = SignificanceLevel, method = "pearson")
        PearsonCorPVal <- diag(x = 0, nrow = CorDS %>% names %>% NROW())
        PearsonCorPVal[lower.tri(PearsonCorPVal)] <- apply(combn(CorDS %>% names(), 2), 2, function(Comb) { #In the end, the upper triangle is filled in with values,
          CurCorDS <- CorDS %>% select(!!Comb[[1]], !!Comb[[2]]) %>% drop_na()
          CurCorDSNROW <- CurCorDS %>% NROW()
          if (CurCorDSNROW > 2) {
            return(cor.test(CorDS %>% pull(!!Comb[[1]]), CorDS %>% pull(!!Comb[[2]]), conf.level = SignificanceLevel, method = "pearson")$p.value)
          }
            return(NA)
          })
        PearsonCorPVal <- t(PearsonCorPVal) #But because R assigns values to a matrix by column isntead of by row, we counter that by assigning on lower.tri and then transposing.
        PearsonCorPVal[lower.tri(PearsonCorPVal)] <- t(PearsonCorPVal)[lower.tri(PearsonCorPVal)]

        rownames(PearsonCorPVal) <- names(CorDS)
        # rownames(PearsonCorPVal$lowCI) <- names(CorDS)
        # rownames(PearsonCorPVal$uppCI) <- names(CorDS)
        colnames(PearsonCorPVal) <- names(CorDS)
        # colnames(PearsonCorPVal$lowCI) <- names(CorDS)
        # colnames(PearsonCorPVal$uppCI) <- names(CorDS)

        PearsonCorPlot[is.na(PearsonCorPVal)] <- NA #If a p-value is non-calculable we might have just 2 points and show 100% correlation, but it's bogus!
        if (is.not.null(CorrVarOrder) & is.not.null(NewOrder)) {
          PearsonCorOrderedPVal <- PearsonCorPVal[NewOrder, NewOrder]
          PearsonCorOrderedPlot[is.na(PearsonCorOrderedPVal)] <- NA #If a p-value is non-calculable we might have just 2 points and show 100% correlation, but it's bogus!
        } else {
            PearsonCorOrderedPVal <- NULL
            }
      }, error = function(e) {
        PearsonCorPVal <- NULL
        PearsonCorOrderedPVal <- NULL
        cat(paste0("An error occurred during Pearson p-value calculation:\n", e, "\n"))
      })

    } else {
      PearsonCorPVal <- NULL
      PearsonCorOrderedPVal <- NULL
      cat("Skipping Pearson p-value calculation\n")
    }

    #== Spearman ==#
    if (Verbose) cat(toString(now()), "Calculating Spearman Correlations\n")
    #Performing a spearman's Correlation on the Continuous Variables
    SpearmanCor <- CorDS %>% cor(method = "spearman", use = "pairwise.complete.obs") %>% round(2)
    SpearmanCorPlot <- SpearmanCor
    #The Upper and Lower Triangles have the same values, so Let's only keep the resulsts once
    SpearmanCor[lower.tri(SpearmanCor)] <- NA
    SpearmanCor %<>% as_tibble()

    if (is.not.null(CorrVarOrder) & is.not.null(NewOrder)) {
      SpearmanCorOrdered <- SpearmanCorPlot[NewOrder, NewOrder]
      SpearmanCorOrderedPlot <- SpearmanCorOrdered
      SpearmanCorOrdered[lower.tri(SpearmanCorOrdered)] <- NA
      SpearmanCorOrdered %<>% as_tibble()
    }

    if (CalcPValues == TRUE) {
      if (Verbose) cat(toString(now()), "Calculating Spearman Correlation p.values\n")
      # SpearmanCorPVal <- cor.mtest(CorDS, conf.level = SignificanceLevel, method = "spearman")
      SpearmanCorPVal <- diag(x = 0, nrow = CorDS %>% names %>% NROW())
      SpearmanCorPVal[lower.tri(SpearmanCorPVal)] <- apply(combn(CorDS %>% names(), 2), 2, function(Comb) { #In the end, the upper triangle is filled in with values,
        CurCorDS <- CorDS %>% select(!!Comb[[1]], !!Comb[[2]]) %>% drop_na()
        CurCorDSNROW <- CurCorDS %>% NROW()
        if (CurCorDSNROW > 2) {
          return(cor.test(CorDS %>% pull(!!Comb[[1]]), CorDS %>% pull(!!Comb[[2]]), conf.level = SignificanceLevel, method = "spearman")$p.value)
        }
          return(NA)
        })
      SpearmanCorPVal <- t(SpearmanCorPVal) #But because R assigns values to a matrix by column isntead of by row, we counter that by assigning on lower.tri and then transposing.
      SpearmanCorPVal[lower.tri(SpearmanCorPVal)] <- t(SpearmanCorPVal)[lower.tri(SpearmanCorPVal)]

      rownames(SpearmanCorPVal) <- names(CorDS)
      # rownames(SpearmanCorPVal$lowCI) <- names(CorDS)
      # rownames(SpearmanCorPVal$uppCI) <- names(CorDS)
      colnames(SpearmanCorPVal) <- names(CorDS)
      # colnames(SpearmanCorPVal$lowCI) <- names(CorDS)
      # colnames(SpearmanCorPVal$uppCI) <- names(CorDS)

      SpearmanCorPlot[is.na(SpearmanCorPVal)] <- NA #If a p-value is non-calculable we might have just 2 points and show 100% correlation, but it's bogus!
      if (is.not.null(CorrVarOrder) & is.not.null(NewOrder)) {
        SpearmanCorOrderedPVal <- SpearmanCorPVal[NewOrder, NewOrder]
        SpearmanCorOrderedPlot[is.na(SpearmanCorOrderedPVal)] <- NA #If a p-value is non-calculable we might have just 2 points and show 100% correlation, but it's bogus!
      } else {
          SpearmanCorOrderedPVal <- NULL
      }
    } else {
      SpearmanCorPVal <- NULL
      SpearmanCorOrderedPVal <- NULL
      cat("Skipping Spearman p-value calculation\n")
    }


    if (Verbose) cat(toString(now()), "Calculating Pearson Correlation Plot\n")
    #We're calculating the Pearson Correlations even if CalculateGraphs = FALSE
    # corrplot.mixed(PearsonCorPlot, p.mat = NULL, #PearsonCorPVal %>% {if (is.not.null(.)) PearsonCorPVal$p else NULL}, Can't use this because of bug: https://github.com/taiyun/corrplot/issues/120
    #                lower = "number", upper = "square", tl.pos = TitlePosition, tl.cex = TitleSize, sig.level = SignificanceLevel, order = CorrVarOrder)
    # PearsonCorPlot <- recordPlot()
    # invisible(dev.off())

    PearsonCorPlot <-
      ggcorrplot(PearsonCorPlot, method = "square", type = "lower", outline.col = "black", hc.order = FALSE, lab = TRUE, p.mat = PearsonCorPVal, sig.level = SignificanceLevel)

    if (is.not.null(CorrVarOrder)) {
      PearsonCorOrderedPlot <-
        ggcorrplot(PearsonCorOrderedPlot, method = "square", type = "lower", outline.col = "black", hc.order = CorrVarOrder == "hclust", lab = TRUE, p.mat = PearsonCorOrderedPVal, sig.level = SignificanceLevel) #For PCA or alphabetical, the order is already applied before.
    } else {
      PearsonCorOrderedPlot <- NULL
    }


    if (!NoPrints) {
      cat("\nCorrelation upper triangle matrix (Spearman)\n")
      SpearmanCor %>%
        print() #Viewing the results
    }
  }

  NumericDistrGraphs = NULL
  NumericDistrGraphsPerGroup = NULL
  BoxplotGraphs <- NULL
  BoxplotGraphsPerGroup <- NULL
  BarChartGraphs <- NULL
  BarChartGraphsPerGroup <- NULL
  StatInferNumGraphs <- NULL
  StatInferNumGraphsPerGroup <- NULL
  StatInferCatGraphs <- NULL
  StatInferCatGraphsPerGroup <- NULL
  TimeProgressionPlots <- NULL
  AutoCorrelationsPlots = NULL
  PartialAutoCorrelationsPlots = NULL
  AutoCovariancePlots = NULL
  TaperedAutoCorrelationsPlots = NULL
  TaperedPartialAutoCorrelationsPlots = NULL
  CrossCorrelationPlots = NULL
  CrossCovariancePlots = NULL
  #==========================#
  #=== Calculating Graphs ===#
  #==========================#
  if (CalculateGraphs) {
    if (NCOL(CorDS) > 1) {
      #Perason's graph is calculated before
      if(ShowGraphs) print(PearsonCorPlot)

      # if (Verbose) cat(toString(now()), "Calculating Spearman Correlation Upper Triangle\n")
      # corrplot.mixed(SpearmanCorPlot, p.mat = NULL, #SpearmanCorPVal %>% {if(is.not.null(.)) SpearmanCorPVal$p else NULL}, Can't use this because of bug: https://github.com/taiyun/corrplot/issues/120
      #                lower = "number", upper = "square", tl.pos = TitlePosition, tl.cex = TitleSize, sig.level = SignificanceLevel, order = CorrVarOrder)
      # SpearmanCorPlot <- recordPlot()
      # invisible(dev.off())

      SpearmanCorPlot <-
        ggcorrplot(SpearmanCorPlot, method = "square", type = "lower", outline.col = "black", hc.order = FALSE, lab = TRUE, p.mat = SpearmanCorPVal, sig.level = SignificanceLevel)

      if (is.not.null(CorrVarOrder)) SpearmanCorOrderedPlot <-
        ggcorrplot(SpearmanCorOrderedPlot, method = "square", type = "lower", outline.col = "black", hc.order = CorrVarOrder == "hclust", lab = TRUE, p.mat = SpearmanCorOrderedPVal, sig.level = SignificanceLevel) #For PCA or alphabetical, the order is already applied before.

      if (ShowGraphs) print(SpearmanCorPlot)
    }

    #########################################
    ###           Continuous:             ###
    ### Viewing Distributions: Histograms ###
    #########################################
    if (Verbose) cat(toString(now()), "Building the Distribution Histograms\n")
    if (NROW(NumericDSColNames) > 0) MeanValues <- NumericDescriptives %>% filter(Name %notin% !!NumericColsToDrop) %>% pull(Mean) %>% set_names(NumericDSColNames) else MeanValues <- NULL
    NumericDistrGraphs <-
      lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName, MeanValues) {
        Binwidth <- 2 * (NumericDescriptives %>% filter(Name == NumVarName) %>% pull(IQR)) / CubeRoot((NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Obs))) #based on Freedman–Diaconis rule
        BinsCount <- ((NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Max)) - (NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Min))) / Binwidth

        ggplot(data = NumericDS, aes(x = !!sym(NumVarName))) +
          geom_histogram(aes(y = ..density..), bins = BinsCount, binwidth = Binwidth, alpha = 0.7, fill = "#0c4c8a") +
          geom_density(aes(y = ..density..), fill = "#ff4d4d", alpha = 0.5) +
          geom_vline(data = NumericDS, aes(xintercept = MeanValues[[NumVarName]], color = "red"), linetype = "dashed", show.legend = FALSE) #+
        # stat_function(fun = function(y) (dnorm(y,
        #                                        mean = NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Mean),
        #                                        sd = NumericDescriptives %>% filter(Name == NumVarName) %>% pull(SD)
        #                                       ) * (NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Obs)) * Binwidth)
        #               )
      }, MeanValues)
    names(NumericDistrGraphs) <- NumericDSColNames %>% setdiff(ExcludeFromTimeseries)

    if(ShowGraphs) {
      if(AllHistsOn1Page) {
        grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
          ggplotGrob(NumericDistrGraphs[[NumVarName]])
        }),
        nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)))))
      } else {
        print(NumericDistrGraphs)
      }
    }

    #===============#
    #== Per Group ==#
    #===============#
    if (is.not.null(GroupBy)) {
      for (NumVarName in NumericDSColNames %>% setdiff(ExcludeFromTimeseries)) {
        NumericDistrGraphsPerGroup[[NumVarName]] <-
          lapply(GroupByGroups, function(CurGroup) {
            Binwidth <- 2 * (PerGroupDescrStats[[CurGroup]]$NumericDescriptives %>% filter(Name == NumVarName) %>% pull(IQR)) / CubeRoot((PerGroupDescrStats[[CurGroup]]$NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Obs))) #based on Freedman–Diaconis rule
            BinsCount <- ((PerGroupDescrStats[[CurGroup]]$NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Max)) - (PerGroupDescrStats[[CurGroup]]$NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Min))) / Binwidth

            ggplot(data = NumericDS %>% filter(CurGroupFilterIndx[[CurGroup]]), aes(x = !!sym(NumVarName))) +
              ggtitle(paste0(GroupBy, ": ", as.character(CurGroup))) +
              xlim(min(NumericDS[[NumVarName]], na.rm = TRUE), max(NumericDS[[NumVarName]], na.rm = TRUE)) +
              geom_histogram(aes(y = ..density..), bins = BinsCount, binwidth = Binwidth, alpha = 0.7, fill = "#0c4c8a") +
              geom_density(aes(y = ..density..), fill = "#ff4d4d", alpha = 0.5) +
              geom_vline(data = NumericDS %>% filter(CurGroupFilterIndx[[CurGroup]]), aes(xintercept = PerGroupDescrStats[[CurGroup]]$NumericDescriptives %>% filter(Name == !!NumVarName) %>% pull(Mean), color="red"), linetype = "dashed", show.legend = FALSE) #+
            # stat_function(fun = function(y) (dnorm(y,
            #                                        mean = PerGroupDescrStats[[CurGroup]]$NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Mean),
            #                                        sd = PerGroupDescrStats[[CurGroup]]$NumericDescriptives %>% filter(Name == NumVarName) %>% pull(SD)
            #                                       ) * (PerGroupDescrStats[[CurGroup]]$NumericDescriptives %>% filter(Name == NumVarName) %>% pull(Obs)) * Binwidth)
            #               )
          })
        names(NumericDistrGraphsPerGroup[[NumVarName]]) <- GroupByGroups
      }
    }


    ###########################
    ### Continuous: Boxplot ###
    ###########################
    if (Verbose) cat(toString(now()), "Building Boxplots\n")

    if(AllBoxplotsOn1Page) { #When there are differences in the value range, the low-range plots are practically invisible
      NumericDS_Melted <-
        suppressMessages(reshape2::melt(NumericDS)) %>%
        as_tibble() %>%
        drop_na()

      BoxplotGraphs <-
        ggplot(data = NumericDS_Melted) +
        aes(x = variable, y = value) +
        geom_boxplot(aes(fill = variable), na.rm = TRUE)

    } else {
      BoxplotGraphs <-
        lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
          ggplot(data = NumericDS) +
            aes(x = 0, y = !!sym(NumVarName)) +
            geom_boxplot(fill = "#0c4c8a", na.rm = TRUE) +
            xlab("") +
            ylab("") +
            {if (is.not.null(BoxplotPointsColourVar)) { ##Points (jitter) cannot be applied by fill-group when 'fill' is set on geom_boxplot
              geom_jitter(aes(colour = BoxplotPointsColourVar), shape = 16, position = position_jitter(0.2), size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
            } else {
              geom_jitter(shape = 16, position = position_jitter(0.2), colour = "red", size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
            }} +
            ggtitle(NumVarName) +
            {if (is.not.null(BoxplotPointsColourVar)) scale_colour_gradient(low = "#FF0000", high = "#0000FF") else NULL} +
            {if (is.not.null(BoxplotPointsColourVar)) labs(colour = ColourVarName) else NULL}
        })
      names(BoxplotGraphs) <- NumericDSColNames %>% setdiff(ExcludeFromTimeseries)
    }

    if(ShowGraphs) grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
      ggplotGrob(BoxplotGraphs[[NumVarName]])
    }),
    nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)))))

    #===============#
    #== Per Group ==#
    #===============#
    #For per group 'AllBoxplotsOn1Page = FALSE' always
    if (is.not.null(GroupBy)) {
      for (NumVarName in NumericDSColNames %>% setdiff(ExcludeFromTimeseries)) {
        NumericDS_Melted <- #Is this just VarDF[c(GroupBy, NumVarName)] %>% rename(variable = GroupBy, value = NumVarName)?? TODO! Also [FilteredIndx,] on VarDF?
          bind_rows(
            lapply(GroupByGroups, function(CurGroup) {
              tibble(
                variable = CurGroup,
                value = NumericDS %>% filter(CurGroupFilterIndx[[CurGroup]]) %>% pull(!!sym(NumVarName))
              )
            })
          )

        BoxplotGraphsPerGroup[[NumVarName]] <-
          ggplot(data = NumericDS_Melted) +
          aes(x = variable, y = value) +
          geom_boxplot(aes(fill = variable), na.rm = TRUE, show.legend = FALSE) +
          labs(fill = GroupBy) + xlab(GroupBy) + ylab(NumVarName)
      }

      if(ShowGraphs) {
        grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
          ggplotGrob(BoxplotGraphsPerGroup[[NumVarName]])
        }),
        nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(c(ExcludeFromTimeseries))))))
      }
    }


    ##############################
    ### Categorical: Bar Chart ###
    ##############################
    if (NROW(NonNumericDSColNames %>% setdiff(c("DOY", "DOM"))) > 0) {
      if (Verbose) cat(toString(now()), "Building Barcharts\n")
      BarChartGraphs <-
        lapply(NonNumericDSColNames %>% setdiff(c("DOY", "DOM")), function(CatVarName) { #DOY and DOM have TOO many levels for plotting
          ggplot(data = NonNumericDS) +
            aes(x = !!sym(CatVarName)) +
            geom_bar(aes(fill = !!sym(CatVarName))) +
            geom_text(stat = 'count', aes(label = ..count..), vjust = -.25) #+ #Always shows 100%
            # geom_text(stat = "count", aes(label = scales::percent(..prop..), y = ..prop..), vjust = +.75)
        })
      names(BarChartGraphs) <- NonNumericDSColNames %>% setdiff(c("DOY", "DOM"))

      if(ShowGraphs) {
        if(AllBarChartsOn1Page) {
          grid.arrange(grobs = lapply(NonNumericDSColNames %>% setdiff(c("DOY", "DOM")), function(CatVarName) {
            ggplotGrob(BarChartGraphs[[CatVarName]])
          }),
          nrow = round(sqrt(NROW(NonNumericDSColNames %>% setdiff(c("DOY", "DOM"))))))
        } else {
          print(BarChartGraphs)
        }
      }

      #===============#
      #== Per Group ==#
      #===============#
      if (is.not.null(GroupBy) & NROW(NonNumericDSColNames %>% setdiff(c("DOY", "DOM"))) > 1) { #We need +1 because the GroupBy variable is going to dissapear
        BarChartGraphsPerGroup <-
          lapply(NonNumericDSColNames %>% setdiff(c("DOY", "DOM")) %>% setdiff(GroupBy), function(CatVarName) {

            CurDS <- #TODO! This dataset is just VarDF %>% select(CatVarName, CurGroup) #Also [FilteredIndx,] on VarDF?
              bind_rows(
                lapply(GroupByGroups, function(CurGroup) {
                  tibble(
                    !!sym(CatVarName) := NonNumericDS %>% filter(CurGroupFilterIndx[[CurGroup]]) %>% pull(!!sym(CatVarName)),
                    !!sym(GroupBy) := CurGroup
                  )
                })
              )

            ggplot(data = CurDS) +
              aes(x = !!sym(CatVarName)) +
              geom_bar(aes(fill = !!sym(GroupBy)))

          })
        names(BarChartGraphsPerGroup) <- NonNumericDSColNames %>% setdiff(c("DOY", "DOM")) %>% setdiff(GroupBy)
      }

      if(ShowGraphs) {
          grid.arrange(grobs = lapply(NonNumericDSColNames %>% setdiff(c("DOY", "DOM")) %>% setdiff(GroupBy), function(CatVarName) {
            ggplotGrob(BarChartGraphsPerGroup[[CatVarName]])
          }),
          nrow = round(sqrt(NROW(NonNumericDSColNames %>% setdiff(c("DOY", "DOM")) %>% setdiff(GroupBy)))))
      }
    }


    #############################
    ### Statistical Inference ###
    #############################
    if (is.not.null(DependentVar) && (DependentVar %in% (VarDF %>% names()))) {
      if (Verbose) cat(toString(now()), "Building Statistical Inference Plots\n")
      DepVarIsNumeric <- !(DependentVar %in% (NonNumericDS %>% names()))

      if (DepVarIsNumeric) {
        ###################################################################
        ### Creating the Numeric Indep. vars VS Numeric Dep. Var Graphs ###
        ###################################################################
        StatInferNumGraphsColNames <- NumericDSColNames[NumericDSColNames %notin% DependentVar] %>% setdiff(CyclicDates)
        if (NROW(StatInferNumGraphsColNames) > 0) {
          StatInferNumGraphs <-
            lapply(StatInferNumGraphsColNames, function(NumVarName) {
              ggplot(data = NumericDS, aes(x = !!sym(NumVarName), y = !!sym(DependentVar))) +
                {if (is.not.null(BoxplotPointsColourVar)) geom_point(aes(colour = BoxplotPointsColourVar)) else geom_point(colour = "#0c4c8a", na.rm = TRUE)} +
                {if (is.not.null(BoxplotPointsColourVar)) labs(colour = ColourVarName) else NULL} +
                scale_colour_gradient(low = "#FF0000", high = "#0000FF")
            })
          names(StatInferNumGraphs) <- StatInferNumGraphsColNames

          if (ShowGraphs) {
            #Numeric Statistical Inference Graphs
            grid.arrange(grobs = lapply(StatInferNumGraphsColNames, function(NumVarName) {
              ggplotGrob(StatInferNumGraphs[[NumVarName]])
            }),
            nrow = round(sqrt(NROW(StatInferNumGraphsColNames))))
          }


          #===============#
          #== Per Group ==#
          #===============#
          if (is.not.null(GroupBy)) {
            for (NumVarName in StatInferNumGraphsColNames) {
              StatInferNumGraphsPerGroup[[NumVarName]] <-
                lapply(GroupByGroups, function(CurGroup) {
                  ggplot(data = NumericDS %>% filter(CurGroupFilterIndx[[CurGroup]]), aes(x = !!sym(NumVarName), y = !!sym(DependentVar))) +
                    ylim(min(NumericDS[[DependentVar]], na.rm = TRUE), max(NumericDS[[DependentVar]], na.rm = TRUE)) +
                    {if (is.not.null(BoxplotPointsColourVar)) geom_point(aes(colour = BoxplotPointsColourVar[CurGroupFilterIndx[[CurGroup]]])) else geom_point(colour = "#0c4c8a", na.rm = TRUE)} +
                    {if (is.not.null(BoxplotPointsColourVar)) labs(colour = ColourVarName) else NULL} +
                    scale_colour_gradient(low = "#FF0000", high = "#0000FF", limits = c(min(BoxplotPointsColourVar), max(BoxplotPointsColourVar))) +
                    ggtitle(paste0(GroupBy, ": ", CurGroup))
                })
              names(StatInferNumGraphsPerGroup[[NumVarName]]) <- GroupByGroups
            }
          }
        }

        #######################################################################
        ### Creating the Categorical Indep. vars VS Numeric Dep. Var Graphs ###
        #######################################################################
        CatStatInfDS <- #This was above, between 'names(StatInferNumGraphs) <- ..' and 'if (ShowGraphs) {', probably by mistake?
          NumericDS %>% select(!!sym(DependentVar)) %>%
          bind_cols(NonNumericDS %>% select_if(function(CatVarName) is.factor(CatVarName) | (!is.numeric(CatVarName) & NROW(unique(CatVarName)) <= 10)))

        StatInferCatGraphsColNames <- names(CatStatInfDS)[names(CatStatInfDS) %notin% DependentVar] %>% setdiff(c("DOY", "DOM"))
        if (NROW(StatInferCatGraphsColNames) > 0) {
          StatInferCatGraphs <-
            lapply(StatInferCatGraphsColNames, function(CatVarName) {
              ggplot(data = CatStatInfDS, aes(x = !!sym(CatVarName), y = !!sym(DependentVar))) +
                geom_boxplot(aes(fill = !!sym(CatVarName)), na.rm = TRUE) +
                {if (is.not.null(BoxplotPointsColourVar)) {
                  geom_jitter(aes(colour = BoxplotPointsColourVar), shape = 16, position = position_jitter(0.2), size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
                } else {
                  geom_jitter(shape = 16, position = position_jitter(0.2), colour = "red", size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
                }} +
                {if (is.not.null(BoxplotPointsColourVar)) scale_colour_gradient(low = "#FF0000", high = "#0000FF") else NULL} +
                {if (is.not.null(BoxplotPointsColourVar)) labs(colour = ColourVarName) else NULL}
            })
          names(StatInferCatGraphs) <- StatInferCatGraphsColNames

          if (ShowGraphs) {
            #Categorical Statistical Inference Graphs
            grid.arrange(grobs = lapply(StatInferCatGraphsColNames, function(NumVarName) {
              ggplotGrob(StatInferCatGraphs[[NumVarName]])
            }),
            nrow = round(sqrt(NROW(StatInferCatGraphsColNames))))
          }


          #===============#
          #== Per Group ==#
          #===============#
          if (is.not.null(GroupBy) & NROW(StatInferCatGraphsColNames) > 1) {
            for (CatVarName in StatInferCatGraphsColNames[StatInferCatGraphsColNames %notin% GroupBy]) {
              StatInferCatGraphsPerGroup[[CatVarName]] <-
                lapply(GroupByGroups, function(CurGroup) {
                  ggplot(data = CatStatInfDS %>% filter(CurGroupFilterIndx[[CurGroup]]), aes(x = !!sym(CatVarName), y = !!sym(DependentVar))) +
                    ylim(min(CatStatInfDS[[DependentVar]], na.rm = TRUE), max(CatStatInfDS[[DependentVar]], na.rm = TRUE)) +
                    geom_boxplot(fill = "#0c4c8a", na.rm = TRUE) +
                    ggtitle(paste0(GroupBy, ": ", CurGroup)) +
                    {if (is.not.null(BoxplotPointsColourVar)) {
                      geom_jitter(aes(colour = BoxplotPointsColourVar[CurGroupFilterIndx[[CurGroup]]]), shape = 16, position = position_jitter(0.2), size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
                    } else {
                      geom_jitter(shape = 16, position = position_jitter(0.2), colour = "red", size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
                    }} +
                    {if (is.not.null(BoxplotPointsColourVar)) scale_colour_gradient(low = "#FF0000", high = "#0000FF", limits = c(min(BoxplotPointsColourVar), max(BoxplotPointsColourVar))) else NULL} +
                    {if (is.not.null(BoxplotPointsColourVar)) labs(colour = ColourVarName) else NULL}
                })
              names(StatInferCatGraphsPerGroup[[CatVarName]]) <- GroupByGroups
            }
          }
        }


      } else { #Dependent Var is Categorical then
        #######################################################################
        ### Creating the Numeric Indep. vars VS Categorical Dep. Var Graphs ###
        #######################################################################
        NumStatInfDS <-
          NonNumericDS %>% select(!!sym(DependentVar)) %>%
          bind_cols(NumericDS %>% select(-one_of(DependentVar)))

        StatInferNumGraphsColNames <- names(NumStatInfDS)[names(NumStatInfDS) %notin% DependentVar] %>% setdiff(ExcludeFromTimeseries)
        if (NROW(StatInferNumGraphsColNames) > 0) {
          StatInferNumGraphs <-
            lapply(StatInferNumGraphsColNames, function(CatVar) {
              ggplot(data = NumStatInfDS) +
                aes(x = !!sym(DependentVar), y = !!sym(CatVar)) + #Dependent on X-axis as it is categorical
                geom_boxplot(aes(fill = !!sym(DependentVar)), na.rm = TRUE)
            })
          names(StatInferNumGraphs) <- StatInferNumGraphsColNames

          if (is.not.null(BoxplotPointsColourVar)) {
            for (i in 1:NROW(StatInferNumGraphs)) {
              StatInferNumGraphs[[i]] <- StatInferNumGraphs[[i]] + geom_jitter(aes(colour = BoxplotPointsColourVar), shape = 16, position = position_jitter(0.2), size = BoxPlotPointSize, alpha = BoxPlotPointAlpha) + scale_colour_gradient(low = "#FF0000", high = "#0000FF") +
                labs(colour = ColourVarName) ##########
            }
          } else {
            for (i in 1:NROW(StatInferNumGraphs)) {
              StatInferNumGraphs[[i]] <- StatInferNumGraphs[[i]] + geom_jitter(shape = 16, position = position_jitter(0.2), colour = "red", size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
            }
          }

          if (ShowGraphs) {
            #Numeric Statistical Inference Graphs
            grid.arrange(grobs = lapply(StatInferNumGraphsColNames, function(NumVarName) {
              ggplotGrob(StatInferNumGraphs[[NumVarName]])
            }),
            nrow = round(sqrt(NROW(StatInferNumGraphsColNames))))
          }


          #===============#
          #== Per Group ==#
          #===============#
          if (is.not.null(GroupBy)) {
            for (NumVarName in StatInferNumGraphsColNames) {
              StatInferNumGraphsPerGroup[[NumVarName]] <-
                lapply(GroupByGroups, function(CurGroup) {
                  ggplot(data = NumStatInfDS %>% filter(CurGroupFilterIndx[[CurGroup]]), aes(x = !!sym(DependentVar), y = !!sym(NumVarName))) + #Dependent on X-axis as it is categorical
                    geom_boxplot(aes(fill = !!sym(DependentVar)), na.rm = TRUE) +
                    ggtitle(paste0(GroupBy, ": ", CurGroup)) +
                    {if (is.not.null(BoxplotPointsColourVar)) {
                      geom_jitter(aes(colour = BoxplotPointsColourVar[CurGroupFilterIndx[[CurGroup]]]), shape = 16, position = position_jitter(0.2), size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
                     }
                     else {
                       geom_jitter(shape = 16, position = position_jitter(0.2), colour = "red", size = BoxPlotPointSize, alpha = BoxPlotPointAlpha)
                    }} +
                    {if (is.not.null(BoxplotPointsColourVar)) scale_colour_gradient(low = "#FF0000", high = "#0000FF", limits = c(min(BoxplotPointsColourVar), max(BoxplotPointsColourVar))) else NULL} +
                    {if (is.not.null(BoxplotPointsColourVar)) labs(colour = ColourVarName) else NULL}
                })
              names(StatInferNumGraphsPerGroup[[NumVarName]]) <- GroupByGroups
            }
          }
        }

        ###########################################################################
        ### Creating the Categorical Indep. vars VS Categorical Dep. Var Graphs ###
        ###########################################################################
        StatInferCatGraphsColNames <- NonNumericDSColNames[NonNumericDSColNames %notin% DependentVar] %>% setdiff(c("DOY", "DOM"))
        if (NROW(StatInferCatGraphsColNames) > 0) {
          StatInferCatGraphs <-
            lapply(StatInferCatGraphsColNames, function(CatVarName) {
              # ggplot(data = NonNumericDS, aes(x = !!sym(DependentVar), y = !!sym(CatVarName))) +
              #   geom_jitter(color = "#0c4c8a", na.rm = TRUE)
              ggplot(NonNumericDS, aes(x = !!sym(DependentVar), fill = !!sym(CatVarName))) + #If fill = DependentVar and DependentVar is Date, then there's a problem
                geom_bar(position = "fill") +
                geom_text(data = NonNumericDS %>%
                            group_by(!!sym(DependentVar)) %>%
                            summarise(number_cases = n()),
                          aes(x = !!sym(DependentVar), y = 0.05, label = number_cases),
                          size = 5, colour = "white", inherit.aes = FALSE) +
                ylab("")
            })
          names(StatInferCatGraphs) <- StatInferCatGraphsColNames

          if (ShowGraphs) {
            #Categorical Statistical Inference Graphs
            grid.arrange(grobs = lapply(StatInferCatGraphsColNames, function(CatVarName) {
              ggplotGrob(StatInferCatGraphs[[CatVarName]])
            }),
            nrow = round(sqrt(NROW(StatInferCatGraphsColNames))))
          }


          #===============#
          #== Per Group ==#
          #===============#
          if (is.not.null(GroupBy) & NROW(StatInferCatGraphsColNames) > 1) {
            for (CatVarName in StatInferCatGraphsColNames[StatInferCatGraphsColNames %notin% c(DependentVar, GroupBy)]) {
              StatInferCatGraphsPerGroup[[CatVarName]] <-
                lapply(GroupByGroups, function(CurGroup) {
                  # ggplot(data = NonNumericDS %>% filter(CurGroupFilterIndx[[CurGroup]]), aes(x = !!sym(DependentVar), y = !!sym(CatVarName))) +
                  #   geom_jitter(color = "#0c4c8a", na.rm = TRUE)
                  ggplot(NonNumericDS %>% filter(CurGroupFilterIndx[[CurGroup]]), aes(x = !!sym(DependentVar), fill = !!sym(CatVarName))) + #If fill = DependentVar and DependentVar is Date, then there's a problem
                    geom_bar(aes(color = !!sym(DependentVar)), position = "fill") +
                    geom_text(data = NonNumericDS %>% filter(CurGroupFilterIndx[[CurGroup]]) %>%
                                group_by(!!sym(DependentVar)) %>%
                                summarise(number_cases = n()),
                              aes(x = !!sym(DependentVar), y = 0.05, label = number_cases),
                              size = 5, colour = "white", inherit.aes = FALSE) +
                    ggtitle(paste0(GroupBy, ": ", CurGroup)) +
                    ylab("")
                })
              names(StatInferCatGraphsPerGroup[[CatVarName]]) <- GroupByGroups
            }
          }
        }

      } #DependentVar is What
    } #There actually is a Dependent Var (so we do Statistical Inference)


    ###################
    ### Time Series ###
    ###################
    if (IsTimeSeries) {

      if (Verbose) cat(toString(now()), "Building Time Series Plots\n")

      TimeseriesDS <- NumericDS %>% select(-one_of(ExcludeFromTimeseries)) #No matter what, if it's time series then the starting point is the NumericDS

      if (is.not.null(DependentVar) && NROW(DependentVar) > 0 && (DependentVar %in% NonNumericDSColNames)) { #Column-Binding the Dependent Variable if it's not already in NumericDS (it's Categorical)
        TimeseriesDS %<>%
          add_column(
            NonNumericDS %>% select(one_of(DependentVar)) %>% rename(!!sym(paste0(DependentVar, ".")) := !!sym(DependentVar))
          )
      }

      if (is.not.null(GroupBy)) TimeseriesDS %<>% group_by(Grp = NonNumericDS[[GroupBy]] %>% as.factor())
      if (is.not.null(TimeFlowVar)) {
        if (is.not.null(TimeFlowVarName)) {
          TimeseriesDS[[TimeFlowVarName]] <- TimeFlowVar[FilteredIndx]
        } else {
          TimeFlowVarName <- "TimeFlow"
          TimeseriesDS$TimeFlow <- TimeFlowVar[FilteredIndx]
        }
      } else {
        TimeseriesDS %<>% mutate(TimeFlow = 1:n())
      }

      TimeProgressionPlots <-
        lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
          CurPlot <- ggplot(data = TimeseriesDS, aes(x = !!sym(TimeFlowVarName), y = !!sym(NumVarName))) +
            geom_point(size = 0.5, alpha = 0.6) +
            geom_line(size = 0.1, color = "black", alpha = 0.3)

          if (is.not.null(DependentVar) && DependentVar %in% NonNumericDSColNames) { #Factor should also work for colour as a set instead of gradient.
            CurPlot <- CurPlot + aes(color = !!sym(paste0(DependentVar, "."))) #If an error occurs, then don't throw it away, see how to fix it; it's possible.
          } else if (is.not.null(DependentVar) && (DependentVar %in% (NumericDS %>% names()))) {
            CurPlot <- CurPlot + aes(color = !!sym(DependentVar))
          } else if (is.not.null(BoxplotPointsColourVar)) {
            CurPlot <- CurPlot + aes(color = BoxplotPointsColourVar)
          }

          if ((is.not.null(DependentVar) && DependentVar %notin% NonNumericDSColNames) | (is.null(DependentVar) && is.not.null(BoxplotPointsColourVar))) {
            CurPlot <- CurPlot +
              scale_colour_gradientn(colours = c("red", "green", "blue")) +
              theme_minimal()
          }

          if (is.not.null(GroupBy)) {CurPlot <- CurPlot + facet_grid(rows = vars(Grp)) + ylab(paste0(NumVarName, " (per ", GroupBy, ")"))}

          return(CurPlot)
        })

      names(TimeProgressionPlots) <- NumericDSColNames %>% setdiff(ExcludeFromTimeseries)

      if (ShowGraphs) {
        grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(VarName) {
          ggplotGrob(TimeProgressionPlots[[VarName]])
        }),
        nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)))))
      }


      #== Removing Groupping variable ==#
      TimeseriesDS %<>% ungroup()
      #=================================#


      if (Verbose) cat(toString(now()), "Building AutoCorrelations Plots. TimeseriesMaxLag=", TimeseriesMaxLag, "\n")
      AutoCorrelationsPlots <-
        lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
          tryCatch({
            CurAutoCorPlot <- forecast::ggAcf(
              TimeseriesDS %>% pull(NumVarName),
              lag.max = TimeseriesMaxLag,
              type = "correlation",
              plot = TRUE,
              na.action = na.contiguous,
              demean = TRUE
            )
            if ((CurAutoCorPlot$data %>% NROW()) == 0) {
              stop("Probably not enough data to plot")
            }
            CurAutoCorPlot <- CurAutoCorPlot + ggtitle(paste0("Autocorrelation: ", NumVarName))
            return(CurAutoCorPlot)

          }, error = function(e) {
            CurAutoCorPlot <- ggplot() + ggtitle(paste0(NumVarName, ": Probably not enough data to plot"), subtitle = e)
            return(CurAutoCorPlot)
          })
        })
      names(AutoCorrelationsPlots) <- NumericDSColNames %>% setdiff(ExcludeFromTimeseries)

      if (ShowGraphs) {
        grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(VarName) {
          ggplotGrob(AutoCorrelationsPlots[[VarName]])
        }),
        nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)))))
      }


      if (Verbose) cat(toString(now()), "Building Partial AutoCorrelations Plots. TimeseriesMaxLag=", TimeseriesMaxLag, "\n")
      PartialAutoCorrelationsPlots <-
        lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
          tryCatch({
            CurPartAutoCorPlot <- forecast::ggAcf(
              TimeseriesDS %>% pull(NumVarName),
              lag.max = TimeseriesMaxLag,
              type = "partial",
              plot = TRUE,
              na.action = na.contiguous,
              demean = TRUE
            )
            if ((CurPartAutoCorPlot$data %>% NROW()) == 0) {
              stop("Probably not enough data to plot")
            }
            CurPartAutoCorPlot <- CurPartAutoCorPlot + ggtitle(paste0("Partial Autocorrelation: ", NumVarName))
            return(CurPartAutoCorPlot)

          }, error = function(e) {
            CurPartAutoCorPlot <- ggplot() + ggtitle(paste0(NumVarName, ": Probably not enough data to plot"), subtitle = e)
            return(CurPartAutoCorPlot)
          })
        })
      names(PartialAutoCorrelationsPlots) <- NumericDSColNames %>% setdiff(ExcludeFromTimeseries)

      if (ShowGraphs) {
        grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(VarName) {
          ggplotGrob(PartialAutoCorrelationsPlots[[VarName]])
        }),
        nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)))))
      }


      if (!ExludeCovariances) {
        if (Verbose) cat(toString(now()), "Building AutoCovariance Plots. TimeseriesMaxLag=", TimeseriesMaxLag, "\n")
        AutoCovariancePlots <-
          lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
            tryCatch({
              CurACF <- forecast::Acf(
                TimeseriesDS %>% pull(NumVarName),
                type = "covariance",
                lag.max = TimeseriesMaxLag,
                plot = FALSE,
                na.action = na.contiguous,
                demean = TRUE
              )
              if ((CurACF$acf %>% NROW()) == 0) {
                stop("Probably not enough data to plot")
              }
              CurAutoCovPlot <-
                ((CurACF$acf[1:NROW(CurACF$lag), 1, 1] %>% enframe(name = NULL, value = NumVarName)) %>%
                   ggplot() +
                   geom_col(aes(x = CurACF$lag, y = !!sym(NumVarName)), width = 0.1, na.rm = FALSE) +
                   xlab("Lag") +
                   ylab("ACF (cov)") +
                   theme_minimal() +
                   theme(
                     axis.text.x = element_text(size = 14),
                     axis.text.y = element_text(size = 14, angle = 90),
                     panel.border = element_rect(colour = "black", fill = NA, size = 1)#,
                     # plot.title = element_text(hjust = 0.5) #Centre the title
                   )) + ggtitle(paste0("Autocovariance: ", NumVarName))
              return(CurAutoCovPlot)

            }, error = function(e) {
              CurAutoCovPlot <- ggplot() + ggtitle(paste0(NumVarName, ": Probably not enough data to plot"), subtitle = e)
              return(CurAutoCovPlot)
            })
          })
        names(AutoCovariancePlots) <- NumericDSColNames %>% setdiff(ExcludeFromTimeseries)

        if (ShowGraphs) {
          grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(VarName) {
            ggplotGrob(AutoCovariancePlots[[VarName]])
          }),
          nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)))))
        }
      }

      if (!ExcludeTaperedAutocor) {
        if (NROW(TimeseriesDS) > MaxTaperedRows) message(paste0("Tapered Partial and Autocorrelations will use the ", MaxTaperedRows, " last non-NA rows so that the code can run in a reasonable time"))

        if (Verbose) cat(toString(now()), "Building Tapered AutoCorrelations Plots. TimeseriesMaxLag=", TimeseriesMaxLag, "\n")
        #The tapered versions implement the ACF and PACF estimates and plots described in Hyndman (2015), based on the banded and tapered estimates of autocovariance proposed by McMurry and Politis (2010).
        TaperedAutoCorrelationsPlots <-
          lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
            tryCatch({
              CurTapAutoCorPlot <- forecast::ggtaperedacf(
                TimeseriesDS %>% select(all_of(NumVarName)) %>% drop_na() %>% slice_tail(n = MaxTaperedRows) %>% pull(NumVarName),
                lag.max = TimeseriesMaxLag,
                type = c("correlation"),
                plot = TRUE,
                calc.ci = NROW(TimeseriesDS) <= 1000,
                level = (1-SignificanceLevel)*100,
                nsim = ifelse(NROW(TimeseriesDS) <= 500, 100, 100)
              )
              #Unfortunately its '$data' doesn't contain the data so we can't check for NROW==0
              #This might lead to errors not captured by the tryCatch and spiral into errors of saving or even not finishing running this function
              # if ((CurTapAutoCorPlot$data %>% NROW()) == 0) {
              #   stop("Probably not enough data to plot")
              # }
              CurTapAutoCorPlot <- CurTapAutoCorPlot + ggtitle(paste0("Tapered Autocorrelation: ", NumVarName))
              return(CurTapAutoCorPlot)

            }, error = function(e) {
              CurTapAutoCorPlot <- ggplot() + ggtitle(paste0(NumVarName, ": Probably not enough data to plot"), subtitle = e)
              return(CurTapAutoCorPlot)
            })
          })
        names(TaperedAutoCorrelationsPlots) <- NumericDSColNames %>% setdiff(ExcludeFromTimeseries)

        if (ShowGraphs) {
          grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(VarName) {
            ggplotGrob(TaperedAutoCorrelationsPlots[[VarName]])
          }),
          nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)))))
        }


        if (Verbose) cat(toString(now()), "Building Tapered Partial AutoCorrelations Plots\n")
        #The tapered versions implement the ACF and PACF estimates and plots described in Hyndman (2015), based on the banded and tapered estimates of autocovariance proposed by McMurry and Politis (2010).
        TaperedPartialAutoCorrelationsPlots <-
          lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
            tryCatch({
              CurTapPAutoCorPlot <- forecast::ggtaperedacf(
                TimeseriesDS %>% select(NumVarName) %>% drop_na() %>% slice_tail(n = MaxTaperedRows) %>% pull(NumVarName),
                lag.max = TimeseriesMaxLag,
                type = c("partial"),
                plot = TRUE,
                calc.ci = NROW(TimeseriesDS) <= 1000,
                level = (1-SignificanceLevel)*100,
                nsim = ifelse(NROW(TimeseriesDS) <= 500, 100, 100)
              )
              #Unfortunately its '$data' doesn't contain the data so we can't check for NROW==0
              #This might lead to errors not captured by the tryCatch and spiral into errors of saving or even not finishing running this function
              # if ((CurTapPAutoCorPlot$data %>% NROW()) == 0) {
              #   stop("Probably not enough data to plot")
              # }
              CurTapPAutoCorPlot <- CurTapPAutoCorPlot + ggtitle(paste0("Tapered Autocovariance: ", NumVarName))
              return(CurTapPAutoCorPlot)

            }, error = function(e) {
              CurTapPAutoCorPlot <- ggplot() + ggtitle(paste0(NumVarName, ": Probably not enough data to plot"), subtitle = e)
              return(CurTapPAutoCorPlot)
            })
          })
        names(TaperedPartialAutoCorrelationsPlots) <- NumericDSColNames %>% setdiff(ExcludeFromTimeseries)

        if (ShowGraphs) {
          grid.arrange(grobs = lapply(NumericDSColNames %>% setdiff(ExcludeFromTimeseries), function(VarName) {
            ggplotGrob(TaperedPartialAutoCorrelationsPlots[[VarName]])
          }),
          nrow = round(sqrt(NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)))))
        }
      }

      #== Crosscorrelations for Versus Numerical Dependent ==#
      CrossCorrelationPlots <- NULL
      CrossCovariancePlots <- NULL
      if (is.not.null(DependentVar) && NROW(NumericDSColNames %>% setdiff(ExcludeFromTimeseries)) > 0 && DependentVar %in% NumericDSColNames %>% setdiff(ExcludeFromTimeseries)) {
        if (Verbose) cat(toString(now()), "Building Cross-Correlation Plots\n")

          CrossCorrelationPlots <-
            lapply(StatInferNumGraphsColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
              tryCatch({
                CurCrossCorPlot <- forecast::ggCcf(
                  TimeseriesDS %>% pull(NumVarName),
                  TimeseriesDS %>% pull(DependentVar),
                  lag.max = TimeseriesMaxLag,
                  type = c("correlation"),
                  plot = TRUE,
                  na.action = na.contiguous
                )
                if ((CurCrossCorPlot$data %>% NROW()) == 0) {
                  stop("Probably not enough data to plot")
                }
                CurCrossCorPlot <- CurCrossCorPlot + ggtitle(paste0(NumVarName, " VS ", DependentVar))
                return(CurCrossCorPlot)

              }, error = function(e) {
                CurCrossCorPlot <- ggplot() + ggtitle(paste0(NumVarName, ": Probably not enough data to plot"), subtitle = e)
                return(CurCrossCorPlot)
              })
            })
          names(CrossCorrelationPlots) <- StatInferNumGraphsColNames %>% setdiff(ExcludeFromTimeseries)

          if (ShowGraphs) {
            grid.arrange(grobs = lapply(StatInferNumGraphsColNames %>% setdiff(ExcludeFromTimeseries), function(VarName) {
              ggplotGrob(CrossCorrelationPlots[[VarName]])
            }),
            nrow = round(sqrt(NROW(StatInferNumGraphsColNames %>% setdiff(ExcludeFromTimeseries)))))
          }

          if (!ExludeCovariances) {
            if (Verbose) cat(toString(now()), "Building Cross-Covariance Plots\n")
            CrossCovariancePlots <-
              lapply(StatInferNumGraphsColNames %>% setdiff(ExcludeFromTimeseries), function(NumVarName) {
                tryCatch({
                  CurCrossCovPlot <- forecast::ggCcf(
                    TimeseriesDS %>% pull(NumVarName),
                    TimeseriesDS %>% pull(DependentVar),
                    lag.max = TimeseriesMaxLag,
                    type = c("covariance"),
                    plot = TRUE,
                    na.action = na.contiguous
                  )
                  if ((CurCrossCovPlot$data %>% NROW()) == 0) {
                    stop("Probably not enough data to plot")
                  }
                  CurCrossCovPlot <- CurCrossCovPlot + ggtitle(paste0(NumVarName, " VS ", DependentVar))
                  return(CurCrossCovPlot)
                }, error = function(e) {
                  CurCrossCovPlot <- ggplot() + ggtitle(paste0(NumVarName, ": Probably not enough data to plot"), subtitle = e)
                  return(CurCrossCovPlot)
                })
              })
            names(CrossCovariancePlots) <- StatInferNumGraphsColNames %>% setdiff(ExcludeFromTimeseries)

            if (ShowGraphs) {
              grid.arrange(grobs = lapply(StatInferNumGraphsColNames %>% setdiff(ExcludeFromTimeseries), function(VarName) {
                ggplotGrob(CrossCovariancePlots[[VarName]])
              }),
              nrow = round(sqrt(NROW(StatInferNumGraphsColNames %>% setdiff(ExcludeFromTimeseries)))))
            }
          }

      } #/Crosscorrelations for Versus Numerical Dependent

    } #/IsTimeSeries

  } #/CalculateGraphs

  return(
    list(
      PerGroupDescrStats = PerGroupDescrStats,
      NumericDescriptives = NumericDescriptives,
      CategoricalDescriptives = CategoricalDescriptives,
      NumericDistrGraphs = NumericDistrGraphs,
      NumericDistrGraphsPerGroup = NumericDistrGraphsPerGroup,
      BoxplotGraphs = BoxplotGraphs,
      BoxplotGraphsPerGroup = BoxplotGraphsPerGroup,
      BarChartGraphs = BarChartGraphs,
      BarChartGraphsPerGroup = BarChartGraphsPerGroup,
      PearsonCor = PearsonCor,
      PearsonCorOrdered = PearsonCorOrdered,
      PearsonCorPVal = PearsonCorPVal,
      PearsonCorOrderedPVal = PearsonCorOrderedPVal,
      PearsonCorPlot = PearsonCorPlot,
      PearsonCorOrderedPlot = PearsonCorOrderedPlot,
      SpearmanCor = SpearmanCor,
      SpearmanCorOrdered = SpearmanCorOrdered,
      SpearmanCorPVal = SpearmanCorPVal,
      SpearmanCorOrderedPVal = SpearmanCorOrderedPVal,
      SpearmanCorPlot = SpearmanCorPlot,
      SpearmanCorOrderedPlot = SpearmanCorOrderedPlot,
      NumericVSDependentGraphs = StatInferNumGraphs,
      NumericVSDependentGraphsPerGroup = StatInferNumGraphsPerGroup,
      CategoricalVSDependentGraphs = StatInferCatGraphs,
      CategoricalVSDependentGraphsPerGroup = StatInferCatGraphsPerGroup,
      TimeProgressionPlots = TimeProgressionPlots,
      AutoCorrelationsPlots = AutoCorrelationsPlots,
      PartialAutoCorrelationsPlots = PartialAutoCorrelationsPlots,
      AutoCovariancePlots = AutoCovariancePlots,
      TaperedAutoCorrelationsPlots = TaperedAutoCorrelationsPlots,
      TaperedPartialAutoCorrelationsPlots = TaperedPartialAutoCorrelationsPlots,
      CrossCorrelationPlots = CrossCorrelationPlots,
      CrossCovariancePlots = CrossCovariancePlots
    )
  )
} #/DescriptiveStats
