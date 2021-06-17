#' Saves all the components of the output of DescriptiveStats() to a specified folder
#'
#' Gets the output of DescriptiveStats() and no matter what options were selected, saves in a specified folder all the valid Statistics
#' Saving the plots, one might also choose different sizes for the Categorical plots, Numerical ones, and Timeseries as well.
#'
#' @param DescriptiveStatsVar A list. The output of DescriptiveStats() on a dataset
#' @param path_stats A string. The relative or full path of a folder (existing or to be created) where all the matrices and plots will be saved on
#' @param NumWidth An Integer. Width (in pixels) for Numerical Variables' plots
#' @param NumHeight An Integer. Height (in pixels) for Numerical Variables' plots
#' @param CatWidth An Integer. Width (in pixels) for Categorical Variables' plots
#' @param CatHeight An Integer. Height (in pixels) for Categorical Variables' plots
#' @param TimeSeriesWidth An Integer. Width (in pixels) for the Timeseries plot
#' @param TimeSeriesHeight An Integer. Height (in pixels) for the Timeseries plot
#' @param CorrelationsWidth An Integer. Width (in pixels) for the Correlation plots
#' @param CorrelationsHeight An Integer. Height (in pixels) for the Correlation plots
#' @keywords Save Descriptive Statistics SaveDescrStats
#' @export
#' @examples
#' DS <- mtcars %>% mutate(vs = as.factor(vs), am = as.factor(am), gear = as.factor(gear), carb = as.factor(carb)) %>% as_tibble()
#'
#' #Creating the Variable which holds all the Matrices and Plots
#' MTCarsStats <- DS %>% DescriptiveStats(CalculateGraphs = TRUE, DependentVar = "mpg", IsTimeSeries = TRUE, GroupBy = "gear", CorrVarOrder = "PCA")
#'
#' #Saving everything to a folder on our Hard Drive
#' MTCarsStats %>% SaveDescrStats("MTCarsFolder")
SaveDescrStats <- function(DescriptiveStatsVar, path_stats, NumWidth = 1300, NumHeight = 800, CatWidth = 1300, CatHeight = 800, TimeSeriesWidth = NULL, TimeSeriesHeight = NULL, CorrelationsWidth = NULL, CorrelationsHeight = NULL) {
  if (!dir.exists(path_stats)) {
    if (endsWith(path_stats, "/") | endsWith(path_stats, "\\")) path_stats <- substr(path_stats, 1, (nchar(path_stats)-1))

    LastPath <- GetFileNamesAlone(path_stats)
    ValidLastPath <- make.names(LastPath)
    if (endsWith(ValidLastPath, ".")) ValidLastPath <- paste0(substr(ValidLastPath, 1, (nchar(ValidLastPath)-1)), "_")
    path_stats <- paste0(substr(path_stats, 1, nchar(path_stats)-nchar(LastPath)), ValidLastPath, "/")

    # if (!grepl("/", path_stats, fixed = TRUE) && !grepl("\\", path_stats, fixed = TRUE)) {
    #   path_stats <- paste0(make.names(path_stats))
    #   if (endsWith(path_stats, ".")) path_stats <- paste0(substr(path_stats, 1, (nchar(path_stats)-1)), "_")
    #   path_stats <- paste0(getwd(), "/", path_stats, "/")
    # }

    tmp <- file.path(GetParentDir(GetParentDir(path_stats)), GetFileNamesAlone(GetParentDir(path_stats)))
    if (!dir.exists(tmp)) try(dir.create(tmp, showWarnings = FALSE))
    tmp <- file.path(GetParentDir(path_stats), GetFileNamesAlone(path_stats))
    if (!dir.exists(tmp)) try(dir.create(file.path(GetParentDir(path_stats), GetFileNamesAlone(path_stats)), showWarnings = FALSE))
    tmp <- file.path(path_stats)
    if (!dir.exists(tmp)) try(dir.create(file.path(path_stats), showWarnings = FALSE))

  }

  if (is.not.null(DescriptiveStatsVar$PerGroupDescrStats)) {
    for (CurGroup in names(DescriptiveStatsVar$PerGroupDescrStats)) {
      SaveDescrStats(DescriptiveStatsVar$PerGroupDescrStats[[CurGroup]],
                       path_stats = paste0(file.path(path_stats, "Per Group/", CurGroup), "/"),
                       NumWidth = NumWidth,
                       NumHeight = NumHeight,
                       CatWidth = CatWidth,
                       CatHeight = CatHeight,
                       TimeSeriesWidth = TimeSeriesWidth,
                       TimeSeriesHeight = TimeSeriesHeight
      )
    }
  }

  if (Right(path_stats, 1) != "/" & Right(path_stats, 1) != "\\") path_stats <- paste0(path_stats, "/")
  if (is.null(TimeSeriesWidth)) TimeSeriesWidth <- NumWidth * 1.5
  if (is.null(TimeSeriesHeight)) TimeSeriesHeight <- NumHeight * 1.25

  if (is.null(CorrelationsWidth)) CorrelationsWidth <- NumHeight * 0.65
  if (is.null(CorrelationsHeight)) CorrelationsHeight <- NumHeight * 0.65

  if(NROW(DescriptiveStatsVar$NumericDescriptives) > 0) write.csv(DescriptiveStatsVar$NumericDescriptives, paste0(path_stats, "Numerical Descriptives.csv"), row.names = FALSE)
  if(is.not.null(DescriptiveStatsVar$CategoricalDescriptives)) write.csv(DescriptiveStatsVar$CategoricalDescriptives, paste0(path_stats, "Categorical Descriptives.csv"), row.names = FALSE)

  if(is.not.null(DescriptiveStatsVar$PearsonCor)) write.csv(DescriptiveStatsVar$PearsonCor, paste0(path_stats, "Pearson Cor.csv"), row.names = FALSE)
  if (is.not.null(DescriptiveStatsVar$PearsonCorPVal)) write.csv(DescriptiveStatsVar$PearsonCorPVal, paste0(path_stats, "Pearson Cor p-Value.csv"), row.names = FALSE)
  if (is.not.null(DescriptiveStatsVar$PearsonCorOrdered)) write.csv(DescriptiveStatsVar$PearsonCorOrdered, paste0(path_stats, "Pearson Cor Ordered.csv"), row.names = FALSE)
  if (is.not.null(DescriptiveStatsVar$PearsonCorOrderedPVal)) write.csv(DescriptiveStatsVar$PearsonCorOrderedPVal, paste0(path_stats, "Pearson Cor Ordered p-Value.csv"), row.names = FALSE)

  if (is.not.null(DescriptiveStatsVar$PearsonCorPlot)) {
    tryCatch({
      png(paste0(path_stats, "Pearson Cor.png"), width = CorrelationsWidth, height = CorrelationsHeight, units = "px")
      print(DescriptiveStatsVar$PearsonCorPlot)
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  if (is.not.null(DescriptiveStatsVar$PearsonCorOrderedPlot)) {
    tryCatch({
      png(paste0(path_stats, "Pearson Cor Ordered.png"), width = CorrelationsWidth, height = CorrelationsHeight, units = "px")
      print(DescriptiveStatsVar$PearsonCorOrderedPlot)
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  if (is.not.null(DescriptiveStatsVar$SpearmanCor)) write.csv(DescriptiveStatsVar$SpearmanCor, paste0(path_stats, "Spearman Cor.csv"), row.names = FALSE)
  if (is.not.null(DescriptiveStatsVar$SpearmanCorPVal)) write.csv(DescriptiveStatsVar$SpearmanCorPVal, paste0(path_stats, "Spearman Cor p-value.csv"), row.names = FALSE)
  if (is.not.null(DescriptiveStatsVar$SpearmanCorOrdered)) write.csv(DescriptiveStatsVar$SpearmanCorOrdered, paste0(path_stats, "Spearman Cor Ordered.csv"), row.names = FALSE)
  if (is.not.null(DescriptiveStatsVar$SpearmanCorOrderedPVal)) write.csv(DescriptiveStatsVar$SpearmanCorOrderedPVal, paste0(path_stats, "Spearman Cor Ordered p-Value.csv"), row.names = FALSE)

  if (is.not.null(DescriptiveStatsVar$SpearmanCorPlot)) {
    tryCatch({
      png(paste0(path_stats, "Spearman Cor.png"), width = CorrelationsWidth, height = CorrelationsHeight, units = "px")
      print(DescriptiveStatsVar$SpearmanCorPlot)
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  if (is.not.null(DescriptiveStatsVar$SpearmanCorOrderedPlot)) {
    tryCatch({
      png(paste0(path_stats, "Spearman Cor Ordered.png"), width = CorrelationsWidth, height = CorrelationsHeight, units = "px")
      print(DescriptiveStatsVar$SpearmanCorOrderedPlot)
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  #-- Looking at distributions --#
  ### Histograms and Density of Numerical Variables
  if (NROW(DescriptiveStatsVar$NumericDistrGraphs) > 0) {
    tryCatch({
      png(paste0(path_stats, "Numerical Distrubutions.png"), width = NumWidth, height = NumHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$NumericDistrGraphs), function(x) {
          ggplotGrob(DescriptiveStatsVar$NumericDistrGraphs[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$NumericDistrGraphs)))))
      )
    }, warning = function(w) {
      cat("Warning:\n")
      print(w)
    }, error = function(e) {
      cat("Error:\n")
      print(e)
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }
  if (NROW(DescriptiveStatsVar$NumericDistrGraphsPerGroup) > 0) {
    PerGroupNames <- names(DescriptiveStatsVar$NumericDistrGraphsPerGroup)
    for (CurName in PerGroupNames) {
      try(dir.create(file.path(path_stats, "Numerical Distrubutions Per Group"), showWarnings = FALSE))
      tryCatch({
        png(paste0(path_stats, "Numerical Distrubutions Per Group/", CurName, ".png"), width = NumWidth, height = NumHeight, units = "px")
        suppressWarnings(
          grid.arrange(grobs = lapply(names(DescriptiveStatsVar$NumericDistrGraphsPerGroup[[CurName]]), function(x) {
            ggplotGrob(DescriptiveStatsVar$NumericDistrGraphsPerGroup[[CurName]][[x]])
          }),
          nrow = round(sqrt(NROW(names(DescriptiveStatsVar$NumericDistrGraphsPerGroup[[CurName]])))))
        )
      }, warning = function(w) {
        cat("Warning:\n")
        print(w)
      }, error = function(e) {
        cat("Error:\n")
        print(e)
      }, finally = {
        try(dev.off(), silent = TRUE)
      })
    }
  }

  ### Boxplot for Numerical Variables
  if (NROW(DescriptiveStatsVar$BoxplotGraphs) > 0) {
    tryCatch({
      png(paste0(path_stats, "Numerical Boxplots.png"), width = NumWidth, height = NumHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$BoxplotGraphs), function(x) {
          ggplotGrob(DescriptiveStatsVar$BoxplotGraphs[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$BoxplotGraphs)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }
  if (NROW(DescriptiveStatsVar$BoxplotGraphsPerGroup) > 0) {
    PerGroupNames <- names(DescriptiveStatsVar$BoxplotGraphsPerGroup)
    for (CurName in PerGroupNames) {
      try(dir.create(file.path(path_stats, "Boxplot Graphs Per Group"), showWarnings = FALSE))
      tryCatch({
        png(paste0(path_stats, "Boxplot Graphs Per Group/", CurName, ".png"), width = NumWidth, height = NumHeight, units = "px")
        print(DescriptiveStatsVar$BoxplotGraphsPerGroup[[CurName]])
      }, warning = function(w) {
        cat("Warning:\n")
        print(w)
      }, error = function(e) {
        cat("Error:\n")
        print(e)
      }, finally = {
        try(dev.off(), silent = TRUE)
      })
    }
  }

  ### Bar Charts for Categorical Variables
  if (NROW(DescriptiveStatsVar$BarChartGraphs) > 0) {
    tryCatch({
      png(paste0(path_stats, "Categorical Distributions.png"), width = CatWidth, height = CatHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$BarChartGraphs)[names(DescriptiveStatsVar$BarChartGraphs) %notin% "Participant"], function(x) {
          ggplotGrob(DescriptiveStatsVar$BarChartGraphs[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$BarChartGraphs)[names(DescriptiveStatsVar$BarChartGraphs) %notin% "Participant"]))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }
  if (NROW(DescriptiveStatsVar$BarChartGraphsPerGroup) > 0) {
    PerGroupNames <- names(DescriptiveStatsVar$BarChartGraphsPerGroup)
    for (CurName in PerGroupNames) {
      try(dir.create(file.path(path_stats, "Categorical Distributions Per Group"), showWarnings = FALSE))
      tryCatch({
        png(paste0(path_stats, "Categorical Distributions Per Group/", CurName, ".png"), width = CatWidth, height = CatHeight, units = "px")
        print(DescriptiveStatsVar$BarChartGraphsPerGroup[[CurName]])
      }, warning = function(w) {
        cat("Warning:\n")
        print(w)
      }, error = function(e) {
        cat("Error:\n")
        print(e)
      }, finally = {
        try(dev.off(), silent = TRUE)
      })
    }
  }

  if (NROW(DescriptiveStatsVar$NumericVSDependentGraphs) > 0) {
    ### Numerical Variables VS the Dependent one
    tryCatch({
      png(paste0(path_stats, "Numerical VS Dependent.png"), width = NumWidth, height = NumHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$NumericVSDependentGraphs), function(x) {
          ggplotGrob(DescriptiveStatsVar$NumericVSDependentGraphs[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$NumericVSDependentGraphs)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }
  if (NROW(DescriptiveStatsVar$NumericVSDependentGraphsPerGroup) > 0) {
    PerGroupNames <- names(DescriptiveStatsVar$NumericVSDependentGraphsPerGroup)
    for (CurName in PerGroupNames) {
      try(dir.create(file.path(path_stats, "Numerical VS Dependent Per Group"), showWarnings = FALSE))
      tryCatch({
        png(paste0(path_stats, "Numerical VS Dependent Per Group/", CurName, ".png"), width = NumWidth, height = NumHeight, units = "px")
        suppressWarnings(
          grid.arrange(grobs = lapply(names(DescriptiveStatsVar$NumericVSDependentGraphsPerGroup[[CurName]]), function(x) {
            ggplotGrob(DescriptiveStatsVar$NumericVSDependentGraphsPerGroup[[CurName]][[x]])
          }),
          nrow = round(sqrt(NROW(names(DescriptiveStatsVar$NumericVSDependentGraphsPerGroup[[CurName]])))))
        )
      }, warning = function(w) {
        cat("Warning:\n")
        print(w)
      }, error = function(e) {
        cat("Error:\n")
        print(e)
      }, finally = {
        try(dev.off(), silent = TRUE)
      })
    }
  }

  if (NROW(DescriptiveStatsVar$CategoricalVSDependentGraphs) > 0) {
    ### Categorical Variables VS the Dependent one
    tryCatch({
      png(paste0(path_stats, "Categorical VS Dependent.png"), width = CatWidth, height = CatHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$CategoricalVSDependentGraphs), function(x) {
          ggplotGrob(DescriptiveStatsVar$CategoricalVSDependentGraphs[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$CategoricalVSDependentGraphs)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }
  if (NROW(DescriptiveStatsVar$CategoricalVSDependentGraphsPerGroup) > 0) {
    PerGroupNames <- names(DescriptiveStatsVar$CategoricalVSDependentGraphsPerGroup)
    for (CurName in PerGroupNames) {
      try(dir.create(file.path(path_stats, "Categorical VS Dependent Per Group"), showWarnings = FALSE))
      tryCatch({
        png(paste0(path_stats, "Categorical VS Dependent Per Group/", CurName, ".png"), width = CatWidth, height = CatHeight, units = "px")
        suppressWarnings(
          grid.arrange(grobs = lapply(names(DescriptiveStatsVar$CategoricalVSDependentGraphsPerGroup[[CurName]]), function(x) {
            ggplotGrob(DescriptiveStatsVar$CategoricalVSDependentGraphsPerGroup[[CurName]][[x]])
          }),
          nrow = round(sqrt(NROW(names(DescriptiveStatsVar$CategoricalVSDependentGraphsPerGroup[[CurName]])))))
        )
      }, warning = function(w) {
        cat("Warning:\n")
        print(w)
      }, error = function(e) {
        cat("Error:\n")
        print(e)
      }, finally = {
        try(dev.off(), silent = TRUE)
      })
    }
  }

  ### Time Series Progression Plots
  if (NROW(DescriptiveStatsVar$TimeProgressionPlots) > 0) {
    tryCatch({
      png(paste0(path_stats, "Time Progression for Numerical Variables.png"), width = TimeSeriesWidth, height = TimeSeriesHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$TimeProgressionPlots), function(x) {
          ggplotGrob(DescriptiveStatsVar$TimeProgressionPlots[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$TimeProgressionPlots)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  ### Auto Correlations Plots
  if (NROW(DescriptiveStatsVar$AutoCorrelationsPlots) > 0) {
    tryCatch({
      png(paste0(path_stats, "Autocorrelations.png"), width = TimeSeriesWidth, height = TimeSeriesHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$AutoCorrelationsPlots), function(x) {
          ggplotGrob(DescriptiveStatsVar$AutoCorrelationsPlots[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$AutoCorrelationsPlots)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  ### Partial Auto Correlations Plots
  if (NROW(DescriptiveStatsVar$PartialAutoCorrelationsPlots) > 0) {
    tryCatch({
      png(paste0(path_stats, "Partial Autocorrelations.png"), width = TimeSeriesWidth, height = TimeSeriesHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$PartialAutoCorrelationsPlots), function(x) {
          ggplotGrob(DescriptiveStatsVar$PartialAutoCorrelationsPlots[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$PartialAutoCorrelationsPlots)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  ### Auto Covariance
  if (NROW(DescriptiveStatsVar$AutoCovariancePlots) > 0) {
    tryCatch({
      png(paste0(path_stats, "Autocovariances.png"), width = TimeSeriesWidth, height = TimeSeriesHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$AutoCovariancePlots), function(x) {
          ggplotGrob(DescriptiveStatsVar$AutoCovariancePlots[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$AutoCovariancePlots)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  ### Tapered Auto Correlations
  if (NROW(DescriptiveStatsVar$TaperedAutoCorrelationsPlots) > 0) {
    tryCatch({
      png(paste0(path_stats, "Tapered Autocorrelations.png"), width = TimeSeriesWidth, height = TimeSeriesHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$TaperedAutoCorrelationsPlots), function(x) {
          ggplotGrob(DescriptiveStatsVar$TaperedAutoCorrelationsPlots[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$TaperedAutoCorrelationsPlots)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }


  ### Tapered Partial Auto Correlations
  if (NROW(DescriptiveStatsVar$TaperedPartialAutoCorrelationsPlots) > 0) {
    tryCatch({
      png(paste0(path_stats, "Tapered Partial Autocorrelations.png"), width = TimeSeriesWidth, height = TimeSeriesHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$TaperedPartialAutoCorrelationsPlots), function(x) {
          ggplotGrob(DescriptiveStatsVar$TaperedPartialAutoCorrelationsPlots[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$TaperedPartialAutoCorrelationsPlots)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }


  ### Cross Correlation Plots
  if (NROW(DescriptiveStatsVar$CrossCorrelationPlots) > 0) {
    tryCatch({
      png(paste0(path_stats, "Cross Correlations with Dependent Variable.png"), width = TimeSeriesWidth, height = TimeSeriesHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$CrossCorrelationPlots), function(x) {
          ggplotGrob(DescriptiveStatsVar$CrossCorrelationPlots[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$CrossCorrelationPlots)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  ### Cross Covariance Plots
  if (NROW(DescriptiveStatsVar$CrossCovariancePlots) > 0) {
    tryCatch({
      png(paste0(path_stats, "Cross Covariances with Dependent Variable.png"), width = TimeSeriesWidth, height = TimeSeriesHeight, units = "px")
      suppressWarnings(
        grid.arrange(grobs = lapply(names(DescriptiveStatsVar$CrossCovariancePlots), function(x) {
          ggplotGrob(DescriptiveStatsVar$CrossCovariancePlots[[x]])
        }),
        nrow = round(sqrt(NROW(names(DescriptiveStatsVar$CrossCovariancePlots)))))
      )
    }, warning = function(w) {
    }, error = function(e) {
    }, finally = {
      try(dev.off(), silent = TRUE)
    })
  }

  return(paste0("Descriptive Statistics Results and Plots have been saved on: ", path_stats))
} #/SaveDescrStats
