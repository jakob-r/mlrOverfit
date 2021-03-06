#' @title Plots the simulated results
#' @description Plots the result of \code{\link{simulateOuterPerformance}}
#'
#' @param x [\code{\link{SimulateOuterPerformanceResult}}]
#' @param resample.overfit [\code{\link{ResampleOverfitResult}}]
#'  If supplied boxplots for the untuned and the tuned performances will be shown.
#' @param ... Not used.
#' @export
plot.SimulateOuterPerformanceResult = function(x, resample.overfit = NULL, ...) {
  outer.performance = x
  measure.vars = unlist(outer.performance[c("y.inner.name", "y.outer.name", "cum.y.inner.name", "cum.y.outer.name", "sim.y.outer.name")])
  summary.vars = unlist(outer.performance[c("cum.y.inner.name", "cum.y.outer.name", "sim.y.outer.name")])

  assertClass(resample.overfit, "ResampleOverfitResult", null.ok = TRUE)

  # what does each value mean. What is shown in the legend
  translate = c(y.inner.name = "training",
                y.outer.name = "test",
                cum.y.inner.name = "training",
                cum.y.outer.name = "hypothetical test",
                sim.y.outer.name = "test")
  names(translate) = unlist(outer.performance[names(translate)])

  data = outer.performance$data
  mdata = melt(data, measure.vars = measure.vars)
  mdata[, ':='(do.summary = get("variable") %in% summary.vars), ]

  # renaming stuff
  mdata[, "variable" := revalue(get("variable"), replace = translate, warn_missing = FALSE)]

  # data for boxplots
  if (!is.null(resample.overfit)) {
    data.boxplots = rbind(
      data.table(value = resample.overfit$tuning.resampled$measures.test[,2], dob = max(data$dob) + 1, variable = outer.performance$y.outer.name),
      data.table(value = resample.overfit$untuned.resampled$measures.test[,2], dob = 0, variable = "untuned"))
    # renaming stuff
    data.boxplots[, "variable" := revalue(get("variable"), replace = translate, warn_missing = FALSE)]
  }

  g = ggplot(mapping = aes_string(x = "dob", y = "value", color = "variable"))
  g = g + stat_summary(data = mdata[get("do.summary") == TRUE, ], fun = median, geom="line")
  g = g + geom_point(data = mdata[get("do.summary") == FALSE, ], alpha = 0.2)
  if (!is.null(resample.overfit)) {
    g = g + geom_boxplot(data = data.boxplots, mapping = aes_string(x = "dob", y = "value"), alpha = 0.5, width = 1)
  }
  g = g + scale_color_discrete(name = "Performance")
  g = g + ylab(outer.performance$measures[[1]]$name) + xlab("Date of Birth")
  g
}

#' @title Plots the simulated results
#' @description Plots the result of \code{\link{simulateOuterPerformance}}
#'
#' @param x [\code{\link{OverfitAnalysisResult}}]
#' @param ... Not used.
#' @export
plot.OverfitAnalysisResult = function(x, ...) {
  plot(x$simulated.outer.performance, x$resample.overfit, ...)
}