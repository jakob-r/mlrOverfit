#' @title Plots the simulated results
#' @description Plots the result of \code{\link{simulateOuterTestError}}
#'
#' @param outer.performance [\code{\link{SimulateOuterTestErrorResult}}]
#' @param resample.overfit [\code{\link{ResampleOverfitResult}}]
#'  If supplied boxplots for the untuned and the tuned performances will be shown.
#' @export
plot.SimulateOuterTestErrorResult = function(outer.performance, resample.overfit = NULL) {
  measure.vars = unlist(outer.performance[c("y.inner.name", "y.outer.name", "cum.y.inner.name", "cum.y.outer.name", "sim.y.outer.name")])
  plot.vars = unlist(outer.performance[c("cum.y.inner.name", "cum.y.outer.name", "sim.y.outer.name")])
  data = outer.performance$data
  mdata = melt(data, measure.vars = measure.vars)
  g = ggplot(mapping = aes(x = dob, y = value, color = variable))
  g = g + stat_summary(data = mdata[variable %in% plot.vars, ], fun.y=median, geom="line")
  g = g + geom_point(data = mdata[variable %nin% plot.vars], alpha = 0.2)
  if (!is.null(resample.overfit)) {
    g = g + geom_boxplot(data = data.frame(value = resample.overfit$tuning.resampled$measures.test[,2], dob = max(data$dob), variable = outer.performance$y.outer.name), mapping = aes(x = dob, y = value), alpha = 0.5)
    g = g + geom_boxplot(data = data.frame(value = resample.overfit$untuned.resampled$measures.test[,2], dob = 0, variable = "untuned"), mapping = aes(x = dob, y = value), alpha = 0.5)
  }
  g
}