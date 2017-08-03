#' @title Simulates the outer test error
#' @description Based on the best observed point in the inner resampling the performance on the outer test set will be chosen.
#'
#' @param outer.performance [\code{\link{OuterPerformanceResult}}]
#' @return [\code{SimulateOuterPerformanceResult}]
#' @aliases SimulateOuterPerformanceResult
#' @export
simulateOuterPerformance = function(outer.performance) {
  data = outer.performance$data
  y.inner.name = outer.performance$y.inner.name
  y.outer.name = outer.performance$y.outer.name
  cum.y.inner.name = paste0("cum.", y.inner.name)
  cum.y.outer.name = paste0("cum.", y.outer.name)
  sim.y.outer.name = paste0("sim.", y.outer.name)

  minimize = outer.performance$measures[[1]]$minimize

  cum.data = data[, c(list(dob = dob), parallelMap(cum.over.dob, x = as.list(.SD), more.args = list(dob = dob, minimize = minimize))), by = .(iter), .SDcols=c(y.inner.name, y.outer.name)]

  colnames(cum.data)[-(1:2)] = c(cum.y.inner.name, cum.y.outer.name)

  data = merge(data, cum.data, sort = FALSE)

  simulated.error = data[, setNames(list(simulate.outer.test(inner.perf = get(y.inner.name), outer.perf = get(y.outer.name), minimize = minimize)), sim.y.outer.name), by = .(iter)]

  data = cbind(data, simulated.error[,-1])

  outer.performance$data = data
  outer.performance = insert(outer.performance, list(cum.y.inner.name = cum.y.inner.name, cum.y.outer.name = cum.y.outer.name, sim.y.outer.name = sim.y.outer.name))

  addClasses(outer.performance, "SimulateOuterPerformanceResult")
}
