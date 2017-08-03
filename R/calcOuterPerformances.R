#' @title Calculates Outer Performances
#' @description Calculates the performance on the outer test set for a nested resampling with a tune wrapper
#' @param tuning.resampled [\code{\link[mlr]{ResampleResult}} | \code{\link{ResampleOverfitResult}}]
#'   Make sure to run \code{resample(..., extract = getTuneResult, keep.pred = TRUE)}.
#' @param task [\code{\link[mlr]{Task}}]
#' @param measures [\code{\link[mlr]{Measure}}]
#' @param ...
#'   Arguments passed to specific methods
#' @template param_only_on_improvement
#' @return [\code{OuterPerformanceResult}]
#' @aliases OuterPerformanceResult
#' @export
calcOuterPerformances = function(tuning.resampled, ...) {
  UseMethod("calcOuterPerformances")
}

#' @export
calcOuterPerformances.ResampleOverfitResult = function(tuning.resampled, only.on.improvement = FALSE) {
  calcOuterPerformances(tuning.resampled$tuning.resampled, tuning.resampled$task, tuning.resampled$measures, only.on.improvement = only.on.improvement)
}

#' @export
calcOuterPerformances.ResampleResult = function(tuning.resampled, task, measures, only.on.improvement = FALSE) {
  assertList(tuning.resampled$extract, "TuneResult")
  assertClass(task, "Task")
  measures = BBmisc::ensureVector(measures, cl = "Measure")
  assertList(measures, types = "Measure")

  outer.errors = parallelMap(calcOuterPerformance, out.res.i = seq_along(tuning.resampled$extract), more.args = list(tuning.resampled = tuning.resampled, task = task, measures = measures, only.on.improvement = only.on.improvement))
  data = extractSubList(outer.errors, "data", simplify = FALSE)
  data = rbindlist(data)
  res = insert(outer.errors[[1]], list(data = data))
  addClasses(res, "OuterPerformanceResult")
}

#' @title Calculates Outer Performance for a given Iteration
#' @description Calculates the performance on the outer test set for a specific nested resampling iteration with a tune wrapper
#' @inheritParams calcOuterPerformances
#' @param out.res.i [\code{integer(1)}]
#' @return \code{data.table}
#' @export
calcOuterPerformance = function(tuning.resampled, out.res.i, ...) {
  UseMethod("calcOuterPerformance")
}

#' @export
calcOuterPerformance.ResampleOverfitResult = function(tuning.resampled, out.res.i, only.on.improvement = FALSE) {
  calcOuterPerformance(tuning.resampled$tuning.resampled, out.res.i = out.res.i, tuning.resampled$task, tuning.resampled$measures, only.on.improvement = only.on.improvement)
}

#' @export
calcOuterPerformance.ResampleResult = function(tuning.resampled, out.res.i, task, measures, only.on.improvement = FALSE) {
  assertList(tuning.resampled$extract, "TuneResult")
  assertIntegerish(out.res.i, lower = 1, upper = length(tuning.resampled$extract))
  assertClass(task, "Task")
  measures = BBmisc::ensureVector(measures, cl = "Measure")
  assertList(measures, types = "Measure")
  assertFlag(only.on.improvement)

  par.set = tuning.resampled$extract[[1]]$opt.path$par.set
  learner = tuning.resampled$extract[[1]]$learner

  op = tuning.resampled$extract[[out.res.i]]$opt.path
  train.inds = tuning.resampled$pred$instance$train.inds[[out.res.i]]
  test.inds = tuning.resampled$pred$instance$test.inds[[out.res.i]]

  par.settings = getOptPathX(op)

  # subset the par.settings we want to calculate to the rows where an improvement in the performance was measured
  if (only.on.improvement) {
    y = getOptPathY(op)
    if (measures[[1]]$minimize) {
      y = cummin(y)
    } else {
      y = cummax(y)
    }
    y.change = which(c(TRUE, diff(y) != 0))
  } else {
    y.change = seq_row(par.settings)
  }
  par.settings = par.settings[y.change, ]

  par.settings = dfRowsToList(df = par.settings, par.set = par.set)
  par.settings = lapply(par.settings, function(x) trafoValue(par.set, x))
  learners.step = lapply(par.settings, function(x) setHyperPars(learner = learner, par.vals = x))

  # Function to calculate the performance on the outer test data
  calcPerf = function(learner, task, train.inds, test.inds, measures) {
    model = train(learner = learner, task = task, subset = train.inds)
    pred = predict(model, task = task, subset = test.inds)
    performance(pred, measures)
  }
  outer.error = parallelMap(fun = calcPerf, learner = learners.step, more.args = list(task = task, train.inds = train.inds, test.inds, measures = measures), level = "mlrOverfit.opteval")
  errors = rbindlist(lapply(outer.error, as.list))

  y.outer.name = paste0(colnames(errors),".outer.test")
  y.inner.name = names(tuning.resampled$extract[[1]]$y)

  # create empty data.table that we can fill with the obtained error data
  # this is neccessary because we probably just calculate the outer performance for a subset
  empty.dt = allEntriesToNa(errors, goal.nrow = getOptPathLength(op))
  empty.dt[y.change, ] = errors
  errors = empty.dt

  errors = setColNames(errors, y.outer.name)
  errors = cbind(errors, as.data.frame(op))
  errors[["iter"]] = out.res.i
  res = list(data = errors, y.inner.name = y.inner.name, y.outer.name = y.outer.name, measures = measures)
  addClasses(res, "OuterPerformanceResult")
}