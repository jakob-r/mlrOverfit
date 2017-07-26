#' @title Calculates Outer Performances
#' @description Calculates the performance on the outer test set for a nested resampling with a tune wrapper
#' @param tuning.resampled [\code{\link[mlr]{ResampleResult}}]
#'   Make sure to run \code{resample(..., extract = getTuneResult, keep.pred = TRUE)}.
#' @param task [\code{\link[mlr]{Task}}]
#' @param measures [\code{\link[mlr]{Measure}}]
#' @return \code{data.table}
#' @export
calcOuterPerformances = function(tuning.resampled, task, measures) {
  assertClass(tuning.resampled, "ResampleResult")
  assertList(tuning.resampled$extract, "TuneResult")
  assertClass(task, "Task")
  measures = BBmisc::ensureVector(measures, cl = "Measure")
  assertList(measures, types = "Measure")

  par.set = tuning.resampled$extract[[1]]$opt.path$par.set
  learner = tuning.resampled$extract[[1]]$learner

  outer.errors = parallelMap(calcOuterPerformance, out.res.i = seq_along(tuning.resampled$extract), more.args = list(tuning.resampled = tuning.resampled, task = task, measures = measures))
  outer.errors = rbindlist(outer.errors)
  outer.errors
}

#' @title Calculates Outer Performance for a given Iteration
#' @description Calculates the performance on the outer test set for a specific nested resampling iteration with a tune wrapper
#' @inheritParams calcOuterPerformances
#' @param out.res.i [\code{integer(1)}]
#' @return \code{data.table}
#' @export
calcOuterPerformance = function(tuning.resampled, out.res.i, task, measures) {
  assertClass(tuning.resampled, "ResampleResult")
  assertList(tuning.resampled$extract, "TuneResult")
  assertIntegerish(out.res.i, lower = 1, upper = length(tuning.resampled$extract))
  assertClass(task, "Task")
  measures = BBmisc::ensureVector(measures, cl = "Measure")
  assertList(measures, types = "Measure")

  par.set = tuning.resampled$extract[[1]]$opt.path$par.set
  learner = tuning.resampled$extract[[1]]$learner

  op = tuning.resampled$extract[[out.res.i]]$opt.path
  train.inds = tuning.resampled$pred$instance$train.inds[[out.res.i]]
  test.inds = tuning.resampled$pred$instance$test.inds[[out.res.i]]

  par.settings = getOptPathX(op)
  par.settings = dfRowsToList(df = par.settings, par.set = par.set)
  par.settings = lapply(par.settings, function(x) trafoValue(par.set, x))
  learners.step = lapply(par.settings, function(x) setHyperPars(learner = learner, par.vals = x))

  calcPerf = function(learner, task, train.inds, test.inds, measures) {
    model = train(learner = learner, task = task, subset = train.inds)
    pred = predict(model, task = task, subset = test.inds)
    performance(pred, measures)
  }
  outer.error = parallelMap(fun = calcPerf, learner = learners.step, more.args = list(task = task, train.inds = train.inds, test.inds, measures = measures), level = "mlrOverfit.opteval")
  errors = rbindlist(lapply(outer.error, as.list))
  errors = setColNames(errors, paste0(colnames(errors),".outer.test"))
  errors = cbind(errors, as.data.frame(op))
  errors[["iter"]] = out.res.i
  errors
}