#' @title Resampling Generating the Necessary Data for Analysis
#' @description Calls \code{\link[mlr]{resample}} with all the arguments that are nesseccary to generate the data for the overfitting analysis.
#'
#' @param learner.tuned [\code{\link[mlr]{Learner}}]
#'   Learner wrapped with [\code{\link[mlr]{makeTuneWrapper}}]
#' @param task [\code{\link[mlr]{Task}}]
#' @param resampling [\code{\link[mlr]{ResampleDesc}} | \code{\link[mlr]{ResampleInstance}} ]
#' @param measures [\code{\link[mlr]{Measure}}]
#'   Only the first measure will be used for analysis
#' @return [\code{ResampleOverfitResult}]
#' @aliases ResampleOverfitResult
#' @export
resampleOverfit = function(learner.tuned, task, resampling, measures = NULL, ...) {
  assertClass(learner.tuned, "TuneWrapper")
  assertClass(task, "Task")
  checkTRUE(checkClass(resampling, "ResampleDesc") || checkClass(resampling, "ResampleInstance"))

  if (is.null(measures)) {
    measures = getDefaultMeasure(task)
  }
  measures = ensureVector(x = measures, cl = "Measure", ensure.list = TRUE)

  learner = learner.tuned$next.learner

  tuning.resampled = resample(learner = learner.tuned, task = task, resampling = resampling, measures = measures, extract = getTuneResult, keep.pred = TRUE, ...)
  untuned.resampled = resample(learner = learner, task = task, resampling = resampling, measures = measures, keep.pred = TRUE, ...)

  res = list(tuning.resampled = tuning.resampled, untuned.resampled = untuned.resampled, task = task, measures = measures)
  addClasses(res, "ResampleOverfitResult")
}