#' @title Generates Data for Overfitting Analysis
#' @description Generates all necessary data to perform the overfitting analysis
#'
#' @param task [\code{\link[mlr]{Task}}]
#' @param learner [\code{\link[mlr]{Learner}}]
#'   Learner to be tuned and analyzed.
#' @param learner.tuned [\code{\link[mlr]{Learner}}]
#'   Learner wrapped with [\code{\link[mlr]{makeTuneWrapper}}]
#' @param tune.control [\code{\link[mlr]{TuneControl}}]
#'   If none is supplied a random search with 10 iterations will be performed.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]
#' @template param_only_on_improvement
#' @param outer.resampling [\code{\link[mlr]{ResampleDesc}} | \code{\link[mlr]{ResampleInstance}}]
#'   Default is 10CV.
#' @param ... [\code{...}]
#'   Arguments passed to \code{resampleOverfit}
#' @aliases OverfitAnalysisResult
#' @export
generateOverfitAnalysis = function(task, learner, tune.control = NULL, par.set, learner.tuned = NULL, only.on.improvement = FALSE, outer.resampling = cv10, ...) {

  assert_true(xor(is.null(learner), is.null(learner.tuned)))
  assert_true(is.null(learner.tuned) || is.null(tune.control))

  if (is.null(tune.control)) {
    tune.control = makeTuneControlRandom(maxit = 10)
  }
  if (is.null(learner.tuned)) {
    learner.tuned = makeTuneWrapper(learner = learner, resampling = cv10, par.set = par.set, control = tune.control)
  }

  ro = resampleOverfit(learner.tuned = learner.tuned, task = task, resampling = outer.resampling, ...)

  outer.performance = calcOuterPerformances(ro, only.on.improvement = only.on.improvement)
  outer.performance = simulateOuterPerformance(outer.performance)
  addClasses(list(simulated.outer.performance = outer.performance, resample.overfit = ro), "OverfitAnalysisResult")
}