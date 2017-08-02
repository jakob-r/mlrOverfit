context("Workflow")

test_that("basic workflow works", {
  task = iris.task
  learner = makeLearner("classif.ksvm")
  par.set = makeParamSet(
    makeNumericParam(id = "C",  upper = 10, lower = -5, trafo = function(x) 2^x),
    makeNumericParam(id = "sigma",  upper = 15, lower = -15, trafo = function(x) 2^x)
  )
  tune.control = makeTuneControlRandom(maxit = 5)
  learner.tuned = makeTuneWrapper(learner = learner, resampling = hout, par.set = par.set, control = tune.control)
  ro = resampleOverfit(learner = learner.tuned, task = task, resampling = cv3)

  expect_class(ro, "ResampleOverfitResult")
  expect_list(ro[c("tuning.resampled", "untuned.resampled")], types = "ResampleResult")
  expect_class(ro$task, "Task")
  expect_list(ro$measures, "Measure")

  outer.errors = calcOuterPerformances(ro, only.on.improvement = FALSE)
  outer.errors = calcOuterPerformances(ro, only.on.improvement = TRUE)

  expect_class(outer.errors, "OuterPerformanceResult")
  expect_class(outer.errors$data, "data.table")
  expect_list(outer.errors$measures, "Measure")
  expect_character(outer.errors$y.inner.name)
  expect_character(outer.errors$y.outer.name)
  expect_subset(c(outer.errors$y.inner.name, outer.errors$y.outer.name, "dob", "iter"), colnames(outer.errors$data))
  expect_set_equal(unique(outer.errors$data$dob), 1:5)
  expect_set_equal(unique(outer.errors$data$iter), 1:3)

  outer.errors = simulateOuterTestError(outer.errors)
  expect_numeric(outer.errors$data[[outer.errors$sim.y.outer.name]], any.missing = FALSE)

  plot(outer.errors, ro)
})