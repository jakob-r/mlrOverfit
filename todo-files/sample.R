library(mlr)
library(mlrMBO)
library(mlrHyperopt)
library(parallelMap)
library(data.table)
library(ggplot2)
devtools::load_all()
parallelMap::parallelRegisterLevels(package = "mlrOverfit", levels = "opteval")
task = bh.task
learner = makeLearner("regr.ksvm")
tune.control = makeTuneControlRandom(maxit = 50)
measures = mlr::getDefaultMeasure(task)

par.set = getDefaultParConfig(learner)$par.set
#par.set = evaluateParamExpressions(par.set, dict = mlrHyperopt::getTaskDictionary(task))
learner.tuned = makeTuneWrapper(learner = learner, resampling = hout, measures = measures, par.set = par.set, control = tune.control)
ro = resampleOverfit(learner.tuned = learner.tuned, task = task, resampling = cv10, measures = measures)

outer.errors = calcOuterPerformances(ro, only.on.improvement = TRUE)

# calculate theoretical outer performance at time x
head(outer.errors)


outer.errors = simulateOuterPerformance(outer.errors)

g = plot(outer.errors, ro)
g
#plotly::ggplotly(g)
#find the best values over the time
