library(mlr)
library(mlrMBO)
library(mlrHyperopt)
library(parallelMap)
library(data.table)
library(ggplot2)
devtools::load_all()
parallelMap::parallelRegisterLevels(package = "mlrOverfit", levels = "opteval")
task = sonar.task
learner = makeLearner("classif.svm")
tune.control = makeTuneControlMBO(budget = 20)
measures = list(acc)

par.set = getDefaultParConfig(learner)$par.set
learner.tuned = makeTuneWrapper(learner = learner, resampling = cv3, measures = measures, par.set = par.set, control = tune.control)
tuning.resampled = resample(learner = learner.tuned, task = task, resampling = cv10, measures = measures, extract = getTuneResult, keep.pred = TRUE)

outer.errors = calcOuterPerformances(tuning.resampled = tuning.resampled, task = task, measures = measures)

# calculate theoretical outer performance at time x
head(outer.errors)
y.inner.name = c("acc.test.mean")
y.outer.name = c("acc.outer.test")

plot(acc.outer.test ~ acc.test.mean, outer.errors)

cum.outer.errors = outer.errors[, c(list(dob = dob), parallelMap(cum.over.dob, x = as.list(.SD), more.args = list(dob = dob, minimize = FALSE))), by = .(iter), .SDcols=c(y.inner.name, y.outer.name)]
colnames(cum.outer.errors)[-(1:2)] = paste0("cum.", c(y.inner.name, y.outer.name))
outer.errors = merge(outer.errors, cum.outer.errors)
simulated.error = outer.errors[, setNames(list(simulate.outer.test(inner.perf = get(y.inner.name), outer.perf = get(y.outer.name), minimize = FALSE)), paste0("sim.",y.outer.name)), by = .(iter)]
outer.errors = cbind(outer.errors, simulated.error[,-1])


measure.vars = c(y.inner.name, y.outer.name, paste0("cum.", c(y.inner.name, y.outer.name)), paste0("sim.", y.outer.name))
plot.vars = c(paste0("cum.", c(y.inner.name, y.outer.name)), paste0("sim.", y.outer.name))
mdata = melt(outer.errors, measure.vars = measure.vars)
g = ggplot(mapping = aes(x = dob, y = value, color = variable))
g = g + stat_summary(data = mdata[variable %in% plot.vars, ], fun.y=mean, geom="line")
g = g + geom_point(data = mdata[variable %nin% plot.vars], alpha = 0.2)
g

#find the best values over the time
