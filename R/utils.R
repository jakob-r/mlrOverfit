# @arg x [numeric]
#  vector of performance values
# @arg dob [numeric]
#  vector if date of birth for each performance value
# @arg minimize [logical(1)]
#  e.g. TRUE for cummin, else cummax
# @return numeric
#  numeric vector of the cummulated values. for values with the same dob just the min/max is used

cum.over.dob = function(x, dob, minimize = TRUE) {
  if (minimize) {
    cumfun = get("cummin", mode = "function")
    mfun = get("min", mode = "function")
  } else {
    cumfun = get("cummax", mode = "function")
    mfun = get("max", mode = "function")
  }
  values.dt = data.table(x = x, dob = dob)
  tmp.min = values.dt[, list(cum = min(x)), by = "dob"]
  values.dt = merge(values.dt, tmp.min, all.x = TRUE, all.y = FALSE, by = "dob")
  values.dt = values.dt[order(dob), ]
  cumfun(values.dt$cum)
}

# @arg inner.perf [numeric]
#  values for inner performance
# @outer.perf [numeric]
#  values for performance on outer test set
# @arg minimize [logical(1)]
#  e.g. TRUE for cummin, else cummax
# @return numeric
#  the numeric vector that indicates the outer test performance that would be reached at a certain point
simulate.outer.test = function(inner.perf, outer.perf, minimize = TRUE) {
  if (minimize) {
    cumfun = get("cummin", mode = "function")
  } else {
    cumfun = get("cummax", mode = "function")
  }
  sel.index = sapply(cumfun(inner.perf), function(x) which.first(x == inner.perf))
  outer.perf[sel.index]
}