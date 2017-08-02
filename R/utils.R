# cummax / cummin handling nas
cummax.na = function(x) cummax(ifelse(is.na(x), -Inf, x))
cummin.na = function(x) cummin(ifelse(is.na(x), Inf, x))
max.na = function(x) max(x, na.rm = TRUE)
min.na = function(x) min(x, na.rm = TRUE)

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
    cumfun = cummin.na
    mfun = min.na
  } else {
    cumfun = cummax.na
    mfun = max.na
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
    cumfun = cummin.na
  } else {
    cumfun = cummax.na()
  }
  sel.index = sapply(cumfun(inner.perf), function(x) which.first(x == inner.perf))
  outer.perf[sel.index]
}

allEntriesToNa = function(df, goal.nrow = nrow(df)) {
  # simple stupid hack to keep the col.types while converting eveything to NAs
  if (nrow(df) == 1) {
    df = rbind(df,df)
  }
  df[1, ] = NA
  df[rep(1, times = goal.nrow), ]
}
