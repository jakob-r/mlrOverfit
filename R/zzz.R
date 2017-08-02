#' @import BBmisc
#' @import checkmate
#' @import mlr
#' @import ggplot2
#' @import data.table
#' @import parallelMap
#' @importFrom plyr revalue

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlrOverfit", levels = c("opteval"))
}