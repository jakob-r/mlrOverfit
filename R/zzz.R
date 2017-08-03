#' @import BBmisc
#' @import checkmate
#' @import mlr
#' @import ggplot2
#' @import data.table
#' @import parallelMap
#' @import ParamHelpers
#' @importFrom plyr revalue
#' @importFrom graphics plot
#' @importFrom stats median setNames predict

.onAttach = function(libname, pkgname) {
  configureMlr()
  parallelRegisterLevels(package = "mlrOverfit", levels = c("opteval"))
}