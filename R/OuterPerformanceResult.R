#' @title Convert OuterPerformanceResult to data.frame
#' @description Basically just extracts the data as a data.table
#' @param obj [\code{\link{OuterPerformanceResult}}]
#' @param ... [\code{...}]
#'   Arguments passed do \code{\link{as.data.frame}}
#' @return [\code{data.frame}]
#' @export
as.data.frame.OuterPerformanceResult = function(obj, ...) {
  as.data.frame(obj$data, )
}