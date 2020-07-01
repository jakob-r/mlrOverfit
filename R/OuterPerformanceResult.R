#' @title Convert OuterPerformanceResult to data.frame
#' @description Basically just extracts the data as a data.table
#' @param x [\code{\link{OuterPerformanceResult}}]
#' @param row.names [character]\cr
#'   Row names for result.
#'   Default is none.
#' @param optional (any)\cr
#'   Currently ignored.
#' @param ... [\code{...}]
#'   Arguments passed do \code{\link{as.data.frame}}
#' @return [\code{data.frame}]
#' @export
as.data.frame.OuterPerformanceResult = function(x, row.names, optional, ...) {
  as.data.frame(x$data, row.names = row.names, optional = optional, ...)
}