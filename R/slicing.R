
#' @title Creates intervals with equal sizes to be used by plot functions
#' @description Creates intervals with equal sizes from min/max values to be
#' used by plot functions. If data is provided, it gets min/max from data.
#' @param min The minimum value
#' @param max The maximum value
#' @param slices The number of slices to be created. Note that, when you have
#' n slices, the legend will have n - 1 colors.
#' @param data Any data that can be converted to a data.frame. The first column
#' of the data will be used to compute min and max.
#' @examples
#' equalSteps(0, 100, 5)
#' @export
equalSteps <- function(min = NULL, max = NULL, slices, data = NULL){
  if(!is.null(data)){
    values = as.data.frame(data)[,1]

    if(!is.null(max)) stop("Cannot use 'max' and 'data' at the same time.")
    if(!is.null(min)) stop("Cannot use 'min' and 'data' at the same time.")

    max <- max(values)
    min <- min(values)
  }

  rep(min, slices) + (max - min) / (slices - 1) * 0:(slices - 1)
}
