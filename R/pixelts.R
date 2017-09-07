
#' Pixel time series extraction
#'
#' Extracts the time series of a SciDB array at a given coordinate.
#'
#'
#' @name pixelts,scidbst
#' @param obj scidbst object
#' @param x coordinates of pixel where the time series will be extracted
#'
#' @export
pixelts = function(obj, x) {
  stopifnot("scidbst" %in% class(obj))
  A = affine(obj)
  p = as.vector(round((solve(A[,2:3])) %*% c(x[1] - A[1,1], x[2] - A[2,1])))
  obj@proxy = scidb::slice(obj@proxy,c(xdim(obj), ydim(obj)), p)
  obj@isSpatial = FALSE
  obj@isTemporal = TRUE
  return(obj)
}
