#' @title Evaluates the density of a grid copula for a single data point
#' @description Returns the corresponding distribution function values.
#' @param u vector of size 1x2 with the values of the variables.
#' @param mg a grid type copula object.
#' @examples


pdf.grid <- function(u, mg) {

  k <- mg$k
  m <- mg$m
  u.index <- ceiling(m*u[1])
  v.index <- ceiling(k*u[2])
  if(u.index==0) {
    u.index <- 1
  }
  if(v.index==0) {
    v.index <- 1
  }
  index.column <- u.index
  index.row <- 1 - (v.index-k)
  value <- mg$Density[index.row, index.column]
  return(value)
}
