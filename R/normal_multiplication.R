#' @title Calculates the multiplication of normal densities
#' @description Returns a vector with the values of the product of densities.
#' @param x a vector with the values of the variable x.
#' @param y a vector with the values of the variable y.
#' @examples


normal.multiplication <- function(x, y) {
  f.x <- dnorm(x)
  f.y <- dnorm(y)
  value <- f.x * f.y
  return(value)
}
