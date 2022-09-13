#' @title Calculates the Spearman's rho concordance measure for a grid type copula
#' @return Returns a number with the corresponding value.
#' @param gc a grid type copula object.
#' @examples
#' n <- 500
#' x <- rgamma(n,4,1/2)
#' e <- rnorm(n,0,.3)
#' y <- sin(x+e)
#' Fx <- ecdf(x)
#' Fy <- ecdf(y)
#' u <- Fx(x)
#' v <- Fy(y)
#' df <- cbind(u,v)
#' k <- 10
#' m <- 10
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' rho.grid(gc = copula.grid)
#' @export


rho.grid <- function(gc) {
	mg<- gc
  k <- mg$k
  m <- mg$m
  v.breaks <- seq(0, 1, length.out=k+1)
  u.breaks <- seq(0, 1, length.out=m+1)
  value <- 0
  for(i in 1:k) {
    for(j in 1:m) {
      value <- value + integral2(rho.integrand.grid, u.breaks[j], 
                                 u.breaks[j+1], v.breaks[i], v.breaks[i+1], 
                                 gc=mg)$Q
    }
  }
  value <- 12*value - 3
  return(value)
}
