#' @title Draws the density / distribution function of a grid copula with contours
#' @return Returns a graph of the density / distribution.
#' @param gc a grid type copula object.
#' @param FUN the name of the function to be applied (d.grid, p.grid), default is 'd.grid'.
#' @param u1 indicates the place for lines on axis u1.
#' @param u2 indicates the place for lines on axis u2.
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
#' contour_grid(gc = copula.grid, FUN = 'd.grid') 
#' contour_grid(gc = copula.grid, FUN = 'p.grid') 
#' @export



contour_grid <- function(gc, FUN='d.grid', u1=seq(0, 1, length.out=21), 
                         u2=seq(0, 1, length.out=21)) {
	mg<- gc
  f.u <- outer(u1, u2, FUN, mg)
  contour(u1, u2, f.u, xlab="u", ylab="", xaxp=c(0, 1, 4), yaxp=c(0, 1, 4), 
          xlim=c(0,1), ylim=c(0,1))
  mtext(text="v", side=2, line=3, las=1)
  return(0)
}

