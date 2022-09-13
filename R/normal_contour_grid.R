#' @title Draws the density of a grid copula with contours and normal marginals
#' @return Returns a graph.
#' @param gc a grid type copula object.
#' @param x1 indicates the place for lines on axis x1.
#' @param x2 indicates the place for lines on axis x2.
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
#' normal.contour.grid(gc = copula.grid)
#' @export



normal.contour.grid <- function(gc, x1=seq(-3, 3, length.out=21), 
                                x2=seq(-3, 3, length.out=21)) {
	mg<- gc
  u1 <- pnorm(x1)
  u2 <- pnorm(x2)
  f.u <- outer(u1, u2, 'd.grid', mg)
  f.x <- outer(x1, x2, 'normal.multiplication') * f.u
  contour(x1, x2, f.x, xlab="x", ylab="")
  mtext(text="y", side=2, line=3, las=1)
  return(0)
}
