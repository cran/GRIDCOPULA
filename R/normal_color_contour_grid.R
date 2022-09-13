#' @title Draws the density of a grid copula with contours, normal marginals and colors
#' @return Returns a graph.
#' @param gc a grid type copula object.
#' @param x1 indicates the place for lines on axis x1.
#' @param x2 indicates the place for lines on axis x2.
#' @param color.name indicates the palette of colors.
#' @param color.size indicates the number of colors.
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
#' normal.color.contour.grid(gc = copula.grid, color.name = "heat.colors")
#' @export

normal.color.contour.grid <- function(gc, x1=seq(-3, 3, length.out=21), 
                                      x2=seq(-3, 3, length.out=21), color.name="gray", 
                                      color.size=7) {
	mg<- gc
  u1 <- pnorm(x1)
  u2 <- pnorm(x2)
  f.u <- outer(u1, u2, 'd.grid', mg)
  f.x <- outer(x1, x2, 'normal.multiplication') * f.u
  f.levels <- seq(0, 1.02*max(f.x), length.out=(color.size+1))
  if(color.name=="heat.colors") {
    paleta.color <- heat.colors(n=color.size)
  } else if(color.name=="rainbow") {
    paleta.color <- rainbow(n=color.size)
  } else if(color.name=="terrain.colors") {
    paleta.color <- terrain.colors(n=color.size)
  } else if(color.name=="topo.colors") {
    paleta.color <- topo.colors(n=color.size)
  } else if(color.name=="cm.colors") {
    paleta.color <- cm.colors(n=color.size)
  } else if(color.name=="gray") {
    paleta.color <- gray(seq(0,1,length.out=(color.size+1)))
  }
  filled.contour(x1, x2, f.x, col=paleta.color, levels=f.levels, 
                 xlab="x", ylab="")
  mtext(text="y", side=2, line=3, las=1)
  return(0)
}
