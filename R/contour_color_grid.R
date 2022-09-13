#' @title Draws the density / distribution function of a grid copula with contours and colors
#' @return Returns a graph of the density / distribution.
#' @param gc a grid type copula object.
#' @param FUN the name of the function to be applied (d.grid, p.grid), default is 'd.grid'.
#' @param u1 indicates the place for lines on axis u1.
#' @param u2 indicates the place for lines on axis u2.
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
#' contour_color_grid(gc = copula.grid, FUN = 'd.grid', color.name = "rainbow") 
#' contour_color_grid(gc = copula.grid, FUN = 'p.grid', color.name = "rainbow") 
#' @export



contour_color_grid <- function(gc, FUN='d.grid', u1=seq(0, 1, length.out=21), 
                               u2=seq(0, 1, length.out=21), color.name="heat.colors", 
                               color.size=7) {
	mg<-gc
  f.u <- outer(u1, u2, FUN, mg)
  f.levels <- seq(0, 1.02*max(f.u), length.out=(color.size+1))
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
  filled.contour(u1, u2, f.u, col=paleta.color, levels=f.levels, 
                 xlab="u", ylab="", plot.axes={axis(1, seq(0,1,by=0.25))
                   axis(2, seq(0,1,by=0.25))})
  mtext(text="v", side=2, line=3, las=1)
  return(0)
}

