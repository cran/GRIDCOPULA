#' @import fields
#' @import ggplot2
#' @import reshape2
#' @import Rsolnp
#' @import pracma
#' @title Calculates the distribution function of a grid copula for a single data point
#' @description Returns the corresponding distribution function value.
#' @param u vector of size 1x2 with the values of the variables.
#' @param mg a grid type copula object.
#' @param Vm Volume matrix.
#' @examples



cdf.grid <- function(u, mg, Vm) {
  k <- mg$k
  m <- mg$m
  value <- 0
  if(0 < u[1] & 0 < u[2]) {
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
    if(1 < u.index & 1 < v.index) {
      value <- Vm[(index.row+1),(index.column-1)]
      u.excess <- (u[1] - (u.index-1)/m)
      v.excess <- (u[2] - (v.index-1)/k)
      value <- value + ( u.excess * sum(mg$Density[((index.row+1):k),index.column]) / k )
      value <- value + ( v.excess * sum(mg$Density[index.row,(1:(index.column-1))]) / m )
      value <- value + ( u.excess * v.excess * mg$Density[index.row,index.column] )
      
    }
    if(u.index==1 & 1 < v.index) {
      u.excess <- u[1]
      v.excess <- (u[2] - (v.index-1)/k)
      value <- u.excess * sum(mg$Density[((index.row+1):k),index.column]) / k
      value <- value + ( u.excess * v.excess * mg$Density[index.row,index.column] )
    }
    if(1 < u.index & v.index==1) {
      u.excess <- (u[1] - (u.index-1)/m)
      v.excess <- u[2]
      value <- v.excess * sum(mg$Density[index.row,(1:(index.column-1))]) / m
      value <- value + ( u.excess * v.excess * mg$Density[index.row,index.column] )
    }
    if(u.index==1 & v.index==1) {
      u.excess <- u[1]
      v.excess <- u[2]
      value <- u.excess * v.excess * mg$Density[index.row,index.column]
    }
  }
  return(value)
}
