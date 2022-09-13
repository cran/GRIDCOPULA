#' @title Counts the data according to a specified grid
#' @description Returns a matrix of size kxm indicating the quantity of data.
#' @param U a matrix of size nx2 with the observed values.
#' @param k a positive integer indicating the number of subintervals for the U2 variable.
#' @param m a positive integer indicating the number of subintervals for the U1 variable.
#' @examples



count.grid <- function(U, k, m) {
  B <- matrix(0, nrow=k, ncol=m)
  n <- nrow(U)
  for(i in 1:n) {
    u.index <- ceiling(m*U[i, 1])
    v.index <- ceiling(k*U[i, 2])
    if(u.index==0) {
      u.index <- 1
    }
    if(v.index==0) {
      v.index <- 1
    }
    index.column <- u.index
    index.row <- 1 - (v.index-k)
    B[index.row, index.column] <- B[index.row, index.column] + 1
  }
  return(B)
}