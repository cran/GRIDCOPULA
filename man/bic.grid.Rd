% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bic_grid.R
\name{bic.grid}
\alias{bic.grid}
\title{Calculates the BIC of a grid type copula}
\usage{
bic.grid(gc)
}
\arguments{
\item{gc}{a grid type copula object.}
}
\value{
Returns a number with the BIC of a grid type copula.
}
\description{
This function receives a grid type copula as a parameter and returns the value of the BIC.
}
\examples{
n <- 500
x <- rgamma(n,4,1/2)
e <- rnorm(n,0,.3)
y <- sin(x+e)
Fx <- ecdf(x)
Fy <- ecdf(y)
u <- Fx(x)
v <- Fy(y)
df <- cbind(u,v)
k <- 5
m <- 4
copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
bic.grid(copula.grid)
}
