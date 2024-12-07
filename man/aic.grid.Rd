% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aic_grid.R
\name{aic.grid}
\alias{aic.grid}
\title{Calculates the Akaike Information Criterion "AIC" of a grid type copula}
\usage{
aic.grid(gc)
}
\arguments{
\item{gc}{a grid type copula object.}
}
\value{
Returns a number with the AIC of a grid type copula.
}
\description{
This function receives a grid type copula as a parameter and returns the value of the AIC.
}
\examples{
# Generating simulated data with a transformation to the copula domain 
n <- 500
x <- rgamma(n,4,1/2)
e <- rnorm(n,0,.3)
y <- sin(x+e)
Fx <- ecdf(x)
Fy <- ecdf(y)
u <- Fx(x)
v <- Fy(y)
df <- cbind(u,v)
copula.grid <- estimate.gridCopula(U = df, k = 5, m = 4 , method = "ml")
aic.grid(copula.grid)

# Using the Iris dataset, transformation is not mandatory
copula.grid <- estimate.gridCopula(X = iris[,1:2], k = 3, m = 7 , method = "ml")
aic.grid(copula.grid)
}
