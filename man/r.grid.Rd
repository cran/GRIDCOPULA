% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_grid.R
\name{r.grid}
\alias{r.grid}
\title{Generates a random sample from a grid type copula}
\usage{
r.grid(n, gc)
}
\arguments{
\item{n}{an integer number indicating the size of the sample.}

\item{gc}{a grid type copula object.}
}
\value{
Returns a  matrix of size  \eqn{nx2} with the random sample.
}
\description{
Generates a random sample from a grid type copula
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
copula.grid <- estimate.gridCopula(U = df, k = 15, m = 15 , method = "ml")
df2 <- r.grid(n = n, gc = copula.grid)
data.grid(copula.grid$U, k = 15, m = 15)
data.grid(df2, k = 15, m = 15)

# Using the Iris dataset, transformation is not mandatory
copula.grid <- estimate.gridCopula(X = iris[,1:2], k = 3, m = 7 , method = "ml")
df2 <- r.grid(n = n, gc = copula.grid)
data.grid(copula.grid$U, k = 3, m = 7)
data.grid(df2, k = 3, m = 7)
}
