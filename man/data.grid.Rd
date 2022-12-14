% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_grid.R
\name{data.grid}
\alias{data.grid}
\title{Draws the scatter plot of bivariate data in the unit square}
\usage{
data.grid(U, draw.lines = TRUE, k = 4, m = 4)
}
\arguments{
\item{U}{matrix of size kx2 with the values of both variables.}

\item{draw.lines}{draws lines inside the unit square or not.}

\item{k}{positive integer indicating the number of subintervals for the U2 variable.}

\item{m}{positive integer indicating the number of subintervals for the U1 variable.}
}
\value{
Returns a scatter plot of bivariate data in the unit square.
}
\description{
Draws the scatter plot of bivariate data in the unit square
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
k <- 10
m <- 10
data.grid(U=df, draw.lines = FALSE, k = k, m = m)
data.grid(U=df, draw.lines = TRUE, k = k, m = m)
}
