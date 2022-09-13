#' @title Estimates the parameters of a grid type copula
#' @description This function estimates grid type copulas by one of the following methods: maximum likelihood or least squares (See reference).
#' @return Returns a list with a matrix with the density over the grid,
#' a matrix with the quantity of data over the grid, the number of subintervals for the U2 variable,
#' the number of subintervals for the U1 variable.
#' @param U a matrix of size nx2 with the observed values.
#' @param k positive integer indicating the number of subintervals for the U2 variable.
#' @param m positive integer indicating the number of subintervals for the U1 variable.
#' @param method Method that uses, least squares "ls" or maximum likelihood "ml", by default "ml".
#' @param D.ini a matrix with the initial values for the density copula.package: the name of the package for numerical optimization.
#' @param criterion If the values of k i m are not specified, they will be obtained by the "AIC" or "BIC" criteria, by default "AIC".
#' @references {@misc{https://doi.org/10.48550/arxiv.2010.15709,
#' doi = {10.48550/ARXIV.2010.15709}, 
#' url = {https://arxiv.org/abs/2010.15709},
#' author = {Pfeifer, Dietmar and Strassburger, Doreen and Philipps, Joerg},
#' keywords = {Methodology (stat.ME), Risk Management (q-fin.RM), FOS: Computer and information sciences, FOS: Computer and information sciences, FOS: Economics and business, FOS: Economics and business, 62H05, 62H12, 62H17, 11K45},
#' title = {Modelling and simulation of dependence structures in nonlife insurance with Bernstein copulas},
#' publisher = {arXiv},
#' year = {2020},
#' copyright = {arXiv.org perpetual, non-exclusive license}
#' }
#' }
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
#' k <- 5
#' m <- 4
#' copula.grid <- estimate.gridCopula(U = df, k = k, m = m , method = "ml")
#' print(copula.grid$Density)
#' @export




estimate.gridCopula <- function(U,k = NULL,m = NULL,method = "ml" , D.ini=NULL, criterion = "AIC")

	{

	if(method=="ls")
		{
		AIC <- c()
		if(is.null(k) & is.null(m))
			{
			for(i in 2:20)
				{
				copula <- calculate.ls(U=U,k=i,m=i)
				if(criterion == "AIC")
					{

					AIC[i-1] <- aic.grid(copula)

					}else if(criterion == "BIC")
						{

						AIC[i-1] <- bic.grid(copula)

						}
				}

			k <- which(AIC == min(AIC))+1
			m <- which(AIC == min(AIC))+1
			}else if(!is.null(k) & is.null(m)) {
					m <- k	
					}else if(is.null(k) & !is.null(m))
						{
						k <- m
						}
		k <- round(k,0)
		m <- round(m,0)
		calculate.ls(U=U,k=k,m=m)
		}else
	if(method=="ml")
		{
		AIC <- c()
		if(is.null(k) & is.null(m))
			{
			for(i in 2:20)
				{
				copula <- calculate.ml(U=U,k=i,m=i,D.ini=D.ini) 
				if(criterion == "AIC")
					{

					AIC[i-1] <- aic.grid(copula)

					}else if(criterion == "BIC")
						{

						AIC[i-1] <- bic.grid(copula)

						}
				}

			k <- which(AIC == min(AIC))+1
			m <- which(AIC == min(AIC))+1
			}else if(!is.null(k) & is.null(m)) {
					m <- k	
					}else if(is.null(k) & !is.null(m))
						{
						k <- m
						}

		k <- round(k,0)
		m <- round(m,0)
		calculate.ml(U=U,k=k,m=m,D.ini=D.ini)
		}
	}









