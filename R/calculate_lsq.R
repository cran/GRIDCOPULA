calculate_lsq <- function(U, k, m) {
  Qm <- count.grid(U, k, m)
  Am <- (m * k) * (Qm / sum(Qm))
  
  n_var <- k * m  
  Dmat <- 2 * diag(n_var)     
  dvec <- 2 * as.vector(Am)    

  Eq.m <- matrix(0, nrow = k + m, ncol = n_var)
  for (i in 1:m) {
    Eq.m[i, ((i - 1)*k + 1):(i*k)] <- 1
  }
  for (i in 1:k) {
    Eq.m[(m + i), seq(i, n_var, by = k)] <- 1
  }
  
  Eq.m <- Eq.m[-nrow(Eq.m), ]  
  Eq.v <- c(rep(k, m), rep(m, k)) 
  Eq.v <- Eq.v[-length(Eq.v)]      
  Ineq.m <- rbind(
    diag(n_var),    
    -diag(n_var)   
  )
  Ineq.v <- c(
    rep(0, n_var), 
    rep(-min(k, m), n_var)
  )

  Amat <- t(rbind(Eq.m, Ineq.m))
  bvec <- c(Eq.v, Ineq.v)
  meq <- nrow(Eq.m)  
  
  solution <- quadprog::solve.QP(
    Dmat = Dmat,
    dvec = dvec,
    Amat = Amat,
    bvec = bvec,
    meq = meq
  )

  Dm <- matrix(solution$solution, nrow = k, ncol = m)
  
  return(list(Density = Dm, Quantity = Qm, m = m, k = k))
}
