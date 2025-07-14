r_bivariate_Poisson <- function(N, rho, mu1, mu2) {
  
  # covariance term, equal to E(Z_3)
  EZ3 <- rho * sqrt(mu1 * mu2) 
  
  # Generate independent components
  Z1 <- rpois(N, lambda = mu1 - EZ3)
  Z2 <- rpois(N, lambda = mu2 - EZ3)
  Z3 <- rpois(N, lambda = EZ3)
  
  # Assemble components
  dat <- data.frame(
    C1 = Z1 + Z3,
    C2 = Z2 + Z3
  )
  
  return(dat)
}