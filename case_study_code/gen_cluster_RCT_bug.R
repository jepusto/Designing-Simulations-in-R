gen_cluster_RCT <- function(
    J = 30,
    n_bar = 10,
    alpha = 0,
    p = 0.5,
    gamma_0 = 0, gamma_1 = 0, gamma_2 = 0,
    sigma2_u = 0, sigma2_e = 1
) {
  
  # generate schools sizes 
  n_min <- round( n_bar * (1 - alpha) )
  n_max <- round( n_bar * (1 + alpha) )
  nj <- sample( n_min:n_max, J, replace = TRUE )
  
  # Generate average control outcome for all schools
  # (the random effects)
  u0j <- rnorm( J, mean = 0, sd = sqrt(sigma2_u) )
  
  # randomize schools (proportion p to treatment)
  Zj <- ifelse( sample( 1:J ) <= J * p, 1, 0)
  
  # Calculate schools intercepts
  S_j <- (nj - n_bar) / n_bar
  beta_0j <- gamma_0 + gamma_1 * Zj + gamma_2 * Zj * S_j + u0j
  
  # Make individual site membership
  sid <- as.factor( rep( 1:J, nj ) )
  
  # Generate the individual-level errors and outcome
  N <- sum( nj )
  e <- rnorm( N, mean = 0, sd = sqrt(sigma2_e) )
  Y <- beta_0j[sid] + e
  
  # Bundle into a dataset
  dd <- data.frame( 
    sid = sid,
    Z = Zj[ sid ],
    Yobs = Y
  )
  
  return(dd)
}
