# Final functions for the Cronbach alpha case study



library( tidyverse )
library( mvtnorm )


# Design parameters from the experimental design chapter
design_factors <- list(
  n = c(50, 100),
  p = c(4, 8),
  alpha = seq(0.1, 0.9, 0.1),
  df = c(5, 10, 20, 100)
)

params <- cross_df(design_factors)
params$iterations <- 500
params$seed <- 20170405 + 1:nrow(params)

#------------------------------------------------------
# Data Generating Model
#------------------------------------------------------

r_mvt_items <- function(n, p, alpha, df) {
  icc <- alpha / (p - alpha * (p - 1))
  V_mat <- icc + diag(1 - icc, nrow = p)
  X <- rmvt(n = n, sigma = V_mat, df = df)
  colnames(X) <- LETTERS[1:p]
  X
}


if ( FALSE) {
  
  
  #------------------------------------------------------
  # Set development values for simulation parameters
  #------------------------------------------------------
  
  # model parameters
  alpha <- 0.73 # true alpha
  df <- 12 # degrees of freedom
  
  # design parameters
  n <- 50 # sample size
  p <- 6 # number of items
  
  # Test the data-generating model
  
  big_sample <- r_mvt_items(n = 100000, p = 4, alpha = 0.73, df = 5)
  round(cor(big_sample), 3) # looks good
  qqplot(qt(ppoints(200), df = 5), big_sample[,2], ylim = c(-4,4))
  
}

#------------------------------------------------------
# Model-fitting/estimation/testing functions
#------------------------------------------------------

estimate_alpha <- function(dat, coverage = .95) {
  V <- cov(dat)
  p <- ncol(dat)
  n <- nrow(dat)
  A <- p / (p - 1) * (1 - sum(diag(V)) / sum(V))
  Var_A <- 2 * p * (1 - A)^2 / ((p - 1) * n)
  
  B <- log(1 - A) / 2
  SE_B <- sqrt(p / (2 * n * (p - 1)))
  z <- qnorm((1 - coverage) / 2)
  CI_B <- B + c(-1, 1) * SE_B * z
  CI_A <- 1 - exp(2 * CI_B)
  
  data.frame(A = A, Var_A = Var_A, CI_L = CI_A[1], CI_U = CI_A[2])
}

if ( FALSE ) {
  
  # Test the estimation function
  small_sample <- r_mvt_items(n = 50, p = 6, alpha = 0.73, df = 5)
  estimate_alpha(small_sample)
  
}


#------------------------------------------------------
# Calculate performance measures
# (For some simulations, it may make more sense
# to do this as part of the simulation driver.)
#------------------------------------------------------

alpha_performance <- function(alpha_sims, alpha, coverage_level = .95) {
  
  # setup
  K <- nrow(alpha_sims)
  A_err <- alpha_sims$A - alpha
  var_A <- var(alpha_sims$A)
  
  # bias
  A_bias <- mean(A_err)
  A_bias_MCSE <- sqrt(var_A / K)
  
  # RMSE
  A_RMSE <- sqrt(mean((A_err)^2))
  RMSE_j <- sqrt((A_RMSE^2 * K - A_err^2) / (K - 1))
  A_RMSE_MCSE <- sd(RMSE_j)
  
  # relative bias of variance estimator
  V_bar <- mean(alpha_sims$Var_A)
  V_j <- (V_bar * K - alpha_sims$Var_A) / (K - 1)
  Ssq_j <- ((K - 1) * var_A - A_err^2 * K / (K - 1)) / (K - 2)
  RB_j <- V_j / Ssq_j
  V_relbias <- V_bar / var_A
  V_relbias_MCSE <- sd(RB_j)
  
  # coverage
  coverage <- mean(alpha_sims$CI_L < alpha & alpha < alpha_sims$CI_U)
  coverage_MCSE <- sqrt(coverage_level * (1 - coverage_level) / K)
  
  data.frame(
    criterion = c("alpha bias","alpha RMSE", "V relative bias", "coverage"),
    est = c(A_bias, A_RMSE, V_relbias, coverage),
    MCSE = c(A_bias_MCSE, A_RMSE_MCSE, V_relbias_MCSE, coverage_MCSE)
  )
}

if ( FALSE ) {
  # Check performance calculations
  alpha_performance(alpha_sims, alpha = 0.73)
}


#-----------------------------------------------------------
# Simulation Driver - should return a data.frame or tibble
#-----------------------------------------------------------

run_alpha_sim <- function(iterations, n, p, alpha, df, coverage = 0.95, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  results <- 
    replicate(n = iterations, {
      dat <- r_mvt_items(n = n, p = p, alpha = alpha, df = df)
      estimate_alpha(dat, coverage = coverage)
    }, simplify = FALSE) %>%
    bind_rows()
  
  alpha_performance(results, alpha = alpha, coverage = coverage)
}

if ( FALSE ) {
  
  # demonstrate the simulation driver
  run_alpha_sim(iterations = 1000, n = 50, p = 6, alpha = 0.73, df = 5)
  
}

