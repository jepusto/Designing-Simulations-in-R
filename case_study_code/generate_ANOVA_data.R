generate_ANOVA_data <- function(mu, sigma_sq, sample_size) {
  
  N <- sum(sample_size)
  G <- length(sample_size)
  
  group <- factor(rep(1:G, times = sample_size))
  mu_long <- rep(mu, times = sample_size)
  sigma_long <- rep(sqrt(sigma_sq), times = sample_size)
  
  x <- rnorm(N, mean = mu_long, sd = sigma_long)
  sim_data <- tibble(group = group, x = x)
  
  return(sim_data)
}