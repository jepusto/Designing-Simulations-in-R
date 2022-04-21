
```{r case_study_addendum, include=FALSE, eval=FALSE}

# Not sure what this block of code is for?

generate_data <- function(mu, sigma_sq, sample_size) {
  
  N <- sum(sample_size)
  g <- length(sample_size)
  
  group <- rep(1:g, times = sample_size)
  mu_long <- rep(mu, times = sample_size)
  sigma_long <- rep(sqrt(sigma_sq), times = sample_size)
  
  x <- rnorm(N, mean = mu_long, sd = sigma_long)
  sim_data <- data.frame(group = group, x = x)
  
  return(sim_data)
}


ANOVA_F_direct <- function(sim_data) {
  
  x_bar <- with(sim_data, tapply(x, group, mean))
  s_sq <- with(sim_data, tapply(x, group, var))
  n <- table(sim_data$group)
  g <- length(x_bar)
  
  df1 <- g - 1
  df2 <- sum(n) - g
  
  msbtw <- sum(n * (x_bar - mean(sim_data$x))^2) / df1
  mswn <- sum((n - 1) * s_sq) / df2
  fstat <- msbtw / mswn
  pval <- pf(fstat, df1, df2, lower.tail = FALSE)
  
  return(pval)
}


ANOVA_F_aov <- function(sim_data) {
  oneway_anova <- oneway.test(x ~ factor(group), data = sim_data, var.equal = TRUE)
  return(oneway_anova$p.value)
}


tictoc::tic()
purrr::rerun( 1000, {
  dat = generate_data( 1:4, 1:4, c(10,20,30,40) )
  dd = ANOVA_F_direct(dat)
})
tictoc::toc()


tictoc::tic()
purrr::rerun( 1000, {
  dat = generate_data( 1:4, 1:4, c(10,20,30,40) )
  dd = ANOVA_F_aov(dat)
})
tictoc::toc()

```