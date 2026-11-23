library( tidyverse )
library( simhelpers )

res <- readRDS( file = "results/simulation_CRT.rds" )
head( res )

sres <- 
  res |> 
  summarise( 
    .by = c(n_bar, J, ATE, size_coef, ICC, alpha, method),
    calc_absolute( 
      estimates = ATE_hat, true_param = ATE,
      criteria = c("bias","stddev", "rmse")
    ),
    calc_relative_var( 
      estimates = ATE_hat, var_estimates = SE_hat^2,
      criteria = "relative bias" 
    ),
    power = mean( p_value <= 0.05 ),
    ESE_hat = mean( SE_hat ),
    SD_SE_hat = sd( SE_hat ),
  ) |>
  mutate(
    clusters = as.factor(J),
    alpha = factor(alpha)
  ) |> 
  rename( 
    R = K_absolute,
    RMSE = rmse,
    RMSE_mcse = rmse_mcse,
    SE = stddev,
    SE_mcse = stddev_mcse 
  ) |>
  dplyr::select( -K_relvar ) |>
  ungroup()

str( sres )
