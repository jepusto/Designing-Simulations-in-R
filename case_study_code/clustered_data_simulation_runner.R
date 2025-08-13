

# Secret Run code in parallel for speedup
library( tidyverse )
library( future )
library( furrr )

source( "case_study_code/clustered_data_simulation.R")

crt_design_factors <- list(
  n_bar = c( 20, 80, 320 ),
  J = c( 5, 20, 80 ),
  ATE = c( 0.2 ),
  size_coef = c( 0, 0.2 ),
  ICC = c( 0, 0.2, 0.4, 0.6, 0.8 ),
  alpha = c( 0, 0.5, 0.8 )
)

params <- expand_grid(!!!crt_design_factors)

params$seed = 1:nrow(params) * 17 + 100000
params

plan(multisession, workers = parallel::detectCores() - 2)
params$res = future_pmap(params, .f = run_CRT_sim,
                         reps = 1000,
                         .options = furrr_options(seed = NULL),
                         .progress = TRUE )
plan(sequential)
gc()
res <- params %>% unnest( cols=c(res) )
saveRDS( res, file = "results/simulation_CRT.rds" )


if ( FALSE ) {
  
  # Looking more deeply at alpha = 0, size_coef = 0 

  crt_design_factors <- list(
    n_bar = c( 20, 80, 320 ),
    J = c( 5 ),
    ATE = c( 0.2 ),
    size_coef = c( 0 ),
    ICC = c( 0, 0.2, 0.4, 0.6, 0.8 ),
    alpha = c( 0 )
  )
  
  params <- expand_grid(!!!crt_design_factors)
  params
  
  params$seed = 1:nrow(params) * 157 + 104400240
  params
  
  tictoc::tic()
  plan(multisession, workers = parallel::detectCores() - 2)
  params$res = future_pmap(params, .f = run_CRT_sim,
                           reps = 1000,
                           .options = furrr_options(seed = NULL),
                           .progress = TRUE )
  plan(sequential)
  gc()
  res <- params %>% unnest( cols=c(res) )
  tictoc::toc()
  saveRDS( res, file = "results/simulation_CRT_null.rds" )
  
    sres <- res %>% 
    group_by( n_bar, J, ATE, size_coef, ICC, alpha, method ) %>%
    summarise( calc_absolute( estimates = ATE_hat,
                              true_param = ATE,
                              criteria = c("bias","stddev",
                                           "rmse")),
               calc_relative_var( estimates = ATE_hat,
                                  var_estimates = SE_hat^2,
                                  criteria = "relative rmse" ),
               power = mean( p_value <= 0.05 ),
               ESE_hat = sqrt( mean( SE_hat^2 ) ),
               SD_SE_hat = sqrt( sd( SE_hat^2 ) ),
               
    ) %>%
    rename( R = K_absolute,
            RMSE = rmse,
            RMSE_mcse = rmse_mcse,
            SE = stddev,
            SE_mcse = stddev_mcse ) %>%
    dplyr::select(  -K_relvar )
  sres
  
  ggplot( sres,
          aes( x = ICC, y = bias, color = as.factor(method), group = as.factor(method) ) ) +
    facet_wrap( ~n_bar ) +
    geom_line() + geom_point() +
    geom_hline( yintercept = 0 ) +
    geom_errorbar( aes( ymin = bias - 1.96 * bias_mcse,
                       ymax = bias + 1.96 * bias_mcse ),
                   width = 0.0 ) 

  
}