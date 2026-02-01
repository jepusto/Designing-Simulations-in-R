
# This script runs the multifactor simulation in parallel to generate
# the result files used in the book.

# It is sourced in the Secret Run code block in the designing
# multifactor simulation area.

# You can also run it directly here.

library( tidyverse )
library( future )
library( furrr )

source( "case_study_code/clustered_data_simulation.R")


# Updated MLM analysis with max iterations + flags for convergence / maxfun hit
analysis_MLM_safe <- function( dat, all_results = FALSE, maxfun = 1000 ) {
  
  M1 <- quiet_safe_lmer( Yobs ~ 1 + Z + (1 | sid),
                         data=dat,
                         control = lme4::lmerControl(
                           optimizer = "bobyqa",
                           optCtrl = list(maxfun = maxfun)
                         ) )
  
  if (all_results) {
    return(M1)
  } 
  
  if ( is.null( M1$result ) ) {
    # we had an error!
    tibble( ATE_hat = NA, SE_hat = NA, p_value = NA,
            message = length( M1$message ) > 0,
            warning = length( M1$warning ) > 0,
            error = TRUE )
  } else {
    fit = M1$result
    sum <- summary( fit )
    
    optinfo <- fit@optinfo
    
    feval = optinfo$feval
    
    #if ( length( optinfo$conv$lme4 ) > 0 ||
    #     length( optinfo$messages ) > 0 ) {
    #  browser()
    #}
    
    tibble( 
      ATE_hat = sum$coefficients["Z","Estimate"], 
      SE_hat = sum$coefficients["Z","Std. Error"], 
      p_value = sum$coefficients["Z", "Pr(>|t|)"],
      message = length( M1$messages ) > 0,
      warning = length( M1$warning ) > 0 ,
      error = FALSE,
      hit_maxfun = feval >= maxfun
    )
    
  }
}




run_CRT_sim <- function( reps, 
                         n_bar = 10, J = 30, p = 0.5,
                         ATE = 0, ICC = 0.4,
                         size_coef = 0, alpha = 0,
                         seed = NULL ) {
  
  a = Sys.time()
  stopifnot( ICC >= 0 && ICC < 1 )
  
  scat( "Running n=%d, J=%d, ICC=%.2f, ATE=%.2f (%d replicates)\n",
        n_bar, J, ICC, ATE, reps)
  
  if (!is.null(seed)) set.seed(seed)
  
  res <- 
    simhelpers::repeat_and_stack( reps, {
      dat <- gen_cluster_RCT( n_bar = n_bar, J = J, p = p,
                              gamma_0 = 0, gamma_1 = ATE, gamma_2 = size_coef,
                              sigma2_u = ICC, sigma2_e = 1 - ICC,
                              alpha = alpha )
      quiet_analyze_data(dat)
    },
    id = "runID" )
  
  res$total_time <- as.numeric( Sys.time() - a, units="secs" )
  
  res
}




if ( FALSE ) {
  # Test the updated analysis function
  set.seed( 40440 )
  test_dat <- gen_cluster_RCT( n_bar = 320,
                               J = 80,
                               gamma_1 = 0.2,
                               gamma_2 = 0.2,
                               sigma2_u = 0,
                               alpha = 0.5 )
  
  debug( analysis_MLM_safe )
  analysis_MLM_safe( test_dat )
  
  quiet_analyze_data( test_dat )
  
  rr <- run_CRT_sim( reps = 5,
                     n_bar = 320,
                     J = 30,
                     ATE = 0.2,
                     size_coef = 0.2,
                     ICC = 0,
                     alpha = 0.5,
                     seed = 1234567)
  rr  
  rr$total_time[[1]] - sum( rr$seconds)
}



# Run the simulation ----


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

set.seed( 404450 )
params = slice_sample( params, n=nrow(params) )
params


start_time = Sys.time()

plan(multisession, workers = parallel::detectCores() - 2)

cat( "Starting simulation\n" )

R = 1000
params$res = future_pmap(params, .f = run_CRT_sim,
                         reps = R,
                         .options = furrr_options(seed = NULL),
                         .progress = TRUE )
plan(sequential)
gc()
res <- params %>% unnest( cols=c(res) )
saveRDS( res, file = "results/simulation_CRT_timing.rds" )

end_time = Sys.time()

cat( "Total computation time:\n" )
print( end_time - start_time )


# Analyze timing results ----


res
nrow( params )
nrow( res )
nrow( res ) / nrow( params )

mt = mean( res$seconds )
mt

# Estimated total time in hours for full 1000 reps
comp_time <- mt * nrow(params) * 3 * 1000 / (60*60)
comp_time

# Across 6 threads (est.)
comp_time / 5

# Timing for longest runs

rs <- res %>%
  group_by( n_bar, J, ATE, size_coef, ICC, alpha, method ) %>%
  summarise( Etime = mean( seconds ) * 1000 / 60,
             ttime = total_time[1],
             sdttime = sd( total_time ),
             conv = mean( hit_maxfun ) )

ggplot( rs, aes( method, Etime ) ) +
  geom_boxplot() +
  coord_flip()


sum( rs$Etime >= 30 )

# Any failure to converge?
rs$conv[ rs$method == "MLM" ]


ggplot( rs, aes( as.factor(n_bar), Etime, color=as.factor(J), group=interaction( J, n_bar ) ) ) +
  geom_boxplot( width = 0.5 ) +
  #geom_smooth() +
  facet_wrap( ~ method ) +
  #geom_line( aes( group=method ) ) +
  #facet_grid( ICC ~ J, scales="free_y" ) +
  scale_y_log10( breaks =c( 0.1, 1, 10, 30, 60 ) ) + 
  theme_minimal()

# As above, but swap n_bar and J
ggplot( rs, aes( as.factor(J), Etime, color=as.factor(n_bar), group=interaction( J, n_bar ) ) ) +
  geom_boxplot( width = 0.5 ) +
  #geom_smooth() +
  facet_wrap( ~ method ) +
  #geom_line( aes( group=method ) ) +
  #facet_grid( ICC ~ J, scales="free_y" ) +
  scale_y_log10( breaks =c( 0.1, 1, 10, 30, 60 ) ) + 
  theme_minimal()

# ICC matters?
ggplot( rs, aes( ICC, Etime, color=as.factor(n_bar), lty=as.factor(J),
                 group=interaction( J, n_bar ) ) ) +
  geom_smooth( se = FALSE, method="lm" ) +
  facet_wrap( ~ method ) +
  #geom_line( aes( group=method ) ) +
  #facet_grid( ICC ~ J, scales="free_y" ) +
  scale_y_log10( breaks =c( 0.1, 1, 10, 30, 60 ) ) + 
  theme_minimal()


# Calculate _total_ time to run simulation ----

rst <- res %>%
  group_by( n_bar, J, ATE, size_coef, ICC, alpha ) %>%
  summarise( ttime = total_time[1],
             sdttime = sd( total_time ),
             dtime = ttime - sum( seconds ) )

nrow( rst )
sum( rst$ttime ) / (60*60)

rst$sdttime
summary( rst$dtime )

nrow( rst )

# Time for single iteration for all (minutes)
single_iter <- sum( rst$ttime / (R*60) )
single_iter

# Total sim, in hours
1000*single_iter / 60


3 * 4 * 60 / single_iter



# Save to core results file ----

res$hit_maxfun = NULL
res$total_time = NULL
res
saveRDS( res, file = "results/simulation_CRT.rds" )


if ( FALSE ) {
  # check sim format
  run_CRT_sim( reps = 5,
               n_bar = 20,
               J = 5,
               ATE = 0.2,
               size_coef = 0.2,
               ICC = 0,
               alpha = 0.5,
               seed = 1234567)
  
  
  # Check the results
  res = readRDS( "results/simulation_CRT.rds" )
  res
}



