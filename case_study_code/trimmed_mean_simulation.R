
library( tidyverse )
library( simhelpers )


library(sn)

# Sample data from a skewed t-distribution
gen.data <- function(n, df0, skew) {
  
  # Get mean and SD of a skewed t-distribution
  mvar =  st.cumulants( xi = 0, omega=1, alpha=skew, nu = df0, n = 2 )
  
  rs <- rst(n, xi = 0, omega = 1, alpha = skew, nu = df0)
  rs = (rs - mvar[[1]]) / sqrt( mvar[[2]] )
  
  as.numeric( rs )
}

if ( FALSE ) {
  gen.data( 5, 4, 100 )
  
  
  A = gen.data( 100000, df=5, skew = 0 )
  B = gen.data( 100000, df=30, skew = 0 )
  df0 = 5
  C = rt( 100000, df=5 ) / sqrt( df0 / (df0-2) )  # Normal t-distribution
  quantile( A, c(0.1, 0.90 ) )  
  quantile( B, c(0.1, 0.90 ) )  
  quantile( C, c(0.1, 0.90 ) )
  
  df = bind_cols( A = A, B = B ) %>%
    pivot_longer( cols=c(A,B), names_to="group", values_to="value" )
  
  ggplot( df, aes( x=value, fill=group ) ) +
    geom_histogram( position="identity", alpha=0.5, bins=50 ) +
    facet_wrap( ~group, ncol=1 ) +
    labs( title="Skewed t-distribution samples",
          x="Value", y="Count" ) +
    theme_minimal()
}

# Estimator code
analyze.data = function( data ) {
  mn = mean( data )
  md = median( data )
  mn.tr = mean( data, trim=0.1 )
  data.frame( estimator = c( "mean", "trim.mean", "median" ),
              estimate = c( mn, mn.tr, md ) )
}

if ( FALSE ) {
  analyze.data( gen.data( 10, 5, 100 ) )
  
}

# Performance metric code
estimator.quality = function( estimates, mu ) {
  RMSE = sqrt( mean( (estimates - mu)^2 ) )
  bias = mean( estimates - mu )
  SE = sd( estimates )
  data.frame( RMSE=RMSE, bias=bias, SE=SE )
}

analyze_results <- function( raw.exps ) {
  rs <- raw.exps %>%
    group_by( estimator ) %>%
    summarise( qual = estimator.quality( estimate, mu = 0 ) ) %>%
    tidyr::unpack( cols=c( qual ) )
}

# Full simulation code 
run.simulation <- bundle_sim( gen.data, analyze.data, analyze_results,
                              id="runID" )

args( run.simulation )


if ( FALSE ) {
  run_sim <- bundle_sim( gen.data, analyze.data, id="runID" )
  run_sim( 5, 5, skew = 100 )
  
  
  run.simulation( 10, 5, 0, 0 )
}

ns = c( 10, 20, 40, 80, 320, 320*4 )
dfs = c( 3, 5, 15, 30 )
skew = c( 0, 10 )
lvls = expand_grid( n=ns, df0=dfs, skew = skew )
rm( ns, dfs, skew )

# Run multifactor simulation ----

if ( FALSE ) {
  # Source this code to generate the simulation results and save to
  # file.
 
  
  lvls$seed = 17 * 1:nrow(lvls) + 42242442
  
  results <- lvls %>% 
    mutate( results = pmap( lvls, run.simulation, reps = 10000,
                            .progress=TRUE ) 
    ) %>%
    unnest( cols="results" )
  results
  
  saveRDS( results, here::here( "results/skewed_t_simulation.rds" ) )
  
}
