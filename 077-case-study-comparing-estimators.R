
library( tidyverse )
library( simhelpers )


# DGP function
gen.data = function( n, mu, df0 ) {
    mu + rt( n, df=df0 ) / sqrt( df0 / (df0-2) )
  
  
  gen.data.outliers = function( n, prob.outlier = 0.05 ) {
    nN = rbinom( 1, n, prob.outlier )
    nrm = rnorm( n - nN, mean=0.5, sd=1 )
    outmean = (1 - (1-prob.outlier)/2) / prob.outlier
    outs = rnorm( nN, mean=outmean, sd=10 )
    c( nrm, outs )
  }
  
  
}

# Estimator code
analyze.data = function( data ) {
    mn = mean( data )
    md = median( data )
    mn.tr = mean( data, trim=0.1 )
    data.frame( estimator = c( "mean", "trim.mean", "median" ),
                estimate = c( mn, mn.tr, md ) )
}


# Performance metric code
estimator.quality = function( estimates, mu ) {
    RMSE = sqrt( mean( (estimates - mu)^2 ) )
    bias = mean( estimates - mu )
    SE = sd( estimates )
    data.frame( RMSE=RMSE, bias=bias, SE=SE )
}


# Full simulation code 
run.simulation = function( n, df0, mu = 0 ) {
  run_sim <- bundle_sim( gen.data, analyze.data, id="runID" )
  raw.exps <- run_sim( 1000, n=n, df0=df0, mu = mu )

    rs <- raw.exps %>%
        group_by( estimator ) %>%
        summarise( qual = estimator.quality( estimate, mu = mu ) ) %>%
        tidyr::unpack( cols=c( qual ) )

    rs
}

run.simulation( 10, 5 )


# Run multifactor simulation
ns = c( 10, 50, 250, 1250 )
dfs = c( 3, 5, 15, 30 )
lvls = expand_grid( n=ns, df=dfs )

results <- lvls %>% 
  mutate( results = pmap( lvls, run.simulation ) ) %>%
  unnest( cols="results" )
results


run.simulation.exp = function( n ) {
    raw.exps <- replicate( 1000, {
        dt = gen.data.outliers( n=n )
        analyze.data( dt )
    }, simplify = FALSE )
    raw.exps = bind_rows( raw.exps, .id = "runID" )

    rs <- raw.exps %>%
        group_by( estimator ) %>%
        summarise( qual = estimator.quality( estimate, mu = 1 ) ) %>%
        tidyr::unpack( cols = c( qual ) )

    rs
}

res = run.simulation.exp( 100 )
res


## ---------------------------------------------------------------------------------------------
ns = c( 10, 20, 40, 80, 160, 320 )
lvls = tibble( n=ns )


## ----expoExperiment, cache=TRUE---------------------------------------------------------------
results <- lvls %>% 
  mutate( results = pmap( lvls, run.simulation.exp ) ) %>% 
  unnest( cols = c(results) )
head( results )


## ---------------------------------------------------------------------------------------------
res2 = gather( results, RMSE, bias, SE, key="Measure",value="value" )
res2 = mutate( res2, Measure = factor( Measure, levels=c("SE","bias","RMSE" )))


## ----fig.height=4-----------------------------------------------------------------------------
ggplot( res2, aes(x=n, y=value, col=estimator) ) +
    facet_grid( . ~ Measure ) +
    geom_hline( yintercept=0, col="darkgrey" ) +
    geom_line() + geom_point() +
    scale_x_log10( breaks=ns ) +
    labs( y="" )

