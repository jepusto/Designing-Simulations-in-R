
# The final functions from the clustered RCT case study

library( tidyverse )
library( lme4 )
library( arm )
library( lmerTest )
library( estimatr )


options(list(dplyr.summarise.inform = FALSE))




# Utility function for printout
scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}


gen_dat_model <- function( n_bar = 10,
                           J = 30,
                           p = 0.5,
                           gamma_0 = 0, gamma_1 = 0, gamma_2 = 0,
                           sigma2_u = 0, sigma2_e = 1,
                           alpha = 0 ) {
  
  stopifnot( alpha >= 0 )
  
  # generate site sizes 
  n_min = round( n_bar * (1 - alpha) )
  n_max = round( n_bar * (1 + alpha) )
  if ( n_max == n_min ) {
    if ( alpha > 0 ) {
      warning( "alpha > 0 has no effect when there is no variation in site size" )
    }
    nj = rep( n_min, J )
  } else {
    nj <- sample( n_min:n_max, J, replace=TRUE )
  }
  
  # Generate average control outcome and average ATE for all sites
  # (The random effects)
  u0j = rnorm( J, mean=0, sd=sqrt( sigma2_u ) )
  
  # randomize units within each site (proportion p to treatment)
  Zj = ifelse( sample( 1:J ) <= J * p, 1, 0)
  
  # Calculate site intercept for each site
  beta_0j = gamma_0 + gamma_1 * Zj + gamma_2 * Zj * (nj-n_bar)/n_bar + u0j
  
  # Make individual site membership
  sid = as.factor( rep( 1:J, nj ) )
  dd = data.frame( sid = sid )
  
  # Make individual level tx variables
  dd$Z = Zj[ dd$sid ]
  
  # Generate the residuals 
  N = sum( nj )
  e = rnorm( N, mean=0, sd=sqrt( sigma2_e ) )
  
  # Bundle and send out
  dd <- mutate( dd, 
                sid=as.factor(sid),
                Yobs = beta_0j[sid] + e, 
                Z = Zj[ sid ] )
}



#### Analysis functions #####


quiet_lmer = purrr::quietly( lmer )

quiet_analysis_MLM <- function( dat ) {
  M1 = quiet_lmer( Yobs ~ 1 + Z + (1|sid),
                   data=dat )
  message1 = ifelse( length( M1$message ) > 0, 1, 0 )
  warning1 = ifelse( length( M1$warning ) > 0, 1, 0 )
  M1 = M1$result
  
  est = fixef( M1 )[["Z"]]
  se = se.fixef( M1 )[["Z"]]
  pv = summary(M1)$coefficients["Z",5]
  tibble( ATE_hat = est, SE_hat = se, p_value = pv,
          message = message1, warning = warning1 )
}

analysis_MLM <- function( dat ) {
  M1 = lmer( Yobs ~ 1 + Z + (1|sid),
                   data=dat )
  
  est = fixef( M1 )[["Z"]]
  se = se.fixef( M1 )[["Z"]]
  pv = summary(M1)$coefficients["Z",5]
  tibble( ATE_hat = est, SE_hat = se, p_value = pv )
}

analysis_OLS <- function( dat ) {
  M2 <- lm_robust( Yobs ~ 1 + Z, 
                   data=dat, clusters=sid )
  est <- M2$coefficients[["Z"]]
  se  <- M2$std.error[["Z"]]
  pv <- M2$p.value[["Z"]]
  tibble( ATE_hat = est, SE_hat = se, p_value = pv )
}


analysis_agg <- function( dat ) {
  datagg <- 
    dat %>% 
    group_by( sid, Z ) %>%
    summarise( 
      Ybar = mean( Yobs ),
      n = n(),
      .groups = "drop"
    )
  
  stopifnot( nrow( datagg ) == length(unique(dat$sid) ) )
  
  M3 <- lm_robust( Ybar ~ 1 + Z, 
                   data=datagg, se_type = "HC2" )
  est <- M3$coefficients[["Z"]]
  se <- M3$std.error[["Z"]]
  pv <- M3$p.value[["Z"]]
  tibble( ATE_hat = est, SE_hat = se, p_value = pv )
}



analyze_data = function( dat ) {
  MLM = analysis_MLM( dat )
  LR = analysis_OLS( dat )
  Agg = analysis_agg( dat )
  
  bind_rows( MLM = MLM, LR = LR, Agg = Agg,
             .id = "method" )
}


quiet_analyze_data = function( dat ) {
  MLM = quiet_analysis_MLM( dat )
  LR = analysis_OLS( dat )
  Agg = analysis_agg( dat )
  LR$message = 0
  LR$warning = 0
  Agg$message = 0
  Agg$warning = 0
  bind_rows( MLM = MLM, LR = LR, Agg = Agg,
             .id = "method" )
}


run_CRT_sim <- function(reps, 
                        n_bar = 10, J = 30, p = 0.5,
                        ATE = 0, ICC = 0.4,
                        size_coef = 0, alpha = 0,
                        seed = NULL, aggregate = TRUE) {
  
  stopifnot( ICC >= 0 && ICC < 1 )
  
  scat( "Running n=%d, J=%d, ICC=%.2f, ATE=%.2f (%d replicates)\n", n_bar, J, ICC, ATE, reps)
  
  if (!is.null(seed)) set.seed(seed)
  
  res <- 
    simhelpers::repeat_and_stack( reps, {
      dat <- gen_dat_model( n_bar = n_bar, J = J, p = p,
                            gamma_0 = 0, gamma_1 = ATE, gamma_2 = size_coef,
                            sigma2_u = ICC, sigma2_e = 1 - ICC,
                            alpha = alpha )
      quiet_analyze_data(dat)
    }) %>%
    bind_rows( .id="runID" )
}


if ( FALSE ) {
  
  dat <- gen_dat_model( 5, 3 )
  dat
  run_CRT_sim( reps = 5 )
}

