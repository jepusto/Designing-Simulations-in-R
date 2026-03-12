
# The final functions from the clustered RCT case study

library( tidyverse )
library( lme4 )
library( arm )
library( lmerTest )
library( estimatr )


options(list(dplyr.summarise.inform = FALSE))


source( here::here( "case_study_code/gen_cluster_RCT.R" ) )

source( here::here( "case_study_code/analyze_cluster_RCT.R" ) )


# Utility function for printout
scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}



quiet_analyze_data <- function(dat,
                               CR_se_type = "CR2",
                               agg_se_type = "HC2") {
  
  # MLM
  a = Sys.time()
  MLM <- analysis_MLM_safe(dat)
  MLM$seconds <- as.numeric(Sys.time() - a, units = "secs")
  
  # OLS
  a = Sys.time()
  LR <- analysis_OLS(dat, se_type = CR_se_type)
  LR$seconds <- as.numeric(Sys.time() - a, units = "secs")
  LR$message <- 0
  LR$warning <- 0
  LR$error <- 0
  
  # Aggregated
  a = Sys.time()
  Agg <- analysis_agg(dat, se_type = agg_se_type)
  Agg$seconds <- as.numeric(Sys.time() - a, units = "secs")
  Agg$message <- 0
  Agg$warning <- 0
  Agg$error <- 0
  
  bind_rows(MLM = MLM, LR = LR, Agg = Agg, .id = "method")
}




run_CRT_sim <- function( reps, 
                         n_bar = 10, J = 30, p = 0.5,
                         ATE = 0, ICC = 0.4,
                         size_coef = 0, alpha = 0,
                         seed = NULL ) {
  
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
    }) %>%
    bind_rows( .id="runID" )
  
  res
}


if ( FALSE ) {
  
  
  dat <- gen_cluster_RCT( 5, 3 )
  dat
  
  quiet_analyze_data( dat )
  
  res <- run_CRT_sim( reps = 5, alpha=0.5 )
  res
  
}

