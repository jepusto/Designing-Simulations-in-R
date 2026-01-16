analysis_MLM <- function( dat ) {
  
  M1_test <- lmerTest::lmer( Yobs ~ 1 + Z + (1 | sid), data = dat )
  M1_summary <- summary(M1_test)$coefficients
  
  tibble( 
    ATE_hat = M1_summary["Z","Estimate"], 
    SE_hat = M1_summary["Z","Std. Error"], 
    p_value = M1_summary["Z", "Pr(>|t|)"] 
  )
  
}

analysis_OLS <- function( dat, se_type = "CR2" ) {
  
  M2 <- estimatr::lm_robust( 
    Yobs ~ 1 + Z, data = dat, 
    clusters = sid,  se_type = se_type
  )
  
  tibble( 
    ATE_hat = M2$coefficients[["Z"]], 
    SE_hat = M2$std.error[["Z"]], 
    p_value = M2$p.value[["Z"]] 
  )
}

analysis_agg <- function( dat, se_type = "HC2" ) {
  
  datagg <- dplyr::summarise( 
    dat,
    Ybar = mean( Yobs ),
    n = n(),
    .by = c(sid, Z)
  )
  
  stopifnot( nrow( datagg ) == length(unique(dat$sid) ) )
  
  M3 <- estimatr::lm_robust( 
    Ybar ~ 1 + Z, data = datagg, 
    se_type = se_type 
  )
  
  tibble( 
    ATE_hat = M3$coefficients[["Z"]], 
    SE_hat = M3$std.error[["Z"]], 
    p_value = M3$p.value[["Z"]] 
  )
}

estimate_Tx_Fx <- function(
    data, 
    CR_se_type = "CR2", agg_se_type = "HC2"
) {
  
  dplyr::bind_rows(
    MLM = analysis_MLM( data ),
    OLS = analysis_OLS( data, se_type = CR_se_type),
    agg = analysis_agg( data, se_type = agg_se_type),
    .id = "estimator"
  )
  
}


quiet_safe_lmer <- quietly( possibly( lmerTest::lmer, otherwise=NULL ) )

analysis_MLM_safe <- function( dat, all_results = FALSE ) {
  
  M1 <- quiet_safe_lmer( Yobs ~ 1 + Z + (1 | sid), data=dat )
  
  if (all_results) {
    return(M1)
  } 
  
  if ( is.null( M1$result ) ) {
    # we had an error!
    tibble( ATE_hat = NA, SE_hat = NA, p_value = NA,
            message = M1$message,
            warning = M1$warning,
            error = TRUE )
  } else {
    sum <- summary( M1$result )
    tibble( 
      ATE_hat = sum$coefficients["Z","Estimate"], 
      SE_hat = sum$coefficients["Z","Std. Error"], 
      p_value = sum$coefficients["Z", "Pr(>|t|)"],
      message = list( M1$message ),
      warning = list( M1$warning ),
      error = FALSE )
  }
  
}



analysis_MLM_contingent <- function( dat ) {
  
  M1 <- quiet_safe_lmer( Yobs ~ 1 + Z + (1 | sid), data=dat )
  
  if (!is.null(M1$result)) { 
    sum <- summary( M1$result )
    tibble( 
      ATE_hat = sum$coefficients["Z","Estimate"], 
      SE_hat = sum$coefficients["Z","Std. Error"], 
      p_value = sum$coefficients["Z", "Pr(>|t|)"] )
  } else {
    # If lmer() errors, fall back on OLS
    M_ols <- summary(lm(Yobs ~ Z, data = dat))
    res <- tibble( 
      ATE_hat = M_ols$coefficients["Z","Estimate"], 
      SE_hat = M_ols$coefficients["Z", "Std. Error"], 
      p_value = M_ols$coefficients["Z","Pr(>|t|)"] )
  }

  # Store original messages, warnings, errors  
  res$message <- ifelse( length( M1$message ) > 0, M1$message, NA_character_ )
  res$warning <- ifelse( length( M1$warning ) > 0, M1$warning, NA_character_ )
  res$error <- is.null( M1$result )

  return(res)
}
