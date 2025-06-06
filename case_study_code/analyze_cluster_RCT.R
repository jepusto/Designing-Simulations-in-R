analysis_MLM <- function( dat ) {
  
  M1 <- lme4::lmer( Yobs ~ 1 + Z + (1 | sid), data = dat )
  M1_test <- lmerTest::as_lmerModLmerTest(M1)
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
    MLM = analysis_MLM( dat ),
    OLS = analysis_OLS( dat, se_type = CR_se_type),
    agg = analysis_agg( dat, se_type = agg_se_type),
    .id = "estimator"
  )
  
}
