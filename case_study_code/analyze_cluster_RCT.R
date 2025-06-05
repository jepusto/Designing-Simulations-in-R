analysis_MLM <- function( dat ) {
  require(lme4)
  require(lmerTest)
  
  M1 <- lmer( Yobs ~ 1 + Z + (1 | sid), data = dat )
  M1_summary <- summary(M1)$coefficients
  
  tibble( 
    ATE_hat = M1_summary["Z","Estimate"], 
    SE_hat = M1_summary["Z","Std. Error"], 
    p_value = M1_summary["Z", "Pr(>|t|)"] 
  )
}

analysis_OLS <- function( dat ) {
  require(estimatr)
  M2 <- lm_robust( Yobs ~ 1 + Z, data=dat, clusters=sid )
  
  tibble( 
    ATE_hat = M2$coefficients[["Z"]], 
    SE_hat = M2$std.error[["Z"]], 
    p_value = M2$p.value[["Z"]] 
  )
}

analysis_agg <- function( dat ) {
  require(dplyr)
  require(estimatr)
  
  datagg <- 
    dat %>% 
    group_by( sid, Z ) %>%
    summarise( 
      Ybar = mean( Yobs ),
      n = n(),
      .groups = "drop"
    )
  
  stopifnot( nrow( datagg ) == length(unique(dat$sid) ) )
  
  M3 <- lm_robust( Ybar ~ 1 + Z, data=datagg, se_type = "HC2" )
  
  tibble( 
    ATE_hat = M3$coefficients[["Z"]], 
    SE_hat = M3$std.error[["Z"]], 
    p_value = M3$p.value[["Z"]] 
  )
}
