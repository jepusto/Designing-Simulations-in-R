

# Looking at how to viz simulation results and deal with
# heteroskedasticity in the MCSEs.

# E.g., by the "MCSE Funnel Plot" and by using meta analysis to get
# shrunk estimates of performance

library( tidyverse )

#### Load the ClusterRCT sim results and calc performance metrics ####

source( here::here( "case_study_code/clustered_data_simulation.R" ) )
source( here::here( "case_study_code/cronbach_alpha_simulation.R" ) )

res <- readRDS( file = here::here( "results/simulation_CRT.rds" ) )
res


# Cut down to 100 reps to make MCSE even larger (optional)
res <- res %>%
  filter( as.numeric(runID) <= 100 )
res


library( simhelpers )
sres <- 
  res %>% 
  group_by( 
    n_bar, J, ATE, size_coef, ICC, alpha, method 
  ) %>%
  summarise( 
    calc_absolute( estimates = ATE_hat, true_param = ATE,
                   criteria = c("bias","stddev", "rmse")),
    calc_relative_var( estimates = ATE_hat, var_estimates = SE_hat^2,
                       criteria = "relative bias" ),
    power = mean( p_value <= 0.05 ),
    ESE_hat = sqrt( mean( SE_hat^2 ) ),
    SD_SE_hat = sqrt( sd( SE_hat^2 ) ),
  ) %>%
  rename( 
    R = K_absolute,
    RMSE = rmse,
    RMSE_mcse = rmse_mcse,
    SE = stddev,
    SE_mcse = stddev_mcse 
  ) %>%
  dplyr::select(  -K_relvar ) %>%
  ungroup()

sres


#### The meta analysis funnel plots ####

# For Bias

ggplot( sres, aes( bias_mcse, bias, col=as.factor(size_coef) )) +
  facet_grid( alpha ~ method ) + 
  geom_point() +
  geom_abline( slope=2, intercept=0, col="darkgrey", lty=2 ) +
  geom_abline( slope=-2, intercept=0, col="darkgrey", lty=2 ) +
  theme_minimal()

summary( sres$bias_mcse )


# For SE 

# NOTE: Not useful since we don't expect these SE values to be
# centered around any common value---we would need to somehow subtract
# out their expected values to see the residuals scatter, I think?
ggplot( sres, aes( SE_mcse, SE, col=as.factor(size_coef) )) +
  facet_grid( alpha ~ method ) + 
  geom_point() +
  theme_minimal()

summary( sres$SE_mcse / sres$SE )


# Is smoothing and then looking at residuals better?

# This would be asking: Given reasonablly precise SE_mcse estimates,
# we have a sense of what the true SE should be, roughly.  We then see
# if the deviation from that is larger than expected?
M_se = loess( SE ~ SE_mcse, data=sres )
sres$SE_fitted = predict( M_se )
sres$SE_resid = sres$SE - sres$SE_fitted
summary( sres$SE_resid )

ggplot( sres, aes( SE_mcse, SE_resid, col=as.factor(size_coef) )) +
  facet_grid( alpha ~ method ) + 
  geom_point() +
  geom_abline( slope=2, intercept=0, col="darkgrey", lty=2 ) +
  geom_abline( slope=-2, intercept=0, col="darkgrey", lty=2 ) +
  theme_minimal()

# Is anything to be learned here?


# For RMSE
# Also broken due to the SE reason, above
ggplot( sres, aes( RMSE_mcse, RMSE, col=as.factor(size_coef) )) +
  facet_grid( alpha ~ method ) + 
  geom_point() +
  theme_minimal()

summary( sres$RMSE_mcse / sres$RMSE )




#### Initial attempt to fit multilevel model on the raw data ####


if ( FALSE ) {
  library( lme4 )
  res
  res$err = res$ATE_hat - res$ATE
  table( table( res$seed ) )
  nrow(sres) / 3
  
  M = lmer( err ~ 1 + method + (0+method|seed) + (1|seed:runID),
            data = res )
  
  
  M = lmer( err ~ 1 + (as.factor(size_coef)*as.factor(alpha) + ICC + as.factor(n_bar) + as.factor(J) ) * method + (1+method|seed) + (1|seed:runID),
            data = res )
  
  arm::display(M)
  VarCorr(M)
  a = coef(M)$seed %>%
    as.data.frame()
  a$seed = rownames(a)
  head(a)
  
  aL <- a %>%
    pivot_longer( cols = -c( seed, `(Intercept)` ),
                  names_to = "method",
                  values_to = "bias_method" )
  sres
  a = left_join( a, sres, by="seed" )
  
}

#-------------------------------------------------------------------------------
# random effects meta-analysis of bias per method
library(metafor)

sres %>%
  filter(method == "MLM") %>%
  rma.uni(
    yi = bias, sei = bias_mcse, 
    mods = ~ as.factor(size_coef) * as.factor(alpha) * ICC + as.factor(n_bar) + as.factor(J),
    data = .
  )

RE_shrink <- function(dat) {
  RE_fit <- rma.uni(
    yi = bias, sei = bias_mcse, 
    mods = ~ as.factor(size_coef) * as.factor(alpha) * ICC + as.factor(n_bar) + as.factor(J),
    data = dat
  ) 
  
  shrunk_bias <- blup(RE_fit)
  data.frame(shrunk_bias = shrunk_bias$pred, shrunk_bias_mcse = shrunk_bias$se)
}

sres_shrunken <- 
  sres %>%
  group_nest(method) %>%
  mutate(
    shrunk_bias = map(data, RE_shrink)
  ) %>%
  unnest(cols = c(data, shrunk_bias))

ggplot( sres_shrunken, aes( bias_mcse, bias, col=as.factor(size_coef) )) +
  facet_grid( alpha ~ method ) + 
  geom_point() +
  geom_abline( slope=2, intercept=0, col="darkgrey", lty=2 ) +
  geom_abline( slope=-2, intercept=0, col="darkgrey", lty=2 ) +
  scale_x_continuous(limits = c(0, 0.1)) + 
  scale_y_continuous(limits = c(-0.2, 0.2)) + 
  theme_minimal()

ggplot( sres_shrunken, aes( shrunk_bias_mcse, shrunk_bias, col=as.factor(size_coef) )) +
  facet_grid( alpha ~ method ) + 
  geom_point() +
  geom_abline( slope=2, intercept=0, col="darkgrey", lty=2 ) +
  geom_abline( slope=-2, intercept=0, col="darkgrey", lty=2 ) +
  scale_x_continuous(limits = c(0, 0.1)) + 
  scale_y_continuous(limits = c(-0.2, 0.2)) + 
  theme_minimal()

