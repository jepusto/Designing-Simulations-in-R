

# Examining clusterRCT simulation to verify it is correctly
# implemented.

library( tidyverse )


res <- readRDS( file = "results/simulation_CRT.rds" )
res


sres <- 
  res %>% 
  group_by( n_bar, J, ATE, size_coef, ICC, alpha, method ) %>%
  summarise( 
    bias = mean(ATE_hat - ATE),
    SE = sd( ATE_hat ),
    RMSE = sqrt( mean( (ATE_hat - ATE )^2 ) ),
    ESE_hat = sqrt( mean( SE_hat^2 ) ),
    SD_SE_hat = sqrt( sd( SE_hat^2 ) ),
    power = mean( p_value <= 0.05 ),
    R = n(),
    .groups = "drop"
  )
sres

table( sres$R )



# Did we get actual unique values over our 1000 replications? ----

sb = filter( res, n_bar == 20, J == 5, ATE == 0.2,
             size_coef == 0.2, ICC == 0.20, alpha == 0.8 )
sb

sb = bind_rows( sb, sb ) %>%
  dplyr::select( -seed, -runID )
sb

sb = unique( sb )
sb

# It looks like yes?



# Try with simhelpers package ----


library( simhelpers )

res

ssres <- res %>% 
  group_by( n_bar, J, ATE, size_coef, ICC, alpha, method ) %>%
  summarise( calc_absolute( estimates = ATE_hat,
                            true_param = ATE,
                            criteria = c("bias","stddev", "rmse")) )

summary( ssres$bias - sres$bias )


sres_null <- ssres %>%
  filter( size_coef == 0, alpha == 0 )
sres_null

ggplot( sres_null, aes( ICC, bias, 
                        col=method, pch=method, group=method ) ) +
  facet_grid( J ~ n_bar, labeller = label_both ) +
  geom_point() + geom_line() +
  geom_errorbar( aes( ymin = bias - 1.96*bias_mcse, ymax = bias + 1.96*bias_mcse ), width=0 ) +
  geom_hline( yintercept = 0 ) +
  theme_minimal()


summary( res$ATE_hat - res$ATE )
res %>%
  filter( size_coef == 0, alpha == 0, J == 5 )  %>%
  ggplot(aes(ATE_hat) ) +
  facet_grid( n_bar ~ method ) +
  geom_histogram()


res %>%
  filter( abs( ATE_hat - ATE ) > 2.5 ) %>%
  group_by( method ) %>%
  summarise( n = n() )

sd( res$ATE_hat - res$ATE ) / sqrt( 1000 )


sres_null_sub <- res %>% 
 filter( size_coef == 0, alpha == 0,
         runID %in% 1:100 ) %>%
      #   runID <= 100) %>%
  #         runID %in% sample(1000,100) )  %>%
  group_by( n_bar, J, ATE, size_coef, ICC, alpha, method ) %>%
  summarise( calc_absolute( estimates = ATE_hat,
                            true_param = ATE,
                            criteria = c("bias","stddev", "rmse")) ) 

summary( sres_null_sub$K_absolute )

ggplot( sres_null_sub, aes( ICC, bias, 
                        col=method, pch=method, group=method ) ) +
  facet_grid( J ~ n_bar, labeller = label_both ) +
  geom_point() + geom_line() +
  geom_errorbar( aes( ymin = bias - 1.96*bias_mcse, ymax = bias + 1.96*bias_mcse ), width=0 ) +
  geom_hline( yintercept = 0 ) +
  theme_minimal()





# Look at alpha=0 and size_coef = 0, vs alpha=0 and size_coef = 0.2 ----

sres_a0 <- filter( sres, alpha == 0, size_coef == 0 )
sres_a02 <- filter( sres, alpha == 0, size_coef == 0.2 )

a0 = left_join( sres_a0, sres_a02,
                   by = c("n_bar","J","ICC", "method","ATE"),
                   suffix = c("_a0","_a02") ) %>%
  ungroup() %>%
  dplyr::select( -starts_with( "alpha" ),
                 -starts_with( "size_coef" ) )
a0

ggplot( a0, aes( RMSE_a0, RMSE_a02, col=method ) )+
  facet_wrap( ~method ) +
  geom_point() +
  coord_fixed()






