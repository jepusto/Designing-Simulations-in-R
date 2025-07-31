
# Analyze entire simulation across all scenarios, datasets, and
# outcomes.
#
# Do this by, e.g., the meta regression analysis and making plots for
# that


library( scales )

# **********************************************************
# Initial exploration of trends ----
# **********************************************************

rm( list = ls() )

# Code to make the saved dataset from the main MLIV github results files
if ( FALSE ) {
  source( here::here( "graphs/setup_final_paper_workspace.R" ) )
  
  # Prep data for regression by making ATE the reference category for
  # both queen and model.
  df
  df_sub <- df %>%
    dplyr::filter( !str_detect( model, "INF"), 
                   model != "BART S Int",
                   #!( queen %in% c( "ATE", "CDML" ) ) ) %>%
                   !( queen %in% c( "ATE"))) %>% #, "CDML" ) ) ) %>%
    
    #dplyr::filter( outcome_type == 1,
    #               dataset == "ca" ) %>%
    mutate( model = relevel( factor( model ), ref = "ATE" ),
            #          queen = relevel( factor( queen ), ref = "ATE" ),
            cov_set_size = factor( cov_set_size, levels = c("small","medium","large" ) ),
            train_set_size = factor( train_set_size, levels = c( 1000, 2000, 5000 ) )
    )
  
  
  
  
  
  df_sub_core <- df_sub %>%
    dplyr::filter( !( model %in% c( "XGBOOST R", "XGBOOST S", "SL S", "SL T", "BART S Int" ))) %>%
    mutate( model = droplevels( model ) )
  
  table( df_sub_core$model )
  
  df_rel <- df_sub_core %>%
    dplyr::select( set_id, model, queen, rmse, se, bias, R2, spearman ) %>%
    pivot_longer( cols = c( rmse, se, bias, R2, spearman ), 
                  names_to = "measure",
                  values_to = "value" ) %>%
    group_by( queen, set_id, measure ) %>%
    mutate( n = n(),
            med_value = median( value ),
            rel_perf = value / med_value ) %>%
    ungroup()
  df_rel
  
  write_rds( df_rel, here::here( "MLIV_sim_results.rds" ) )
}

df_rel = read_rds( here::here( "MLIV_sim_results.rds" ) )

length( unique( df_rel$model ) )
table( df_rel$model )
nrow(df_rel) / (3*16)

df_rel_sum <- df_rel %>%
  group_by( model, measure ) %>%
  summarise( level = mean( value ),
             level_sd = sd( value ),
             level_10 = quantile( value, 0.10 ),
             level_90 = quantile( value, 0.90 ),
             perf = mean( rel_perf ),
             sdperf = sd( rel_perf ),
             perf_10 = quantile( rel_perf, 0.10 ),
             perf_90 = quantile( rel_perf, 0.90 ),
             .groups="drop" )

df_rel_sum

df_BSR = filter( df_rel_sum,
                 measure %in% c( "rmse", "se", "bias" ) )

# Make overall performance table and canonical ordering of models ----
overall_performance_table <- df_BSR %>%
  dplyr::select( model, measure, perf, sdperf ) %>%
  pivot_wider( names_from = measure, 
               values_from = c( perf, sdperf ) ) %>%
  arrange( perf_rmse )
knitr::kable( overall_performance_table, digits = 2 )



# ***************************************************************************
# Make plot of the average relative performances of models ----
# ***************************************************************************



df_BSR_long <- df_rel %>%
  filter( measure %in% c( "rmse", "se", "bias" ) )# %>%
# filter( model != "OLS S", model != "OLS S INT" )
df_BSR_long <- df_BSR_long %>%
  mutate(measure = factor(measure, levels = c("rmse", "se", "bias")))

df_BSR_long$model = factor( df_BSR_long$model, levels = levels( overall_performance_table$model ) )

# Average number of observations per box
nrow(df_BSR_long) / (3*length(unique(df_BSR_long$model)))

# Step 1: Get model order based on median rel_perf for RMSE
model_order <- df_BSR_long %>%
  filter(measure == "rmse") %>%
  group_by(model) %>%
  summarise(median_perf = median(rel_perf, na.rm = TRUE), .groups = "drop") %>%
  arrange(median_perf) %>%
  pull(model)

# Step 2: Apply the factor order to the full dataset
df_BSR_long$model <- factor(df_BSR_long$model, levels = model_order)


ggplot( df_BSR_long, aes( model, rel_perf ) ) +
  facet_wrap( ~ measure ) +
  #geom_point( position = position_dodge(width = 0.4) ) +
  #geom_errorbar( aes( ymin = level_25, ymax = level+level_75 ),
  #               linewidth = 2,
  #               width = 0 ) +
  #geom_errorbar( aes( ymin = level_10, ymax = level_90 ),
  #               width = 0 ) +
  #               
  geom_boxplot( width = 0.15, fill = "black", outlier.size = 0.15, outlier.color = "grey" ) +
  coord_flip( ylim=c(0.5, 2 ) ) +
  #  coord_flip( ) +
  theme_minimal() +
  # expand_limits( y = c(0,1) ) +
  labs( x = "", y = "" ) +
  geom_hline( yintercept = 1, lty=2 ) +
  scale_x_discrete( labels = label_wrap(15) ) +
  theme(plot.background = element_rect(fill = "white", color = NA)) + # Added white background
  labs( title = "Relative performance across all simulation scenarios",
        caption = "Cropped at 50% and 200%.") +
  scale_y_log10( labels = c( "x1/4", "x1/2", "x3/4", "x1", "x1.5", "x2" ), #scales::percent_format(),
                 breaks = c( 0.25, 0.5, 0.75, 1, 1.5, 2.0 )) 

