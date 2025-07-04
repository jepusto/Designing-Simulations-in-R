
library( rpart.plot )
library( rpart )


#' Make a regression tree with the simulation results 
#' 
create_analysis_tree <- function( data, 
                                  outcome_var, 
                                  predictor_vars,
                                  min_leaves = 1,
                                  max_leaves = Inf, 
                                  palette_name = "Greens", 
                                  plot_tree = TRUE, 
                                  initial_cp = 0.001,
                                  tree_title = NULL ) {
  
  # Create formula with the specified outcome variable and predictors
  predictors <- paste(predictor_vars, collapse = " + ")
  form <- as.formula(paste(outcome_var, "~", predictors))
  
  # Initial tree fit
  tree <- rpart(form, data = data, cp = initial_cp)
  
  # CP table analysis
  cp_table <- tree$cptable %>% 
    as_tibble()
  
  # Find optimal CP using 1-SE rule
  min_xerror <- which.min(cp_table$xerror)
  xerror_se <- cp_table$xstd[[min_xerror]]
  xerror_target <- cp_table$xerror[[min_xerror]] + xerror_se
  cp_best <- max(cp_table$CP[cp_table$xerror <= xerror_target])
  
  # Alternative CP based on passed max number of leaves
  row <- pmin(nrow(cp_table),
              1 + sum(!(cp_table[, "nsplit"] >= (max_leaves - 1))))
  cp_leaves <- cp_table[row, "CP"]
  
  # Alternative CP based on passed min number of leaves
  row <- pmin( nrow(cp_table),
              sum( cp_table[, "nsplit"] <= min_leaves))
  cp_min_leaves <- cp_table[row, "CP"]
  

  # Prune tree using the more conservative CP
  final_cp <- min( cp_min_leaves, max(cp_leaves, cp_best) )
  tree_p <- prune(tree, cp = final_cp)
  
  # Color mapping for nodes
  node_means <- tree_p$frame$yval
  norm_means <- (node_means - min(node_means)) / 
    (max(node_means) - min(node_means)) * 100
  norm_means <- round(norm_means) + 1
  
  # Create colors
  cols <- colorRampPalette(RColorBrewer::brewer.pal(9, palette_name))(131)
  node_colors <- cols[round(norm_means) + 1]
  
  # Leaf function for clean display
  leaf_fun <- function(x, labs, digits, varlen) {
    ifelse(x$frame$var == "<leaf>",
           sprintf("%.2f", x$frame$yval),
           labs)
  }
  
  # Plot if requested
  if (plot_tree) {
    # Use custom title if provided, otherwise default
    plot_title <- if (is.null(tree_title)) paste("Decision Tree for", outcome_var) else tree_title
    
    prp(tree_p,
        box.col = node_colors,
        border.col = "black",
        node.fun = leaf_fun,
        varlen = -10,
        main = plot_title)
  }
  
  # Return results
  return( invisible( list(
    tree = tree_p,
    cp_table = cp_table,
    final_cp = final_cp,
    cp_best = cp_best,
    formula = form,
    node_colors = node_colors,
    data = data,
    predictor_vars = predictor_vars
  )) )
}
