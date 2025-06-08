ANOVA_Welch_F <- function(data) {
  anova_F <- oneway.test(x ~ group, data = data, var.equal = TRUE)
  Welch_F <- oneway.test(x ~ group, data = data, var.equal = FALSE)
  
  result <- tibble(
    ANOVA = anova_F$p.value,
    Welch = Welch_F$p.value
  )
  
  return(result)
}
