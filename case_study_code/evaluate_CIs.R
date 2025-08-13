evaluate_CIs <- function(data, rho = 0) {
  simhelpers::calc_coverage(
    data = data, 
    lower_bound = CI_lo, upper_bound = CI_hi, 
    true_param = rho
  )
}
