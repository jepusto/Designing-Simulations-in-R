r_and_z <- function(data) {
  
  r <- cor(data$C1, data$C2)
  z <- atanh(r)
  se_z <- 1 / sqrt(nrow(data) - 3)
  ci_z <- z + c(-1, 1) * qnorm(.975) * se_z
  ci_r <- tanh(ci_z)
  
  tibble( r = r, z = z, CI_lo = ci_r[1], CI_hi = ci_r[2] )
}
