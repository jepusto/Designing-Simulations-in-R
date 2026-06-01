# Source: https://osf.io/zypsh/files/by6zv
load(here::here("data","Chen-2025-sim-res.RData"))

res_dat_clean <- 
  res_dat %>%
  mutate(
    weight = map_dbl(weights, \(x) x[[1]]),
    working_mod = str_extract(method, "(MV|UNI)_"),
    working_mod = case_when(
      working_mod == "UNI_" ~ "univariate", 
      working_mod == "MV_" ~ "multivariate",
      method %in% c("3PSM","4PSM","TF","p-uniform","p-uniform*") ~ "univariate",
      method %in% c("CHE","CHE-ISCW") ~ "multivariate"
    ),
    method_lab = str_remove(method, "(MV|UNI)_"),
    method_lab = case_match(method_lab, "CHE" ~ "RE", "CHE-ISCW" ~ "RE-ISW", .default = method_lab)
  ) %>%
  filter(k_stop %in% c("NA","2")) %>%
  select(
    mu, tau, omega, k, cor_mu, cor_sd,
    sel_mechanism,
    weight, wt = wts,
    iterations, method, 
    working_mod, method_lab,
    n_converged:rej_rate_mcse
  )

res_dat_clean %>%
  summarize(
    n = n(),
    .by = c(working_mod, method_lab)
  ) %>%
  pivot_wider(names_from = working_mod, values_from = n) 
 

saveRDS(res_dat_clean, file = here::here("data","Chen-2025-selection-sims.rds"))
