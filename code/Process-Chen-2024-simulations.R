# Source: https://osf.io/zypsh/files/by6zv
load(here::here("data","Chen-2024-es-sim.RData"))

saveRDS(res_LRR, file = here::here("data","Chen-2024-LRR-sims.rds"))
