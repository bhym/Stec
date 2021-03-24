library("tidyverse")
library("furrr")
 plan(multiprocess)
# SWEEP RUN CODE
# This if you want to run the sweep
library("deSolve")

source("model.R")
source("parms.R")

 range_dfac <- seq(0.1, 0.9, 0.1)
 range_r0p  <- seq(0.5, 3, 0.1)
 range_tga  <- seq(0, 6, 1)
 range_f1is <- seq(1, 6, 1)
 range_alp  <- seq(0.01, 0.2, 0.01)

 subspace <- crossing(randfac = range_dfac,
                      ranr0p  = range_r0p,
                      rantga  = range_tga,
                      ranf1is = range_f1is,
                      ranalp  = range_alp)

sweep_ <- subspace %>%
  mutate(outs = future_pmap(list(randfac, ranr0p, 1, rantga, ranf1is, ranalp),
                            function(x, y, z, j, u, b) mollo(x, y, z, j, u,b)
                            )
        )

saveRDS(sweep_, "sweep_out.rds")
