library("tidyverse")
library("furrr")
 plan(multiprocess)
# SWEEP RUN CODE
# This if you want to run the sweep
library("deSolve")

source("model.R")
source("parms.R")

 range_dfac <- seq(0.1, 0.9, 0.05)
 range_f1is <- seq(1, 9, 1)
 range_r0p  <- seq(0.1, 5, 0.05)
 range_tga  <- seq(1, 9, 1)

 subspace <- crossing(randfac = range_dfac,
                      ranf1is = range_f1is,
                      rantga = range_tga,
                      ranr0p  = range_r0p)

sweep_ <- subspace %>%
  mutate(outs = future_pmap(list(randfac, ranf1is, ranr0p, 1, rantga),
                            function(x, y, z, j, u) mollo(x, y, z, j, u)
                            )
        )

saveRDS(sweep_, "sweep_out.rds")
