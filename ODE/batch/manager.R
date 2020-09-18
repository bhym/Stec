library("tidyverse")
library("furrr")
 plan(multiprocess)
# SWEEP RUN CODE
# This if you want to run the sweep
library("deSolve")

source("model_N_s.R")
source("parms.R")

 range_tga <- seq(1, 5, 1)
 range_a__ <- seq(0.01, 0.1, 0.005)
 range_fgr <- seq(0.1, 5, 0.01)
 subspace <- crossing(rantga = range_tga,
                      ranfgr = range_fgr,
                      rana__ = range_a__)

sweep_ <- subspace %>%
  mutate(outs = future_pmap(list(rantga, ranfgr, rana__),
                            function(x, y, z) mollo(x, y, z)
                            )
        )

saveRDS(sweep_, "sweep_out.rds")
