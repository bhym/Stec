library(tidyverse)

lsw <- readRDS("sweep_out.rds") %>%
         mutate(outs = map(outs,data.frame)) %>%
         mutate(endT = map(outs,tail,1)) %>%
         select(-outs) %>%
         unnest(endT)

saveRDS(lsw, "lsw_out.rds")
