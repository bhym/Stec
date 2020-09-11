library("tidyverse")
library("deSolve")
library("furrr")

plan(multiprocess)

source("model.R")
source("parms.R")

range_ <- seq(0.1,1,0.01)
subspace <- crossing(rp=range_, rf1=range_, rm=range_)

sweep_ <- subspace %>%
  mutate(outs = future_pmap(list(rp,rf1,rm), function(x,y,z) mollo(x,y,z)))

lsw <- sweep_ %>%
  mutate(louts = future_map(outs, ~tail(.x,1))) %>%
  unnest_wider(louts)

lsw %>%
  select(-outs,-...1) %>%
  rename(P = ...2, f1 = ...3) %>%
  pivot_longer(-c(rp, rf1, rm)) %>%
  ggplot() +
   aes(x=rp, y= rf1, fill=value) +
   geom_tile() +
   facet_wrap(~name)
