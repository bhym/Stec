library("tidyverse")
library("deSolve")
library("furrr")

plan(multiprocess)

source("model.R")
source("parms.R")

range_ <- seq(0.1,1,0.1)
subspace <- crossing(rp=range_, rf1=range_, dere=range_)

sweep_ <- subspace %>%
  mutate(outs = future_pmap(list(rp,rf1,dere), function(x,y,z) mollo(x,y,z)))

lsw <- sweep_ %>%
  mutate(louts = future_map(outs, ~tail(.x,1))) %>%
  unnest_wider(louts)

lsw_2 <- lsw %>%
  select(-outs,-...1) %>%
  rename(P = ...2, f1 = ...3) %>%
  pivot_longer(-c(rp, rf1, dere)) %>%
  mutate(`rp:rf1`=rp/rf1) %>%
  filter(name == "f1")

library(plotly)
 plot_ly(lsw_2, x = ~`rp:rf1`, y= ~dere, z = ~value) %>%
   add_markers() %>%
   layout(scene = list(
          xaxis = list(title = "rP:rF1"),
          yaxis = list(title = "basal m"),
          zaxis = list(title = "F1 concentration")))
