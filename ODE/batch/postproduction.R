pl_parsub <- function(dat, x_, y_, break__) {
  x <- dplyr::enquo(x_)
  y <- dplyr::enquo(y_)
  ggplot(dat) +
   aes(x = !!x, y = !!y, z = log10(f1 / P.corr)) +
    geom_contour_filled()   +
    geom_contour(col = "white")  +
   theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}

library("tidyverse")
library("furrr")
 plan(multiprocess)
lsw <- readRDS("lsw_out_a_ratio.rds")
#lsw <- readRDS("lsw_out_a_pde.rds")
#lsw <- readRDS("lsw_out_fgr_pde.rds")

lsw_a <-  lsw %>%
   select(-outs) %>%
   mutate(P.corr = ifelse(P < 1, 1, P))

breaks <- c(0, 1, by = 0.1)

## for lsw_out_a_ratio.rds
pl_parsub(lsw_a, ranfgr, rana__, breaks) + facet_wrap(~rantga) + xlab(expression(r[r1] : r[p])) + ylab("\u03b1")

## for lsw_out_a_pde.rds
#pl_parsub(lsw_a, ranpde, rana__, breaks) + facet_wrap(~rantga) + xlab("P basal death")+ ylab("\u03b1")

## for lsw_out_fgr_pde.rds
#pl_parsub(lsw_a, ranfgr, ranpde, breaks) + facet_wrap(~rantga)  + xlab(expression(r[r1] : r[p])) + ylab("P basal death")
