pl_parsub <- function(dat, x_, y_, break__) {
  x <- dplyr::enquo(x_)
  y <- dplyr::enquo(y_)
  ggplot(dat) +
   aes(x = !!x, y = !!y, z = log10(F1.concentration / P.corr)) +
    geom_contour_filled()   +
   theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}

library("tidyverse")
library("furrr")
 plan(multiprocess)
lsw <- readRDS("lsw_out.rds")

lsw_a <-  lsw %>%
   mutate(P.corr = ifelse(Parental.concentration < 1,
                                 1,
                                 Parental.concentration)
         )

breaks <- c(0, 1, by = 0.1)

## for lsw_out_a_ratio.rds
pla <- lsw_a %>%
         filter(ranf1is == 3) %>%
         pl_parsub(randfac, ranr0p, breaks) +
         facet_wrap(~rantga) +
         xlab("P gametogenesis-induced death") +
         ylab(expression(r[p] : r[f1])) +
         scale_y_log10()

plb <- lsw_a %>%
         filter(ranr0p == 1) %>%
         pl_parsub(randfac, ranf1is, breaks) +
         facet_wrap(~rantga) +
         xlab("P gametogenesis-induced death") +
         ylab("Day of F1 appearence")

plc <- lsw_a %>%
         filter(randfac == 0.8) %>%
         pl_parsub(ranf1is, ranr0p, breaks) +
         facet_wrap(~rantga) +
         xlab("Day of F1 appearence") +
         ylab(expression(r[p] : r[f1])) +
         scale_y_log10()

dev.new(); pla
dev.new(); plb
dev.new(); plc
