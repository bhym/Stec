pl_parsub <- function(dat, x_, y_) {
  x <- dplyr::enquo(x_)
  y <- dplyr::enquo(y_)
  ggplot(dat) +
   aes(x = !!x, y = !!y, z = logo) +
   geom_contour_filled()   +
   theme_minimal() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
}

tab_parsub <- function(dat, x_, y_) {
  x <- dplyr::enquo(x_)
  y <- dplyr::enquo(y_)
  dat %>%
    select(!!x, !!y, rantga, logo) %>%
    group_by(rantga) %>%
    nest %>%
    mutate(matrici = map(data, ~pivot_wider(.x, names_from = !!y,
                                            values_from = logo) %>%
    column_to_rownames(x_))) %>%
    ungroup() %>%
    select(-data) %>%
    pwalk(function(rantga, matrici, a = y_, b = x_)
          kable(matrici, "latex", booktabs = T, digits = 2) %>%
          save_kable(paste("../../report/tbls/",
                           a, "-", b, "_",
                           rantga, ".tex", sep = "")))
}

library("tidyverse")
library("kableExtra")

lsw <- readRDS("lsw_out.rds")

lsw_a <-  lsw %>%
   mutate(P.corr = ifelse(Parental.concentration < 1,
                                 1,
                                 Parental.concentration)
         ) %>%
   mutate(logo = log10(F1.concentration / P.corr)) %>%
   filter(rantga == ranf1is + 2)

dfa_vs_r0p <- filter(lsw_a, ranf1is == 3,   ranalp == 0.01)
dfa_vs_f1i <- filter(lsw_a, ranr0p == 1,    ranalp == 0.01)
f1i_vs_r0p <- filter(lsw_a, randfac == 0.8, ranalp == 0.01)
alp_vs_r0p <- filter(lsw_a, randfac == 0.8, ranf1is == 3)

pla <- dfa_vs_r0p %>%
          pl_parsub(randfac, ranr0p) +
          facet_wrap(~rantga) +
          xlab("P gametogenesis-induced death") +
          ylab(expression(r[p] : r[f1])) +
         coord_cartesian(ylim = c(1, 3))

plb <- alp_vs_r0p %>%
         pl_parsub(ranalp, ranr0p) +
         facet_wrap(~rantga) +
         xlab("\u03B1") +
         ylab(expression(r[p] : r[f1])) +
         coord_cartesian(ylim = c(1, 3))

plc <- f1i_vs_r0p %>%
         pl_parsub(ranf1is, ranr0p) +
         facet_wrap(~rantga) +
         xlab("Day of F1 appearence") +
         ylab(expression(r[p] : r[f1])) +
         coord_cartesian(ylim = c(1, 3))

pld <- dfa_vs_f1i %>%
         pl_parsub(randfac, ranf1is) +
         facet_wrap(~rantga) +
         xlab("P gametogenesis-induced death") +
         ylab("Day of F1 appearence")


tab_parsub(dfa_vs_r0p, "randfac", "ranr0p")
tab_parsub(dfa_vs_f1i, "randfac", "ranf1is")
tab_parsub(f1i_vs_r0p, "ranf1is", "ranr0p")

fint <- unique(lsw_a$ranr0p)[c(10, 15, 20, 25, 30)]
sint <- unique(lsw_a$ranalp)[c(1, 5, 10, 15, 20)]
co_alp_vs_r0p <- alp_vs_r0p %>% filter(ranalp %in% sint, ranr0p %in% fint)
tab_parsub(co_alp_vs_r0p, "ranalp",  "ranr0p")

ggsave("../../report/imgs/a.pdf", pla, device = cairo_pdf)
ggsave("../../report/imgs/b.pdf", plb, device = cairo_pdf)
ggsave("../../report/imgs/c.pdf", plc, device = cairo_pdf)
ggsave("../../report/imgs/d.pdf", pld, device = cairo_pdf)
