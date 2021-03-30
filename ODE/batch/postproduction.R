# plot generator function
pl_parsub <- function(dat, x_, y_, breaks_) {
  x <- dplyr::enquo(x_)
  y <- dplyr::enquo(y_)
  ggplot(dat) +
   aes(x = !!x, y = !!y, z = logo) +
   geom_contour_filled(breaks = breaks_)   +
   geom_contour(col = "white", breaks = breaks_)   +
   theme_minimal() +
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
}


library("tidyverse")
library("kableExtra")

f_ref_alpha <- 0.012     #filter
l_ref_alpha <- 0.0122785 #line

lsw <- readRDS("lsw_out.rds")

lsw_a <-  lsw %>%
   mutate(P.corr = ifelse(Parental.concentration < 1,
                                 1,
                                 Parental.concentration)
         ) %>%
   mutate(logo = log10(F1.concentration / P.corr)) %>%
   filter((rantga %in% c(0, 1)) | (ranf1is  == 3))

dfa_vs_r0p <- filter(select(lsw_a, -ranf1is), ranalp == 0.01)
alp_vs_r0p <- filter(select(lsw_a, -ranf1is), randfac == 0.8)

pla <- dfa_vs_r0p %>%
          pl_parsub(randfac, ranr0p, seq(-2, 2, by = 0.25)) +
          geom_hline(yintercept = 2.609, col = "red", alpha = 0.5) +
          facet_grid(~rantga) +
          xlab("m") +
          ylab(expression(r[P] : r[F1])) +
          coord_cartesian(ylim = c(0.5, 3)) +
          scale_fill_viridis_d(end = 1 / 14 * 9,
                               name = expression(log[10](F[1] / P)))
                              #needed for col match of legends

plb <- alp_vs_r0p %>%
         pl_parsub(ranalp, ranr0p, seq(-2, 2, by = 0.25)) +
         facet_grid(~rantga) +
         xlab("\u03B1") +
         ylab(expression(r[P] : r[F1])) +
         geom_point(aes(x = l_ref_alpha, y = 2.609), pch = 21,
                    alpha = 0.5, fill = "red") +
         scale_fill_viridis_d(name = expression(log[10](F[1] / P))) +
         coord_cartesian(ylim = c(0.5, 3)) +
         scale_x_log10()

pan <- patchwork::wrap_plots(pla, plb, nrow = 2)
ggsave("../../report/imgs/parswpan.pdf", pan,
       device = cairo_pdf, width = 12, height = 8)

fint <- unique(lsw_a$ranr0p) [seq(6, 26, len = 9)]
sint <- unique(lsw_a$ranalp) [seq(1, 20, len = 9)]

bds <- lsw_a %>%
filter(rantga == 3, ranalp %in% sint, ranr0p %in% fint) %>%
    select(ranalp, ranr0p, randfac, logo) %>%
    group_by(randfac) %>%
    nest %>%
    mutate(matrici = map(data, ~pivot_wider(.x, names_from = ranalp,
                                            values_from = logo) %>%
                                arrange(desc(ranr0p)) %>%
                                column_to_rownames("ranr0p"))) %>%
    ungroup() %>%
    select(-data) %>%
    pwalk(function(randfac, matrici, a = "ranalp", b = "ranr0p")
          kable(matrici, "latex", booktabs = T, digits = 2) %>%
          save_kable(paste("../../report/tbls/",
                           a, "-", b, "_",
                           randfac, ".tex", sep = "")))
