library("tidyverse")
library("patchwork")

lsw_f1 <- lsw_a %>%
  filter(name == "f1")

lsw_p <- lsw_a  %>%
  filter(name == "P")

breaks_p  <- seq(-8, -2, by = 0.25)
breaks_f1 <- seq(5, 10,  by = 0.25)

pliplo <- function(dat_, x_, y_, break__) {
  x <- dplyr::enquo(x_)
  y <- dplyr::enquo(y_)
  dat_  %>%
    ggplot() +
    aes(x = !!x, y = !!y,   z = log(value)) +
    geom_contour_filled(breaks = break__)   +
    geom_contour(col = "white", breaks = break__)  +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

}

pliplo_p <- function(dat, x, y, break_) {
  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)
    pliplo(dat, !!x, !!y, break_) +
    scale_fill_viridis_d(name = expression(log[10](p)),
                         begin = 0, end = 0.5,  direction = 1)  +
    ggtitle("parameter subspace for p")
}

pliplo_f1 <- function(dat, x, y, break_) {
  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)
    pliplo(dat, !!x, !!y, break_) +
  scale_fill_viridis_d(name = expression(log[10](p)),
                       begin = 0.5, end = 1,  direction = -1)  +
  ggtitle("parameter subspace for f1")
}


a <- pliplo_p(lsw_p,   rprf1, a_, breaks_p)  +
  xlab(expression(r[p] : r[f1])) +
  ylab("\u03b1")

b <- pliplo_f1(lsw_f1, rprf1, a_, breaks_f1) +
  xlab(expression(r[p] : r[f1])) +
  ylab("\u03b1")

e <- pliplo_p(lsw_p,  dere, a_, breaks_p) +
  xlab("p basal death rate") +
  ylab("\u03b1")

f <- pliplo_f1(lsw_f1, dere, a_, breaks_f1) +
  xlab("p basal death rate") +
  ylab("\u03b1")

g <- pliplo_p(lsw_p,  rprf1, dere, breaks_p) +
  xlab(expression(r[p] : r[f1])) +
  ylab("p basal death rate")

h <- pliplo_f1(lsw_f1, rprf1, dere, breaks_f1) +
  xlab(expression(r[p] : r[f1])) +
  ylab("p basal death rate")

gin <- (a | b) / (e | f) / (g | h)
