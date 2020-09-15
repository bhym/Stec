library("tidyverse")
library("patchwork")

lsw_f1 <- lsw_a %>%
  filter(name == "f1")

lsw_P <- lsw_a  %>%
  filter(name == "P")

breaks_P <- seq(-8,-2, by=0.25)
breaks_F1  <- seq(5,10, by=0.25)

a <- lsw_P  %>%
  ggplot() +
  aes(x=rprf1,y=a_,   z=log(value)) +
  geom_contour_filled(breaks=breaks_P)   +
  geom_contour(col = "white", breaks=breaks_P)  +
  theme_minimal() +
  xlab(expression(r[p] : r[f1])) +
  ylab("\u03b1")             +
  scale_fill_viridis_d(name=expression(log[10](P)), begin=0.5, end=1,  direction=-1)  +
  ggtitle("Parameter subspace for P")

b <- lsw_f1 %>%
  ggplot() +
  aes(x=rprf1,y=a_,   z=log(value)) +
  geom_contour_filled(breaks=breaks_F1)  +
  geom_contour(col = "white", breaks=breaks_F1) +
  theme_minimal() +
  xlab(expression(r[p] : r[f1])) +
  ylab("\u03b1")             +
  scale_fill_viridis_d(name=expression(log[10](F1)), begin=0, end=0.5, direction=1) +
  ggtitle("Parameter subspace for F1")

e <- lsw_P  %>%
  ggplot() +
  aes(x=dere,y=a_,    z=log(value)) +
  geom_contour_filled(breaks=breaks_P)   +
  geom_contour(col = "white", breaks=breaks_P)  +
  theme_minimal() +
  xlab("P basal death rate")     +
  ylab("\u03b1")             +
  scale_fill_viridis_d(name=expression(log[10](P)), begin=0.5, end=1,  direction=-1)  +
  ggtitle("Parameter subspace for P")

f <- lsw_f1 %>%
  ggplot() +
  aes(x=dere,y=a_,    z=log(value)) +
  geom_contour_filled(breaks=breaks_F1)  +
  geom_contour(col = "white", breaks=breaks_F1) +
  theme_minimal() +
  xlab("P basal death rate")     +
  ylab("\u03b1")             +
  scale_fill_viridis_d(name=expression(log[10](F1)), begin=0, end=0.5, direction=1) +
  ggtitle("Parameter subspace for F1")

g <- lsw_P  %>%
  ggplot() +
  aes(x=rprf1,y=dere, z=log(value)) +
  geom_contour_filled(breaks=breaks_P)   +
  geom_contour(col = "white", breaks=breaks_P)  +
  theme_minimal() +
  xlab(expression(r[p] : r[f1])) +
  ylab("P basal death rate") +
  scale_fill_viridis_d(name=expression(log[10](P)), begin=0.5, end=1,  direction=-1)  +
  ggtitle("Parameter subspace for P")

h <- lsw_f1 %>%
  ggplot() +
  aes(x=rprf1,y=dere, z=log(value)) +
  geom_contour_filled(breaks=breaks_F1)  +
  geom_contour(col = "white", breaks=breaks_F1) +
  theme_minimal() +
  xlab(expression(r[p] : r[f1])) +
  ylab("P basal death rate") +
  scale_fill_viridis_d(name=expression(log[10](F1)), begin=0, end=0.5, direction=1) +
  ggtitle("Parameter subspace for F1")

gin <- (a | b ) / (e | f) / (g | h)
