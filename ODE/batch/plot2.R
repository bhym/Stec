library("tidyverse")
library("patchwork")

data_ <- lsw %>%
  mutate(otto = map(outs, ~{
                          data.frame(.x) %>%
                          filter(F1.concentration > 0,
                                 F1.concentration >= Parental.concentration) %>%
                          head(1) %>%
                          pull(Time)
                           }
                   )
        )

naz <- data_ %>%
  unnest(otto) %>%
  full_join(subspace) %>%
  mutate(otto = ifelse(is.na(otto), 0, otto))

pl_parsub <- function(dat, x_) {
  x <- dplyr::enquo(x_)
  ggplot(dat) +
   aes(x = !!x, y = otto) +
   #geom_density2d_filled(contour_var = "ndensity") +
   #geom_density2d(col = "white") +
   geom_bin2d() +
   theme_minimal() +
   ylab("Day when F1 becomes equal or bigger than P") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}

d <- pl_parsub(naz, rprf1) + xlab("ratio") + xlim(0.1, 2.0) + ylim(0, 15)
a <- pl_parsub(naz, dere)  + xlab("m")     + xlim(0.1, 0.9) + ylim(0, 15)
b <- pl_parsub(naz, a_)    + xlab("alpha") + xlim(0.01, 0.2) + ylim(0, 15)
