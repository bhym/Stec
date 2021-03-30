allorates <- function(len, r0, d0) {
    bv   <- (len * 9) / 2

    c_   <- 10 ^ ((0.758 * log10(bv)) - 0.421999999999998)
    n    <- c_ / 12 * 106 / 16

    gr_alloc <- 0.25 + 0.04 * len - 0.0005 * len ^ 2
    de_alloc <- 0.40 + 0.04 * len - 0.0005 * len ^ 2

    gr   <- r0 * gr_alloc
    de_  <- d0 * de_alloc

    res <- list(bv       = bv,
                C_       = c_,
                N        = n,
                gr_alloc = gr_alloc,
                de_alloc = de_alloc,
                gr       = gr,
                de_      = de_
                             )
    return(res)
}

library("tidyverse")
library("patchwork")
library("deSolve")

source("model.R")
source("parms.R")

#            dfac r0p   r0f   tga f1is alp
dif <- mollo(0.8, 1.06, 0.58, 2,  3,   0.012)
sam <- mollo(0.8, 1.06, 0.58, 0,  3,   0.012)

lenv_p  <- seq(input$initlen_p, input$minlen, by = -input$shrinking_step)
lenv_f  <- seq(input$initlen_f, input$minlen, by = -input$shrinking_step)
p_pb_l  <- length(lenv_p)
f_pb_l  <- length(lenv_f)

times <- seq(1,  input$end_time)
tl    <- length(times)
    
rates <- data.frame(Time = times,
                    len_p = rep(input$minlen, tl),
                    len_f = rep(input$minlen, tl))

bs    <- input$growth_block_start
len   <- input$growth_block_length
be    <- input$growth_block_start + len
tf1b  <- input$f1_injection_step

ala <- 1:min(p_pb_l, bs)
olo <- (be + 1):min(be + (p_pb_l  - bs), tl)

rates$len_p[ala]  <- lenv_p[ala]
if (p_pb_l > bs) {
  rates$len_p[olo]  <- lenv_p[-ala][seq_len(length(olo))]
}
rates$len_p[bs:be] <- lenv_p[length(ala)]

rates$len_f[1:tf1b] <- NA
rates$len_f[(tf1b + 1):(min(tf1b + f_pb_l, tl))] <- head(lenv_f, tl - tf1b)

temp <- seq(max(input$initlen_p, input$initlen_f),
                input$minlen, by = -input$shrinking_step)

godaf <- (data.frame(len_p = temp, allorates(temp, 1, 1)))


p_sam <- sam %>%
           as.data.frame %>%
           pivot_longer(-Time,
                        values_to = "Concentration",
                        names_to = "Lineage") %>%
           mutate(Lineage = gsub(" concentration", "", Lineage)) %>%
           mutate(Lineage = gsub("Parental", "P", Lineage)) %>%
           ggplot() +
            aes(x = Time, y = Concentration, col = Lineage) +
            geom_path() +
            geom_point() +
            scale_y_log10() +
            scale_x_continuous(breaks = scales::pretty_breaks()) +
            xlab("Time (days)") +
            ylab("Cells/ml")

p_dif <- dif %>%
           as.data.frame %>%
           pivot_longer(-Time,
                        values_to = "Concentration",
                        names_to = "Lineage") %>%
           mutate(Lineage = gsub(" concentration", "", Lineage)) %>%
           mutate(Lineage = gsub("Parental", "P", Lineage)) %>%
           ggplot() +
            aes(x = Time, y = Concentration, col = Lineage) +
            geom_path() +
            geom_point() +
            scale_y_log10() +
            scale_x_continuous(breaks = scales::pretty_breaks()) +
            xlab("Time (days)") +
            ylab("Cells/ml")

p_rat <- rates %>%
           pivot_longer(-Time,
                        values_to = "Cell length",
                        names_to = "Lineage") %>%
           mutate(Lineage = ifelse(Lineage == "len_f", "F1", "P")) %>%
           ggplot() +
            aes(x = Time, y = `Cell length`, col = Lineage) +
            geom_path() +
            geom_point() +
            scale_x_continuous(breaks = scales::pretty_breaks()) +
            xlab("Time (days)") +
            ylab("Cells length (\U3BCm)")

p_gal <- godaf %>%
           select("Cell length" = len_p, "Relative growth rate" = gr_alloc) %>%
           ggplot() +
            aes(x = `Cell length`, y = `Relative growth rate`) +
            geom_path(col = "dodgerblue") +
            xlab("Cells length (\U3BCm)") +
            scale_x_continuous(breaks = scales::pretty_breaks())

pli <- wrap_plots(p_rat, p_gal, p_sam, p_dif) &
  theme_minimal() &
  plot_annotation(tag_levels = "A")

plipan <- pli +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../../report/imgs/Figpan.pdf", plipan, dev = cairo_pdf,
       width = 14, height = 7)
