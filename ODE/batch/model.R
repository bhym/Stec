mollo <- function(dfac, r0p, r0f, tga, f1is, alp) {
  d0 <- 0
  allorates <- function(len, r0, d0) {
      bv   <- (len * 9) / 2

      C_   <- 10 ^ ((0.758 * log10(bv)) - 0.421999999999998)
      N    <- C_ / 12 * 106 / 16

      gr_alloc <- 0.25 + 0.04 * len - 0.0005 * len ^ 2
      de_alloc <- 0.40 + 0.04 * len - 0.0005 * len ^ 2

      gr   <- r0 * gr_alloc
      de_  <- d0 * de_alloc

      res <- list(bv       = bv,
                  C_       = C_,
                  N        = N,
                  gr_alloc = gr_alloc,
                  de_alloc = de_alloc,
                  gr       = gr,
                  de_      = de_
                               )
      return(res)
  }

  model <- function(t, x, parms, approx_len_p, approx_len_f1, allo_rates) {
    with(as.list(c(parms, x)), {
      p_len <- approx_len_p(t)
      f_len <- approx_len_f1(t)

      p_ar <- allo_rates(p_len, r0_p, d0)
      f_ar <- allo_rates(p_len, r0_f1, d0)

      blole <- input$growth_block_start + tga
      t1aps <- f1is

      nc_ <- c(p_p * p_ar$N, p_f1 * f_ar$N)
      nc <- sum(nc_)
      k <- 1 -  (nc / p0)

      dp_p  <- r0_p * ifelse(t < blole, 0, p_ar$gr_alloc) * k * p_p  -
               ifelse((t >= t1aps - 1 & t <= t1aps + 1), dfac, 0) * p_p
      dp_f1 <- r0_f1 * ifelse(t < t1aps, 0, f_ar$gr_alloc) * k * p_f1
      res <- c(dp_p, dp_f1)
      list(res)
    })
  }

    times <- seq(1,  input$end_time)
    tl    <- length(times)

    bs    <- input$growth_block_start
    len   <- input$growth_block_length
    be    <- input$growth_block_start + len
    tf1b  <- f1is

    lenv_p  <- input$initlen_p:input$minlen
    lenv_f  <- input$initlen_f:input$minlen
    p_pb_l  <- length(lenv_p)
    f_pb_l  <- length(lenv_f)

    rates <- data.frame(Time = times,
                        len_p = rep(input$minlen, tl),
                        len_f = rep(input$minlen, tl))

    ala <- 1:min(p_pb_l, bs)
    olo <- (be + 1):min(be + (p_pb_l  - bs), tl)

    rates$len_p[ala]  <- lenv_p[ala]
    if (p_pb_l > bs) {
      rates$len_p[olo]  <- lenv_p[-ala][seq_len(length(olo))]
    }
    rates$len_p[bs:be] <- lenv_p[length(ala)]

    rates$len_f[1:tf1b] <- 0
    rates$len_f[(tf1b + 1):(min(tf1b + f_pb_l, tl))] <- head(lenv_f, tl - tf1b)

    p_lenappro <- approxfun(rates$Time, rates$len_p,  rule = 2)
    f_lenappro <- approxfun(rates$Time, rates$len_f,  rule = 2)


    xstart <- c(p_p = input$p_conc, p_f1 = alp * input$p_conc)

    parms <- c(r0_p  = r0p,
               r0_f1 = r0f,
               d0    = input$death_rate,
               dfac  = dfac,
               p0    = unname(xstart[1] *
                              c(allorates(input$initlen_p, 0, 0)$N)) *
                              input$k
                             )

    out <- ode(y = xstart, times = times, func = model, parms = parms,
               approx_len_p = p_lenappro, approx_len_f1 = f_lenappro,
               allo_rates = allorates)

    colnames(out) <- c("Time", "Parental concentration", "F1 concentration")
    return(out)
}
