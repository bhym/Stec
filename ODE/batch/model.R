
mollo <- function(tga,fgr,a__) {
  allorates <- function(len, r0, d0) {
      bv   <- (len * 9) / 2
      C_   <- 10 ^ (0.758 * (log(bv) / log(10)) - 0.421999999999998)
      N    <- C_ / 12 * 106 / 16
      gr_alloc <- 0.25 + 0.04 * len - 0.0005 * len ^ 2
      de_alloc <- 0.40 + 0.04 * len - 0.0005 * len ^ 2
      gr  <- r0 * gr_alloc
      de_  <- d0 * de_alloc
      res <- list(bv       = bv,
                  C_       = C_,
                  N        = N,
                  gr_alloc = gr_alloc,
                  gr       = gr,
                  de_alloc = de_alloc,
                  de_      = de_)
      return(res)
  }

  model <- function(t, x, parms, approx_len_p, approx_len_f1, allo_rates) {
    with(as.list(c(parms, x)), {

      p_len <- approx_len_p(t)
      f_len <- approx_len_f1(t)
      blole <- input$growth_block_start + tga 

      p_ar <- allo_rates(p_len, r0_p, d0_p)
      f_ar <- allo_rates(p_len, r0_f1, 1)

      nc_ <- unname(c(p_p * p_ar$N, p_f1 * f_ar$N))
      nc <- sum(nc_)
      k <- 1 -  (nc / p0)
      dp_p  <- p_ar$gr * k * p_p   - p_ar$de_ * p_p
      dp_f1 <- ifelse(t < blole, 0, f_ar$gr) * k * p_f1
             - ifelse(t < blole, 0, f_ar$de_) * p_f1
      res <- c(dp_p, dp_f1)
      list(res)
    })
  }

    times <- seq(0,  input$end_time)
    tl    <- length(times)

    bs    <- input$growth_block_start
    len   <- tga
    be    <- input$growth_block_start + len

    lenv  <- input$maxlen:input$minlen
    pb_l  <- length(lenv)

    rates <- data.frame(Time = times,
                                len_p = rep(input$minlen, tl),
                                len_f = rep(input$minlen, tl))

    ala <- 1:min(pb_l, bs)
    olo <- (be + 1):min(be + (pb_l  - bs), tl)

    rates$len_p[ala]  <- lenv[ala]
    if (pb_l > bs) {
      rates$len_p[olo]  <- lenv[-ala][seq_len(length(olo))]
    }
    rates$len_p[bs:be] <- lenv[length(ala)]

    rates$len_f[1:be] <- 0
    rates$len_f[(be + 1):(min(be + pb_l, tl))] <- head(lenv, tl - be)

    p_lenappro <- approxfun(rates$Time, rates$len_p,  rule = 2)
    f_lenappro <- approxfun(rates$Time, rates$len_f,  rule = 2)


    xstart <- c(p_p = input$p_conc, p_f1 = a__ * input$p_conc)

    parms <- c(r0_p  = input$growth_rate_p,
               r0_f1 = fgr,
               d0_p  = input$death_rate,
               p0 = unname(xstart[1] + xstart[2]) *
                           allorates(input$maxlen, 0, 0)$N)

    out <- ode(y = xstart, times = times, func = model, parms = parms,
               approx_len_p = p_lenappro, approx_len_f1 = f_lenappro,
               allo_rates = allorates)

    colnames(out) <- c("Time", "Parental concentration", "F1 concentration")
    return(out)
}
