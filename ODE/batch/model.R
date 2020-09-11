
mollo <- function(p_,f1_,m_) {
  model <- function(t, x, parms, inp_p, inp_f1) {
    with(as.list(c(parms, x)), {
      r_p <- inp_p(t)
      r_f1 <- inp_f1(t)
      k <- 1 - (p_p + p_f1) / p0
      dp_p  <- r0_p  * k * r_p  * p_p  - m * p_p
      dp_f1 <- r0_f1 * k * r_f1 * p_f1
      res <- c(dp_p, dp_f1)
      list(res)
    })
  } 

    times         <- seq(0, input$end_time, by = input$delta_t)
    dit           <- 1 / input$delta_t
    bs            <- input$growth_block_start * dit
    lag_          <- bs + dit
    effective_len <- input$growth_block_length - 1
    be            <- (input$growth_block_start + effective_len) * dit
    trms          <- input$time_to_reach_min_size
    
    ga            <- seq(0.1, 1, by = input$delta_t / trms)
    ga_l          <- length(ga)
    pbl           <- ga ** 2
    
    sig           <- data.frame(times = times,
                                gr_p  = rep(0, length(times)),
                                gr_f1 = rep(0, length(times)))
    
    sig[1:bs, ]$gr_p                <- pbl[1:bs]
    sig[be:(ga_l + bs), ]$gr_p      <- pbl[bs:ga_l]
    sig[be:(ga_l + be - 1), ]$gr_f1 <- pbl
    
    sig_p_imp  <- approxfun(sig$times, sig$gr_p,  rule = 2)
    sig_f1_imp <- approxfun(sig$times, sig$gr_f1, rule = 2)
    
    xstart <- c(p_p  = input$p_conc,
                p_f1 = input$alpha_ * input$p_conc)
    
    parms <- c(m = m_, r0_p = p_, r0_f1 = f1_,
               p0 = unname(xstart[1] + xstart[2]))
    
     out <- ode(y = xstart, times = times, func = model, parms = parms,
                  inp_p = sig_p_imp, inp_f1 = sig_f1_imp)
    return(out)
}
