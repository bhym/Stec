library("shiny")
library("plotly")
library("deSolve")
function(input, output) {
  model <- function(t, x, parms, approx_gp, approx_gf1, approx_dp) {
    with(as.list(c(parms, x)), {
      gr_p <- approx_gp(t)
      r_f1 <- approx_gf1(t)
      d_p <- approx_dp(t)
    # ma <- 1 - t/11
      k <- 1 - (p_p + p_f1) / (p0 * input$carcap)
      dp_p  <- r0_p  * k * gr_p  * p_p  - d_p * p_p
    # dp_p  <- r0_p  * k * gr_p  * p_p  - m * ifelse(1 - gr_p != 0, gr_p,0.5) * p_p
    # dp_p  <- r0_p  * k * gr_p  * p_p  - m * ifelse(ma > 0, ma,0.6) * p_p
      dp_f1 <- r0_f1 * k * r_f1 * p_f1
      res <- c(dp_p, dp_f1)
      list(res)
    })
  } 

  inis <- reactive({
    times         <- seq(0, input$end_time)
    tl            <- length(times) 

    bs            <- input$growth_block_start
    len           <- input$growth_block_length
    be            <- (input$growth_block_start + len)
    trms          <- input$time_to_reach_min_size
    ph_rt         <- input$dday
    
    ga            <- seq(0.1, 1, len = trms)
    pbl           <- ga ** 2
    pb_l          <- length(pbl)
    
    rates         <- data.frame(Time = times,
                                gr_p = rep(0, tl),
                                gr_f1 = rep(0, tl),
                                dr_p  = rep(input$death_rate,tl))

    ala <- 1:min(pb_l,bs)
    olo <- (be+1):min(be+(pb_l-bs), tl)

    rates$gr_p[ala]  <- pbl[ala]
    if(pb_l > bs) {
      rates$gr_p[olo]  <- tail(pbl, -(pb_l-length(olo)))
    }
    rates$gr_f1[(be+1):(min(be+pb_l,tl))] <- head(pbl,tl-be)

    rates$dr_p[rates$Time >= ph_rt & rates$Time <= ph_rt] <- input$gid

    rates_p_approx  <- approxfun(rates$Time, rates$gr_p,  rule = 2)
    rates_f1_approx <- approxfun(rates$Time, rates$gr_f1, rule = 2)
    rates_dp_approx  <- approxfun(rates$Time, rates$dr_p,  rule = 2)

    xstart <- c(p_p  = input$p_conc, p_f1 = input$alpha_ * input$p_conc)
    
    parms <- c(r0_p = input$growth_rate_p,
               r0_f1 = input$growth_rate_f1,
               p0 = unname(xstart[1] + xstart[2]))
    
    out <- ode(y = xstart, times = times, func = model, parms = parms,
                 approx_gp = rates_p_approx, approx_gf1 = rates_f1_approx, approx_dp = rates_dp_approx)

    colnames(out) <- c("Time", "Parental concentration", "F1 concentration")

    return(list(out,rates))
  })
    
  output$plot <- renderPlotly(
    fig1  <- plot_ly(as.data.frame(inis()[[1]]),
                    x = ~Time, y = ~`Parental concentration`, name = "Parental",
                    type = 'scatter', mode = 'lines+markers') %>%
             add_trace(y = ~`F1 concentration`, name = "F1",
                       mode = 'lines+markers') %>%
             layout(yaxis = list(title = "Concentration", type = "log"))
  )

  output$plot2 <- renderPlotly(
    fig2  <- plot_ly(inis()[[2]],
                    x = ~Time, y = ~ gr_p, name = "Parental growth rate",
                    type = 'scatter', mode = 'lines+markers') %>%
             add_trace(y = ~gr_f1, name = "F1 growth rate",
                       mode = 'lines+markers') %>%
             layout(yaxis = list(title = "Relative rate"))
  )
}
