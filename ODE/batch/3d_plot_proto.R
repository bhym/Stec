library("plotly")

pl1 <-   plot_ly(lsw_f1, x = ~`rprf1`, y= ~dere, z = ~value,
          color = ~a_, type = "scatter3d", mode = "markers") %>%
    colorbar(title = "alpha")

pl2 <-   plot_ly(lsw_P, x = ~`rprf1`, y= ~dere, z = ~value,
          color = ~a_, type = "scatter3d", mode = "markers") %>%
    colorbar(title = "Alpha")

