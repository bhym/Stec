library("plotly")
fluidPage(titlePanel("Diatom P vs F1 toy model"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "p_conc",                 label = "Concentration of parental cells",                    step = 1,    min = 0, max = 1E6,  value = 4E3),
      numericInput(inputId = "alpha_",                 label = "Fraction of F1 cells",                               step = 0.01, min = 0, max = 1,    value = 0.012),
      numericInput(inputId = "death_rate",             label = "Basal Parental cell removal rate",                   step = 0.05, min = 0, max = 1,    value = 0.3),
      numericInput(inputId = "end_time",               label = "Simulation time length",                             step = 1,    min = 0, max = 1000, value = 15),
      numericInput(inputId = "growth_block_start",     label = "Starting day of growth block",                       step = 1,    min = 0, max = 1000, value = 2),
      numericInput(inputId = "growth_block_length",    label = "Length of the growth block",                         step = 1,    min = 0, max = 1000, value = 3),
      numericInput(inputId = "growth_rate_p",          label = "Parental growth rate when not blocked",              step = 0.01, min = 0, max = 1,    value = 0.38),
      numericInput(inputId = "growth_rate_f1",         label = "F1 growth rate when not blocked",                    step = 0.01, min = 0, max = 10,   value = 1.2),
      numericInput(inputId = "time_to_reach_min_size", label = "Time needed to reach non-viable size",               step = 1,    min = 0, max = 500,  value = 10),
      numericInput(inputId = "dday",                   label = "Day of gametogenesis-induced death of P generation", step = 1,    min = 0, max = 500,  value = 3),
      numericInput(inputId = "gid",                    label = "Parental gametogenesis-induced death rate",          step = 0.01, min = 0, max = 1,    value = 0.8),
      numericInput(inputId = "carcap",                 label = "Carrying capacity",                                  step = 0.01, min = 0.1, max = 1E6,    value = 0.8)

      ),
    mainPanel(
              plotlyOutput(outputId = "plot"),
              plotlyOutput(outputId = "plot2")
              )
    )
)
