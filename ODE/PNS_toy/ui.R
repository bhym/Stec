library("plotly")
fluidPage(titlePanel("Diatom P vs F1 toy model"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "p_conc",              label = "Concentration of parental cells",       step = 1,    min = 0, max = 1E6,  value = 4E3),
      numericInput(inputId = "alpha_",              label = "Fraction of F1 cells",                  step = 0.01, min = 0, max = 1,    value = 0.012),
      numericInput(inputId = "death_rate",          label = "Basal cell removal rate",               step = 0.05, min = 0, max = 1,    value = 0.3),
      numericInput(inputId = "end_time",            label = "Simulation time length",                step = 1,    min = 1, max = 1000, value = 15),
      numericInput(inputId = "growth_block_start",  label = "Starting day of growth block",          step = 1,    min = 1, max = 1000, value = 2),
      numericInput(inputId = "growth_block_length", label = "Length of the growth block",            step = 1,    min = 0, max = 1000, value = 3),
      numericInput(inputId = "growth_rate_p",       label = "Parental growth rate when not blocked", step = 0.01, min = 0, max = 1,    value = 1.06),
      numericInput(inputId = "growth_rate_f1",      label = "F1 growth rate",                        step = 0.01, min = 0, max = 10,   value = 0.58),
      numericInput(inputId = "minlen",              label = "Minimum length of PN cells",            step = 1,    min = 1, max = 100,    value = 38),
      numericInput(inputId = "initlen_p",           label = "Starting length of parental cells",     step = 1,    min = 1, max = 100,    value = 40),
      numericInput(inputId = "initlen_f",           label = "Starting length of F1 cells",           step = 1,    min = 1, max = 100,    value = 55),
      numericInput(inputId = "k",                   label = "N carrying capacity Factor",            step = 0.1,  min = 1, max = 100,    value = 1.2),
      ),
    mainPanel(
              plotlyOutput(outputId = "plot"),
              plotlyOutput(outputId = "plot2"),
              plotlyOutput(outputId = "plot3")
              )
    )
)
