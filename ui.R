library(shiny)
library(ggplot2)

fluidPage(
  # Título de la aplicación
  titlePanel("Scenarios Treatment effect"), hr(),
  
  # Sidebar con los controles de entrada
  sidebarLayout(
    sidebarPanel(HTML("Parameters"),
                 # Numeric Inputs
                 tabsetPanel(id = "navbar",
                             tabPanel("Distribution",
                                      # Select inputs
                                      numericInput("delta_0",       label = HTML("Treatment effect (&delta;)"),      value = -1),
                                      sliderInput("sd_effect",       label = HTML("SD of Treatment effect (&sigma;<sub>E</sub>)"),      value = 0, min=0,max=3, step=0.1),
                                      numericInput("sd_change",       label = HTML("SD of change over time (&sigma;<sub>P</sub>)"),      value = 1),
                                      sliderInput("no_responders", label = HTML("Proportion of no responders (p<sub>NR</sub>)"), value = 0, min=0,max=1, step=0.05),
                                      
                                      checkboxInput("multiplicative", label = "Multiplicative effect", value = FALSE),
                                      
                                      selectInput("variable",
                                                  label = "Select an indicator", 
                                                  choices = c("SD of change over time " = "SD_CHANGE","SD of Effect" = "SD_EFFECT","Treatment Effect"="TREATMENT_EFFECT","Proportion of Non-Responders"= "PROP_NORES")),
                                      selectInput("factor",
                                                  label = "Maximum factor applied to the indicator", 
                                                  choices = c("1.2x" = 1.2,"1.5x"=1.5,"2x"=2,"4x"=4,"8x"=8,"16x"=16)),
                                      
                                      # Action button
                                      actionButton("go","Run")
                             ),
                             tabPanel("Simulation",
                                      numericInput("n_sim",         label = "Number of simulations", value = 500),
                                      numericInput("n",             label = "Sample size",           value = 1000),
                                      numericInput("mean_baseline", label = HTML("Baseline mean (&mu;<sub>Z</sub>)"),         value = 0),
                                      numericInput("sd_baseline",   label = HTML("SD baseline (&sigma;<sub>Z</sub>)"),           value = 1)
                             )
                 )),
    
    # Cuerpo de la aplicación
    mainPanel(
      tabsetPanel(
        tabPanel("Scene", plotOutput(outputId = "figura1", height = 600)),
        tabPanel("Simulation", plotOutput(outputId = "figura2"),tableOutput(outputId = "taula1"))
      )
    )
  )
)


