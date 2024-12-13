# Interfaz de usuario
ui <- fluidPage(
  navbarPage("Dashboard: Capacitación en Cultura y Liderazgo",
             tabPanel("Feedback",
                      fluidRow(
                        column(12,
                               h3("Análisis del Feedback recibido"),
                               p("Este gráfico muestra cómo el feedback recibido de los empleados 
                                 evoluciona a lo largo del tiempo, tanto históricamente como en 
                                 las proyecciones futuras."),
                               plotOutput("feedbackPlot")
                        )
                      )
             ),
             tabPanel("Campañas",
                      fluidRow(
                        column(12,
                               h3("Análisis de las Campañas realizadas"),
                               p("Este gráfico presenta la relación entre la cantidad de campañas realizadas y 
                                 el tiempo, indicando el impacto de estas iniciativas sobre la capacitación."),
                               plotOutput("campaignsPlot")
                        )
                      )
             ),
             tabPanel("Tasa de clics",
                      fluidRow(
                        column(12,
                               h3("Análisis de la Tasa de clics (CTR)"),
                               p("Aquí se visualiza cómo evoluciona la tasa de clics (CTR) asociada con las 
                                 campañas de capacitación a lo largo del tiempo."),
                               plotOutput("ctrPlot")
                        )
                      )
             )
  )
)

