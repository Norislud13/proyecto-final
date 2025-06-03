# app.R
library(shiny)

# Datos simulados de productos
productos <- data.frame(
  Producto = c("Fondo A", "Fondo B", "Fondo C", "Fondo D"),
  Rentabilidad = c(5.2, 6.8, 4.5, 7.1),  # en porcentaje
  Riesgo = c(2.1, 3.5, 1.8, 4.0)         # desviación estándar
)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Comparador de Productos de Inversión"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("min_rentabilidad",
                  "Rentabilidad mínima deseada (%):",
                  min = 0, max = 10, value = 5, step = 0.1),
      checkboxInput("ordenar", "Ordenar por rentabilidad", TRUE)
    ),
    
    mainPanel(
      tableOutput("tabla"),
      plotOutput("grafico")
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  datos_filtrados <- reactive({
    df <- subset(productos, Rentabilidad >= input$min_rentabilidad)
    if (input$ordenar) {
      df <- df[order(-df$Rentabilidad), ]
    }
    df
  })
  
  output$tabla <- renderTable({
    datos_filtrados()
  })
  
  output$grafico <- renderPlot({
    df <- datos_filtrados()
    
    if (nrow(df) == 0) {
      plot.new()
      title("No hay productos que cumplan con la rentabilidad mínima seleccionada")
      return()
    }
    
    barplot(df$Rentabilidad,
            names.arg = df$Producto,
            col = "steelblue",
            main = "Rentabilidad de productos seleccionados",
            ylab = "Rentabilidad (%)")
  })
}

# Ejecutar la app
shinyApp(ui = ui, server = server)
