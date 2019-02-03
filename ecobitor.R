library("shiny")
library("ggplot2")

anys<-c(0:8)
gasoil<-c(0,1540,3080,4620,6160,7700,9240,10780,12320)
datas<-data.frame(anys, gasoil)

# Define UI for slider demo app -------------------------------
ui <- fluidPage(
  
  # App title 
  titlePanel("Simulador de costes"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebars
    sidebarPanel(
      
      # Input GASOIL price
      sliderInput("range", "Precio del GASOIL (Euros/litro):",
                  min = 0, max = 2.5,
                  value = 0.77, step= 0.01),
      
      # Input GASOIL consumption
      sliderInput("integer", "Consumo anual de GASOIL (litros):",
                  min = 0, max = 10000,
                  value = 2000),
     
       #Separator between inputs
      hr(),
      
      selectInput("alter", "Possibles combustibles:", 
                  choices=c("PELLET","ALMENDRA","ESTELLA","SANSA")),
      
      
      # Input alternative energy price
      sliderInput("range2", "Precio del combustible alternativo (Euros/kg):",
                  min = 0, max = 2.5,
                  value = 0.2, step= 0.01),
      
      # Input alternative supply price
      sliderInput("integer2", "Consumo anual de combustible alternativo (kg):",
                  min = 0, max = 10000,
                  value = 3500)
    ),
    
    # Main panel for displaying outputs 
    mainPanel(
      
      # Output: Table summarizing the values entered 
      tableOutput("values"),
      
      fluidRow(div(class="span12", plotOutput("salesPlot")))
      
    )
  )
)

# Define server logic for slider examples ---------------------------
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values
  sliderValues <- reactive({
    
    data.frame(
      Datos = c("Tipo de combustible",
               "Combustible",
               "Precio (Euros/Litro)",
               "Poder calorifico (Kcal/litro)",
               "Rendimientos del sistema (%)",
               "Eficiencia Energetica (Kcal/litro)",
               "Produccion de energia",
               "Coste KWH (Euro/Kwh)",
               "Consumo anual (Litros)",
               "Produccion de energia (Kwh anuales)",
               "Coste anual (Euros)",
               "Inversion inicial (Euros)"),
      Actual = c("NO RENOVABLE",
                "GASOIL",
                input$range,
                8500,
                "88%",
                7480,
                8.7,
                input$range/8.7,
                input$integer,
                input$integer*8.7,
                input$integer*input$range,
                0),
      Alternativa = c("RENOVABLE",
                      input$alter,
                      input$range2,
                      4250,
                      "96%",
                      4080,
                      4.74,
                      input$range2/4.74,
                      input$integer2,
                      input$integer2*4.74,
                      input$integer2*input$range2,
                      3000),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table 
  output$values <- renderTable({
    sliderValues()
  })
  
  output$salesPlot <- renderPlot({

    # Get all data that has to be visible to the current user.
    costeanual<-input$integer2*input$range2
    costeanualdef<-c(3000,3000+costeanual,3000+(costeanual*2),3000+(costeanual*3),3000+(costeanual*4),3000+(costeanual*5),3000+(costeanual*6),3000+(costeanual*7),3000+(costeanual*8))
    
    rentabilidad<-(gasoil-costeanualdef)
    
    data <- data.frame(datas,costeanualdef,rentabilidad)
    
    # Generate the sales plot
    p <- plot(rentabilidad~anys, type="l", col="dark green", xlab="Tiempo transcurrido", ylab="Coste acumulado" , main="Recuperacion de la inversion inicial" )
    abline(h=0, lty=3)
    text(x=1, y=240, "Break Even")
    print(p)    
  })
  
}

# Create Shiny app -------------------------------------------------------
shinyApp(ui, server)

