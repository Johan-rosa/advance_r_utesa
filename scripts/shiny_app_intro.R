library(shiny)

say_hello <- function(nombre) {
  upper_name <- toupper(nombre)   # Convierte el nombre a MAYÚSCULAS
  paste("Hola", upper_name)       # Construye un saludo
}

ui <- fluidPage(
  h1("Población según provincia"),
  textInput(inputId = "nombre", label = "Nombre"),
  numericInput(inputId = "edad", label = "Edad", value = 18),
  sliderInput(
    inputId = "salario", 
    label = "Salario mensual", min = 0, max = 100e3, value = 50e3
  ),
  selectInput(
    "pelicula", "Película preferida", 
    choices = c("El padrino", "Avatar", "Rápido y furioso"), selected = "Avatar"
  ),
  textOutput(outputId = "saludo")
)

server <- function(input, output, session) {
  output$saludo <- renderText({say_hello(input$nombre)})
  

  # Observar cualquier input dentro de observe
  observe({
    valores <- list(
      nombre = input$nombre,
      salario = input$salario,
      pelicula = input$pelicula,
      edad = input$edad
    )
    
    print(valores)
  })
  
  # Observar un evento particular
  # observeEvent(input$nombre, {
  #   print(say_hello(input$nombre))
  # })
}

shinyApp(ui, server)
