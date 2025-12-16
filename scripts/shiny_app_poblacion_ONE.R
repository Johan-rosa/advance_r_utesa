library(shiny)
library(readxl)
library(ggplot2)

provincia_municipio <- excel_sheets(here::here("data/data_one.xlsx"))[-1]
leer_data_one <- function(ruta, hoja) {
  read_excel(
    ruta, 
    sheet = hoja,
    skip = 6
  ) |>
    rename(edad = 1) |>
    mutate(
      sexo = ifelse(
        str_detect(edad, "Ambos|Hombres|Mujeres"), 
        edad, 
        NA
      )
    ) |>
    fill(sexo) |> 
    filter(
      !str_detect(edad, "Fuente"),
      str_detect(edad, "\\d"),
      sexo != "Ambos sexos"
    )  |>
    suppressMessages()
}

ui <- fluidPage(
  h1("Análisis de población"),
  selectInput("provincia", "Provincia", choices = provincia_municipio, selected = "Azua"),
  textOutput("nombre_provincia"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$nombre_provincia <- renderText({
    total_2020 <- data_provincia() |>
      summarise(total = sum(`2020`))

    paste(
      "En el 2020, la población total de", 
      input$provincia, 
      "era de ", scales::comma(total_2020$total))
  })
  
  data_provincia <- reactive({
    leer_data_one(here::here("data/data_one.xlsx"), input$provincia)
  })
  
  data_provincia_long <- reactive({
    data_provincia() |>
      pivot_longer(matches("\\d"), names_to = "year", values_to = "poblacion")
  })
  
  output$plot <- renderPlot({
    data_provincia_long() |>
      summarise(
        poblacion = sum(poblacion),
        .by = c(year)
      ) |>
      ggplot(aes(x = year, y = poblacion, group = 1)) +
      geom_line() +
      scale_y_continuous(labels = scales::comma)
      
  })
}

shinyApp(ui, server)
