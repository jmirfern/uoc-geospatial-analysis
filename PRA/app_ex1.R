library(shiny)
library(leaflet)
library(dplyr)
library(shinyjs)
library(DT)
library(sf)
air_data <- readRDS("air_mad.RDS")
temp_dir <- tempdir()
unzip("Distritos.zip", exdir = temp_dir)
distritos <- st_read(temp_dir) %>% st_transform(crs = 4326)
ui <- fluidPage(titlePanel("Calidad del Aire en Madrid"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "pollutant",
                      "Selecciona Contaminante:",
                      choices =
                        unique(air_data$nom_abv)
                    ),
                    dateInput(
                      "date",
                      "Selecciona Fecha:",
                      value = Sys.Date(),
                      max =
                        Sys.Date()
                    ),
                    selectInput(
                      "station",
                      "Selecciona Estació:",
                      choices = NULL # Es carregarà dinàmicament
                    )
                  ),
                  mainPanel(
                    leafletOutput(outputId = "map", height = "500px"),
                    DT::dataTableOutput("data_table"),
                    uiOutput("no_data_message")
                    # User feedback on data availability
                  )
                ))
server <- function(input, output, session) {
  # En la parte del servidor, actualiza el selector de estaciones
  observe({
    req(input$pollutant, input$date)
    estaciones <- air_data %>%
      filter(nom_abv == input$pollutant, fecha == as.Date(input$date)) %>%
      pull(id_name) %>%
      unique()
    
    # Añade "Todas las estaciones" al principio de la lista
    choices <- c("Todas las estaciones" = "all", setNames(estaciones, estaciones))
    updateSelectInput(session, "station", choices = choices)
  })
  filtered_data <- reactive({
    req(input$pollutant, input$date, input$station)
    
    base_filter <- air_data %>%
      filter(
        nom_abv == input$pollutant,
        fecha == as.Date(input$date)
      )
    
    if (input$station != "all") {
      base_filter <- base_filter %>%
        filter(id_name == input$station)
    }
    
    base_filter
  })
  output$map <- renderLeaflet({
    data_for_map <- filtered_data()
    req(nrow(data_for_map) > 0) # Ensure data is available before proceeding
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data_for_map$valor,
      na.color = "#FFFFFF"
    )
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = distritos,
        fillColor = "#ffffff",
        weight = 1,
        color =
          "#000000",
        fillOpacity = 0.5
      ) %>%
      addCircleMarkers(
        data = data_for_map,
        ~ longitud,
        ~ latitud,
        radius = 5,
        color = "#007bff",
        fill = TRUE,
        fillColor = ~ pal(valor),
        fillOpacity =
          0.7,
        popup = ~ paste(nom_mag, valor, ud_med)
      )
  })
  output$data_table <- DT::renderDataTable({
    req(filtered_data()) # Ensure data is loaded before proceeding
    data_for_table <- filtered_data() %>%
      select(id_name, fecha, valor, nom_mag, ud_med)
    # Select specific columns for the table
    datatable(data_for_table, options = list(pageLength = 5, autoWidth = TRUE))
  })
  output$no_data_message <- renderUI({
    if (nrow(filtered_data()) == 0) {
      tags$div(
        style = "color: red; font-weight: bold;",
        "No hi ha dades disponibles per a la selecció."
      )
    }
  })
}
shinyApp(ui = ui, server = server)
