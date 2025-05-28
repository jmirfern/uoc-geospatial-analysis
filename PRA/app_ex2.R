library(shiny)
library(leaflet)
library(dplyr)
library(shinyjs)
library(DT)
library(sf)
library(ggplot2)
air_data <- readRDS("air_mad.RDS")
temp_dir <- tempdir()
unzip("Distritos.zip", exdir = temp_dir)
distritos <- st_read(temp_dir) %>% st_transform(crs = 4326)
ui <- fluidPage(titlePanel("Calidad del Aire en Madrid"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("pollutant", "Selecciona Contaminant:", choices = unique(air_data$nom_abv)),
                    selectInput("year", "Selecciona Any:", choices = sort(unique(format(air_data$fecha, "%Y")))),
                    selectInput("station", "Selecciona Estació:", choices = NULL)
                  ),
                  mainPanel(
                    leafletOutput(outputId = "map", height = "500px"),
                    plotOutput("line_plot"),
                    uiOutput("no_data_message")
                  )
                ))
server <- function(input, output, session) {
  # En la parte del servidor, actualiza el selector de estaciones
  observe({
    req(input$pollutant, input$year)
    estacions <- air_data %>%
      filter(nom_abv == input$pollutant, format(fecha, "%Y") == input$year) %>%
      pull(id_name) %>%
      unique()
    
    # Añade "Todas las estaciones" al principio de la lista
    choices <- c("Totes les estacions" = "all", setNames(estacions, estacions))
    updateSelectInput(session, "station", choices = choices)
  })
  filtered_data <- reactive({
    req(input$pollutant, input$year, input$station)
    
    base_filter <- air_data %>%
      filter(
        nom_abv == input$pollutant,
        format(fecha, "%Y") == input$year
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
        popup = ~ paste(nom_mag, valor, ud_med),
        layerId = ~id_name
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
  output$line_plot <- renderPlot({
    req(input$station != "all", nrow(filtered_data()) > 0)
    df <- filtered_data()
    ggplot(df, aes(x = fecha, y = valor)) +
      geom_line(color = "#00bcd4", size = 1.2) +
      geom_point(color = "salmon", fill = "white", size = 2, shape = 21, stroke = 1) +
      labs(
        title = paste("Observaciones anuales para", df$id_name[1]),
        x = "Fecha",
        y = "Valor del Contaminante"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  observeEvent(input$map_marker_click, {
    # Només permet la selecció interactiva si està seleccionada "Totes les estacions"
    if (input$station == "all") {
      station_clicked <- input$map_marker_click$id
      updateSelectInput(session, "station", selected = station_clicked)
    }
  })
}
shinyApp(ui = ui, server = server)
