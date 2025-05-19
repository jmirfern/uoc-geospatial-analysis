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
ui <- fluidPage(
    titlePanel("Calidad del Aire en Madrid"),
    sidebarLayout(
        sidebarPanel(
            selectInput("pollutant", "Selecciona Contaminante:",
                choices =
                    unique(air_data$nom_abv)
            ),
            dateInput("date", "Selecciona Fecha:",
                value = Sys.Date(), max =
                    Sys.Date()
            )
        ),
        mainPanel(
            leafletOutput(outputId = "map", height = "500px"),
            DT::dataTableOutput("data_table"),
            uiOutput("no_data_message") # User feedback on data availability
        )
    )
)

