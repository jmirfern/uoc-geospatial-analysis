

library(tidyverse)
library(xml2)
library(vroom)



url <- "https://datos.madrid.es/egob/catalogo/201410-0-calidad-aire-diario.dcat"
page <- url %>%
  read_xml() %>%
  as_list()

location <- page[["RDF"]][["Catalog"]][["dataset"]][["Dataset"]]
location <- location[names(location) == "distribution"]

links <-
  tibble(
    year = sapply(X = location, function(x)
      unname(unlist(x[["Distribution"]][["title"]][1]))),
    link = sapply(X = location, function(x)
      unname(unlist(x[["Distribution"]][["accessURL"]][1])))
  )

links <- links %>% filter(str_detect(link, pattern = ".csv"))
links <- links %>%
  mutate(file_name = paste0("datos_aire_madrid_", year, ".csv"))
years <- links %>% filter(year >= "2013") %>% .$year

lapply(years, function(x) {
  file_x <- links %>%
    filter(year == x & str_detect(link, ".csv"))
  if (x == max(years)) {
    download.file(url = file_x$link,
                  destfile = paste0("data/", file_x$file_name))
  }
  if (!file.exists(paste0("data/", file_x$file_name))) {
    download.file(url = file_x$link,
                  destfile = paste0("data/", file_x$file_name))
  }
})

data <- vroom(paste0("data/", links %>% filter(year %in% years) %>% .$file_name))

cols_to_numeric <-
  c(
    "PROVINCIA",
    "MUNICIPIO",
    "ESTACION",
    "MAGNITUD",
    "PUNTO_MUESTREO",
    "ANO",
    "MES",
    str_subset(names(data), pattern = '^D')
  )
data <- data %>%
  mutate(across(all_of(cols_to_numeric), as.numeric))

write_rds(data, "data/data_raw.RDS")

air_mad <- data %>%
  gather(v, valor, D01:V31) %>%
  mutate(DIA = str_sub(v, 2, 3), v = str_sub(v, 1, 1))
air_mad <- air_mad %>%
  mutate(id = ESTACION, fecha = as.Date(paste(ANO, MES, DIA, sep = "-"))) %>%
  select(id, MAGNITUD, fecha, v, valor)
air_mad <- air_mad %>%
  unique() %>%
  pivot_wider(names_from = v, values_from = valor)
air_mad <- air_mad %>%
  mutate(valor = as.numeric(D)) %>%
  select(-D)

estaciones <- read_csv("info/estaciones.csv")
observaciones <- read_csv("info/observaciones.csv")
air_mad <- left_join(air_mad, estaciones, by = "id")
air_mad <- left_join(air_mad, observaciones, by = "MAGNITUD")
air_mad <- air_mad %>% select(-MAGNITUD)
write_rds(air_mad, "data/air_mad.RDS")
