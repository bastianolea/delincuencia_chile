library(rvest)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)

#web scraping para obtener periodos presidenciales
sesion <- session("https://es.wikipedia.org/wiki/Presidente_de_Chile") |> 
  read_html()

tabla_presidentes <- sesion |> 
  html_table() |> 
  pluck(2) |> 
  janitor::clean_names()

#función que transforma meses en texto a numeros
mes_a_numero <- function(variable) {
  case_when(str_detect(variable, "Enero|enero") ~ 1,
            str_detect(variable, "Febrero|febrero") ~ 2,
            str_detect(variable, "Marzo|marzo") ~ 3,
            str_detect(variable, "Abril|abril") ~ 4,
            str_detect(variable, "Mayo|mayo") ~ 5,
            str_detect(variable, "Junio|junio") ~ 6,
            str_detect(variable, "Julio|julio", ) ~ 7,
            str_detect(variable, "Agosto|agosto") ~ 8,
            str_detect(variable, "Septiembre|septiembre") ~ 9,
            str_detect(variable, "Octubre|octubre") ~ 10,
            str_detect(variable, "Noviembre|noviembre") ~ 11,
            str_detect(variable, "Diciembre|diciembre") ~ 12)
}


tabla_presidentes_2 <- tabla_presidentes |> 
  mutate(mes_inicio = mes_a_numero(inicio),
         mes_termino = mes_a_numero(termino)) |> 
  mutate(dia_inicio = str_extract(inicio, "\\d{2}|^\\d{1}"),
         dia_termino = str_extract(termino, "\\d{2}|^\\d{1}")) |> 
  mutate(año_inicio = str_extract(inicio, "\\d{4}"),
         año_termino = str_extract(termino, "\\d{4}")) |> 
  mutate(fecha_inicio = dmy(paste(dia_inicio, mes_inicio, año_inicio)),
         fecha_termino = dmy(paste(dia_termino, mes_termino, año_termino)))
  

tabla_presidentes_3 <- tabla_presidentes_2 |> filter(year(fecha_inicio) >= 1970)

tabla_presidentes_3 |> readr::write_csv("datos/periodos_presidenciales_chile.csv")
