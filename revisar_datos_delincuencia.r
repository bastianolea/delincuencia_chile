library(dplyr)
library(ggplot2)
library(slider)
library(lubridate)
library(stringr)
library(forcats)

#cargar datos ----
delincuencia <- arrow::read_parquet("datos/cead_delincuencia.parquet") |> 
  rename(delitos = delito_n)
