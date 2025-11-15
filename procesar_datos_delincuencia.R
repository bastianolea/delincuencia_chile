library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(furrr)

source("funciones_delincuencia.R")

# este proceso carga los datos descargados en obtener_datos_delincuencia.R y los va extrayendo como tablas
# mediante un loop que va por comuna y por años. Luego, los datos son ordenados y filtrados para ser recibidos

# el script también sirve para procesar datos nuevos, y agregarlos incrementalmente a la versión anterior de los datos

# cargar resultados de scraping
# datos_cead <- readr::read_rds("datos/cead_crudo_casospoliciales_2018_2024.rds")
# datos_cead <- readr::read_rds("datos/cead_crudo_casospoliciales_2024_2.rds") # actualización de datos 2024 (17 de diciembre 2024, datos nuevos hasta septiembre)
# datos_cead <- readr::read_rds("datos/cead_crudo_casospoliciales_2024_3.rds") # actualización de datos 2025 (31 de mayo, datos nuevos hasta diciembre 2024)
# datos_cead <- readr::read_rds("datos/cead_crudo_casospoliciales_2025_1.rds") # actualización de datos 2025 (13 de noviembre 2025, datos nuevos hasta junio)
datos_cead <- readr::read_rds("datos/cead_crudo_casospoliciales_2018_2025.rds") # actualización de datos 2025 (13 de noviembre 2025, datos nuevos hasta junio)

#comunas a calcular
comunas_por_calcular <- cargar_comunas()$cut_comuna

# limpieza ----

# por cada comuna
cead_limpiada <- cead_limpiar_resultados(datos_cead, comunas_por_calcular)


#ordenar ----

# filtrar delitos que en la tabla son agrupaciones de delitos individuales, como categorías de delitos que engloban delitos similares,
# para así dejar solo delitos puntuales, de lo contrario al sumar los delitos habrían conteos repetidos y la cifra sería incorrecta
cead_limpiada_2 <- cead_limpiada |> 
  #sacar categorías que son agrupaciones de delitos individuales
  filter(!delitos %in% c(
    "Delitos contra la vida o integridad de las personas",
    "Homicidios y femicidios",
    "Violaciones y delitos sexuales",
    # "Robos con violencia o intimidación", #(repetida)
    "Robos violentos",
    "Delitos contra la propiedad no violentos",
    "Robos en lugares habitados y no habitados",
    "Robos de vehículos y sus accesorios",
    "Incivilidades")) |> 
  # saacr delitos menores o irrelevantes cubiertos por agrupaciones
  filter(!delitos %in% c(
    # cubiertos por "Violencia intrafamiliar"
    "Violencia intrafamiliar con lesiones físicas",
    "Violencia intrafamiliar con lesiones psicológicas",
    "Maltrato habitual",
    "Amenazas en contexto de violencia intrafamiliar",
    "Violencia intrafamiliar no clasificada",
    
    # cubiertos por "Delitos asociados a drogas"
    "Tráfico de sustancias",
    "Microtráfico de sustancias",
    "Elaboración o producción de sustancias",
    "Otras infracciones a la ley de drogas",
    
    # cubiertos por "Delitos asociados a armas"
    "Disparo injustificado",
    "Porte / posesión de armas o explosivos",
    "Otras infracciones a la ley de armas",
    
    # cubiertos por "Amenaza falta o riña"
    "Amenaza con armas (falta)",
    "Riña Pública",
    
    # cubiertos por "Consumo de alcohol y drogas en la vía pública"
    "Consumo de drogas en la vía pública",
    "Porte de drogas",
    "Otras faltas a la ley de drogas",
    "Consumo de alcohol en la vía pública",
    
    # cubiertos por "Otras incivilidades"
    "Animales sueltos en la vía pública",
    "Comercio ilegal",
    "Ofensas al pudor",
    "Otras incivilidades",
    
    # en sección "Otros delitos o faltas", sólo considerar "Robo frustrado"
    "Abigeato",
    # "Robo frustrado"
    "Otros delitos o faltas" #(repetido)
  ))

# repetidos en grupo y subgrupo:
# "Robos con violencia o intimidación", #(repetida)
# "Otros delitos o faltas" #(repetido)

# cead_limpiada_2 |> 
#   filter(cut_comuna == 1101,
#          año == 2024, mes == 4) |> 
#   select(delitos, cifra) |> 
#   print(n=Inf)

# dentro de los delitos que son agrupaciones de otros delitos, el grupo "Robos con violencia o intimidación" 
# contiene un delito del mismo nombre, por lo que hay que filtrarlo distinto dado que se llaman igual
cead_limpiada_3 <- cead_limpiada_2 |> 
    # filter(cut_comuna == 1101,
    #        año == 2024, mes == 4) |> 
    # # select(delitos, cifra) |>
  # "Robos con violencia o intimidación" #sale dos veces, una es agrupación con robo violento de vehiculo motorizado y otra es el real
  mutate(arreglar = case_when(delitos %in% c("Robos con violencia o intimidación", "Otros delitos o faltas") ~ TRUE,
                              .default = FALSE)) |> 
  # filtrarlos por orden, porque desde la tabla vienen ordenados (primero la suma, luego el dato aislado); es decir, en vez de haber 12 filas del delito (una por mes), hay 24
  group_by(delitos, cut_comuna, año) |> 
  mutate(arreglar_n = ifelse(arreglar == TRUE, 1:n(), NA),
         arreglar_n_total = n()) |> #cantidad de filas, ergo de meses... en el delito repetido son 24, en otros 12 o menos
  ungroup() |> 
  # dejar solamente el delito, no el grupo
  filter(arreglar == FALSE | (arreglar == TRUE & arreglar_n > arreglar_n_total/2)) |> # para que aplique si hay 1 o 2
  select(-arreglar, -arreglar_n, -arreglar_n_total)

n_distinct(cead_limpiada_4$delito)

cead_limpiada_3 |> 
  distinct(delitos) |> 
  print(n=Inf)


# crear fecha, corregir formatos de columnas
cead_limpiada_4 <- cead_limpiada_3 |> 
  mutate(fecha = lubridate::ymd(paste(año, mes, 1))) |> 
  select(fecha, cut_comuna, delito = delitos, delito_n = cifra) |>
  mutate(delito_n = as.numeric(delito_n),
         cut_comuna = as.numeric(cut_comuna)) |> 
  arrange(cut_comuna, fecha) |> 
  left_join(cargar_comunas(), join_by(cut_comuna)) |> 
  mutate(across(where(is.character), as.factor)) |> 
  select(comuna, cut_comuna, region, cut_region, fecha, delito, delito_n, everything())


max(cead_limpiada_4$fecha)

# cortar fecha de corte de la base de datos manualmente, porque cead reporta 0 delitos en meses donde no tiene datos, o sea que si la base llega hasta marzo de 2024, abril 2024 muestra 0
cead_limpiada_5 <- cead_limpiada_4 |> 
  # filter(fecha <= "2024-03-01")
  # filter(fecha <= "2024-09-01")
  filter(fecha <= "2025-06-01")

# revisar
waldo::compare(cead_limpiada_4, cead_limpiada_5)
# visdat::vis_miss(cead_limpiada_5, warn_large_data = F)

# cead_limpiada_4 |>
#   filter(fecha > "2024-09-01") |>
#   summarize(max(delito_n))

# cead |> count(delito)



# unir con datos anteriores (opcional) ----
# si se está ejecutando el script para obtener datos nuevos, des-comentar este paso y ajustar las fechas de corte
# para que se carguen los datos existentes y se les agreguen los datos nuevos

# # actualización de datos 2024 (17 de diciembre 2024, datos nuevos hasta septiembre)
# cead_nuevo <- cead_limpiada_5
# cead_anterior <- arrow::read_parquet("app/cead_delincuencia.parquet")
# 
# # revisar fechas de datasets
# cead_nuevo |>
#   summarize(min(fecha),
#             max(fecha))
# 
# cead_anterior |>
#   summarize(min(fecha),
#             max(fecha))
# 
# cead_anterior |>
#   filter(fecha < "2023-01-01") |>
#   summarize(min(fecha),
#             max(fecha))
# 
# waldo::compare(cead_anterior |> pull(delito) |> unique() |> sort() |> as.character(),
#                cead_nuevo |> pull(delito) |> unique() |> sort() |> as.character())
# # cambiaron las clasificaciones!
# 
# #unir
# cead_unido <- cead_anterior |>
#   bind_rows(cead_nuevo) |>
#   arrange(fecha, comuna, delito)
# # (hasta aquí el proceso de actualización de base anterior)

# (si no se actualiza base anterior, ejecutar lo siguiente)
cead_unido <- cead_limpiada_5




# revisar ----
cead_unido |> distinct(delito) |> print(n=Inf)

cead_unido |> filter(cut_comuna == 1101) |> 
  filter(fecha == max(fecha)) |> 
  arrange(delito) |> 
  print(n=Inf)

cead_unido |> 
  summarize(min(fecha),
            max(fecha))

cead_unido |> 
  group_by(comuna) |> 
  summarize(n = n())

#revisar visualmente
library(ggplot2)

cead_unido |> 
  summarize(delito_n = sum(delito_n), .by = c(fecha)) |> 
  ggplot(aes(fecha, delito_n)) +
  geom_line() +
  theme(legend.position = "none")

cead_unido |> 
  summarize(delito_n = sum(delito_n), .by = c(fecha, comuna)) |> 
  ggplot(aes(fecha, delito_n, color = comuna)) +
  geom_line() +
  theme(legend.position = "none")

cead_unido |> 
  summarize(delito_n = sum(delito_n), .by = c(fecha, delito)) |> 
  ggplot(aes(fecha, delito_n, color = delito)) +
  geom_line() +
  theme(legend.position = "none")


# guardar -----
# readr::write_csv2(cead, "datos/cead_incivilidades.csv")
# arrow::write_parquet(cead, "datos/cead_delincuencia.parquet")
# cead <- arrow::read_parquet("datos/cead_delincuencia.parquet")

# para la app
arrow::write_parquet(cead_unido, "app/cead_delincuencia.parquet")
# cead_unido <- arrow::read_parquet("app/cead_delincuencia.parquet")

# para usuarios
arrow::write_parquet(cead_unido, "datos_procesados/cead_delincuencia_chile.parquet")
readr::write_rds(cead_unido, "datos_procesados/cead_delincuencia_chile.rds", compress = "gz")
# readr::write_csv2(cead_unido, "datos_procesados/cead_delincuencia_chile.csv")

