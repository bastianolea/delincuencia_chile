library(dplyr)
library(rvest)
library(purrr)
library(RCurl)
library(ggplot2)

#https://cead.spd.gov.cl/estadisticas-delictuales/

#este proceso genera requests para descargar las estadísticas de CEAD usando su API (privada), con un reposo entre request del
#doble de tiempo en que tardó en entregar los datos. Los delitos se eligen con el argumento `delito` de la función cead_generar_request()

source("funciones_delincuencia.R")

#comunas a calcular
comunas_por_calcular <- cargar_comunas()$cut_comuna
# comunas_por_calcular = 1101

# años_elegidos = 2010:2023
# años_elegidos = 2010:2024
años_elegidos = 2017:2024

# scraping por api ----
# ejecuta la obtención de datos, por comuna y por año, haciendo requests al sitio de cead

# por cada comuna
datos_cead <- cead_descargar_datos(años_elegidos, comunas_por_calcular)

# guardar información cruda
readr::write_rds(datos_cead, "datos/cead_crudo_casospoliciales_2017_2024.rds", compress = "gz")
# datos_cead <- readr::read_rds("datos/cead_crudo_casospoliciales_2014_2024.rds")

# datos_cead <- readr::read_rds("datos/cead_delincuencia_crudo_todos_2010_2024.rds")

#—----


#este proceso carga los datos descargados y los va extrayendo como tablas
#mediante un loop que va por comuna y por años. Luego, los datos son ordenados y filtrados para ser recibidos


# limpieza ----
# datos_cead_2020_2024 <- readr::read_rds("datos/cead_delincuencia_crudo_todos_2010_2024.rds")
# datos_cead_2024_3 <- readr::read_rds("datos/cead_delincuencia_crudo_todos_2024_3.rds")

#por cada comuna
cead_limpiada <- cead_limpiar_resultados(datos_cead, comunas_por_calcular)

# cead_limpiada <- bind_rows(cead_limpiada, cead_limpiada2)


#ordenar ----
# filtrar delitos que en la tabla son agrupaciones de delitos individuales, como categorías de delitos que engloban delitos similares,
# para así dejar solo delitos puntuales, de lo contrario al sumar los delitos habrían conteos repetidos y la cifra sería incorrecta
cead_limpiada_2 <- cead_limpiada |> 
  #sacar categorías que son agrupaciones de delitos individuales
  filter(!delitos %in% c("Delitos de mayor connotación social", "Incivilidades",
                         "Violencia intrafamiliar",
         "Delitos violentos", "Delitos contra la propiedad no violentos",
         "Delitos asociados a armas", "Otros delitos o faltas",
         "Homicidios y femicidios", "Violaciones y delitos sexuales",
         "Crímenes y simples delitos ley de armas", "Robos en lugares habitados y no habitados",
         "Robos en vehículos y sus accesorios", "Otras incivilidades"))

# cead_limpiada_2 |> arrange(año, delitos, cut_comuna) |> filter(año == 2020) |> print(n=100)
# unique(cead_limpiada_2$mes)

# dentro de los delitos que son agrupaciones de otros delitos, el grupo "Robos con violencia o intimidación" contiene un delito del mismo nombre, por lo que hay que filtrarlo distinto dado que se llaman igual
cead_limpiada_3 <- cead_limpiada_2 |> 
  # "Robos con violencia o intimidación" #sale dos veces, una es agrupación con robo violento de vehiculo motorizado y otra es el real
  filter(delitos %in% c("Robos con violencia o intimidación", "Robo violento de vehículo motorizado", "Otros delitos sexuales")) |> 
  mutate(arreglar = case_when(delitos == "Robos con violencia o intimidación" ~ TRUE,
                              .default = FALSE)) |> 
  # filtrarlos por orden, porque desde la tabla vienen ordenados (primero la suma, luego el dato aislado); es decir, en vez de haber 12 filas del delito (una por mes), hay 24
  group_by(delitos, cut_comuna, año) |> 
  mutate(arreglar_n = ifelse(arreglar == TRUE, 1:n(), NA)) |>
  ungroup() |> 
  filter(arreglar == FALSE | (arreglar == TRUE & arreglar_n >= 7)) |> 
  select(-arreglar, -arreglar_n)
         
cead_limpiada_4 <- cead_limpiada_3 |> 
  mutate(fecha = lubridate::ymd(paste(año, mes, 1))) |> 
  select(fecha, cut_comuna, delito = delitos, delito_n = cifra) |>
  mutate(delito_n = as.numeric(delito_n),
         cut_comuna = as.numeric(cut_comuna)) |> 
  arrange(cut_comuna, fecha) |> 
  left_join(cargar_comunas(), join_by(cut_comuna)) |> 
  mutate(across(where(is.character), as.factor)) |> 
  print()


# cead |> count(delito)

#unir con datos anteriores (opcional) ----
# cead_nuevo <- cead
# cead_anterior <- arrow::read_parquet("app/cead_delincuencia.parquet")
# 
# 
# cead_nuevo |> 
#   # filter(fecha > "2023-01-01") |> 
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
# #unir
# cead_unido <- cead_anterior |> 
#   filter(fecha < "2023-01-01") |> 
#   bind_rows(cead_nuevo) |> 
#   arrange(fecha, comuna, delito)

cead_unido <- cead

# revisar ----

cead_unido |> filter(cut_comuna == 1101) |> 
  filter(fecha == max(fecha)) |> 
  arrange(delito) |> 
  print(n=Inf)

cead_unido |> 
  summarize(min(fecha),
            max(fecha))

cead_unido |> 
  group_by(comuna) |> 
  summarize(n = n()) |> 
  filter(n != 3024)

#revisar visualmente
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

# guardar -----
# readr::write_csv2(cead, "datos/cead_incivilidades.csv")
# arrow::write_parquet(cead, "datos/cead_delincuencia.parquet")
# cead <- arrow::read_parquet("datos/cead_delincuencia.parquet")
arrow::write_parquet(cead_unido, "app/cead_delincuencia.parquet")
