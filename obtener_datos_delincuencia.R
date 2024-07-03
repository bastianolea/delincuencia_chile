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

# años_elegidos = 2010:2023
# años_elegidos = 2010:2024
años_elegidos = 2024

# scraping por api ----
# ejecuta la obtención de datos, por comuna y por año, haciendo requests al sitio de cead

# por cada comuna
datos_cead <- cead_descargar_datos(años_elegidos, comunas_por_calcular)

# guardar información cruda
# readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo.rds", compress = "gz")
# readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo_2023.rds", compress = "gz")
# readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo_2023_2024.rds", compress = "gz")
# readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo_todos_2010_2024.rds", compress = "gz")
readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo_todos_2024_3.rds", compress = "gz")
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
cead <- cead_limpiada |> 
  #sacar categorías que son agrupaciones de delitos individuales
  filter(delitos != "Delitos de mayor connotación social",
         delitos != "Incivilidades",
         delitos != "Violencia intrafamiliar",
         # !delitos %in% c("Violencia intrafamiliar a adulto mayor", "Violencia intrafamiliar a hombre", "Violencia intrafamiliar a mujer", "Violencia intrafamiliar a niño", "Violencia intrafamiliar no clasificado"),
         ) |> 
  mutate(fecha = lubridate::ymd(paste(año, mes, 1))) |> 
  select(fecha, cut_comuna, delito = delitos, delito_n = cifra) |>
  mutate(delito_n = as.numeric(delito_n),
         cut_comuna = as.numeric(cut_comuna)) |> 
  arrange(cut_comuna, fecha) |> 
  left_join(cargar_comunas(), join_by(cut_comuna)) |> 
  mutate(across(where(is.character), as.factor))


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
