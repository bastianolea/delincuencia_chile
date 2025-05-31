library(dplyr)
library(readr)
library(rvest)
library(purrr)
library(RCurl)

source("funciones_delincuencia.R")

#https://cead.spd.gov.cl/estadisticas-delictuales/

# este proceso genera requests para descargar las estadísticas de CEAD usando su API (privada), con un reposo entre request del
# doble de tiempo en que tardó en entregar los datos. Los delitos se eligen con el argumento `delito` de la función cead_generar_request()

# comunas a calcular
comunas_por_calcular <- cargar_comunas()$cut_comuna
# comunas_por_calcular = 1101

# años_elegidos = 2010:2023
# años_elegidos = 2010:2024
# años_elegidos = 2018:2024
años_elegidos = 2024
# también se puede usar para descargar un año nuevo de la base, en cuyo caso, en el procesamiento hay que activar
# el paso que agrega incrementalmente el año nuevo a la base existente

# scraping por api ----
# ejecuta la obtención de datos, por comuna y por año, haciendo requests al sitio de cead

# por cada comuna
datos_cead <- cead_descargar_datos(años_elegidos, comunas_por_calcular)
# map(list_flatten(datos_cead), cead_obtener_tabla) #para probar con una sola comuna

# guardar ----
# guardar información cruda, que viene como texto html
readr::write_rds(datos_cead, "datos/cead_crudo_casospoliciales_2024_3.rds", compress = "gz") # actualización de datos 2025 (31 de mayo 2025, datos nuevos hasta diciembre)
# readr::write_rds(datos_cead, "datos/cead_crudo_casospoliciales_2024_2.rds", compress = "gz") # actualización de datos 2024 (17 de diciembre 2024, datos nuevos hasta septiembre)
# readr::write_rds(datos_cead, "datos/cead_crudo_casospoliciales_2018_2024.rds", compress = "gz")
# datos_cead <- readr::read_rds("datos/cead_crudo_casospoliciales_2014_2024.rds")

# luego el html se transforma en tablas en procesar_datos_delincuencia.R