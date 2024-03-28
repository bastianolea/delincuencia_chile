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
años_elegidos = 2010:2024

# scraping por api ----
# ejecuta la obtención de datos, por comuna y por año, haciendo requests al sitio de cead

# por cada comuna
datos_cead <- map(comunas_por_calcular |> set_names(), \(comuna) {
  message("inciando comuna ", comuna)
  
  # por cada año especificado
  data <- map(años_elegidos |> set_names(), \(año) {
    message("año ", año)
    
    # generar request
    xml.request = cead_generar_request(año_elegido = año, 
                                       comuna, 
                                       delitos = "todos") #definidos en la función, se sacan del request
    
    # obtener datos
    inicio = Sys.time()
    data = tryCatch(cead_realizar_request(xml.request), 
                    error = function(e) {
                      message("error:", e)
                      return(NULL)
                    })
    final = Sys.time()
    
    Sys.sleep((final-inicio)*1.5)
    return(data)
  })
  return(data)
})


# guardar información cruda
# readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo.rds", compress = "gz")
# readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo_2023.rds", compress = "gz")
# readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo_2023_2024.rds", compress = "gz")
readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo_todos_2010_2024.rds", compress = "gz")


#—----


#este proceso carga los datos descargados y los va extrayendo como tablas
#mediante un loop que va por comuna y por años. Luego, los datos son ordenados y filtrados para ser recibidos


# limpieza ----

#por cada comuna
cead_limpiada <- map_df(comunas_por_calcular |> as.character(), \(.comuna) {
  message("obteniendo ", .comuna)
  # .comuna <- comunas_por_calcular[2] |> as.character()
  
  #extraer la comuna desde los datos names(cead)
  cead_comuna <- datos_cead |> 
    pluck(.comuna)
  
  #por cada año
  datos_comuna_año <- map(names(cead_comuna), \(.año) {
    message("obteniendo año ", .año)
    #.año <- "2024"
    
    #extraer los datos desde la comuna
    cead_comuna_año <- cead_comuna |> 
      pluck(.año) |>
      # rvest::read_html() |> 
      # rvest::html_table() |> 
      # purrr::pluck(1) |> 
      # janitor::row_to_names(2) |> 
      # rename(delitos = 1)
      cead_obtener_tabla()
    
    if (nrow(cead_comuna_año) == 0) {
      message("sin datos para comuna ", .comuna, " en año ", .año)
      return(NULL)
    }
    
    #pivotar y agregar datos de caracterización
    cead_datos <- cead_comuna_año |> 
      tidyr::pivot_longer(cols = 2:length(cead_comuna_año), names_to = "mes", values_to = "cifra") |> 
      mutate(año = .año,
             cut_comuna = .comuna)
    
    return(cead_datos)
  })
  return(datos_comuna_año)
})

# cead_limpiada <- bind_rows(cead_limpiada, cead_limpiada2)


#ordenar ----
cead <- cead_limpiada |> 
  #sacar categorías que son agrupaciones de delitos individuales
  filter(delitos != "Delitos de mayor connotación social",
         delitos != "Incivilidades") |> 
  mutate(fecha = lubridate::ymd(paste(año, mes, 1))) |> 
  select(fecha, cut_comuna, delito = delitos, delito_n = cifra) |>
  mutate(delito_n = as.numeric(delito_n),
         cut_comuna = as.numeric(cut_comuna)) |> 
  arrange(cut_comuna, fecha) |> 
  left_join(cargar_comunas(), join_by(cut_comuna)) |> 
  mutate(across(where(is.character), as.factor))


#unir con datos anteriores ----
cead_nuevo <- cead
cead_anterior <- arrow::read_parquet("app/cead_delincuencia.parquet")


cead_nuevo |> 
  # filter(fecha > "2023-01-01") |> 
  summarize(min(fecha),
            max(fecha))

cead_anterior |> 
  summarize(min(fecha),
            max(fecha))

cead_anterior |> 
  filter(fecha < "2023-01-01") |> 
  summarize(min(fecha),
            max(fecha))

#unir
cead_unido <- cead_anterior |> 
  filter(fecha < "2023-01-01") |> 
  bind_rows(cead_nuevo) |> 
  arrange(fecha, comuna, delito)

#revisar unión
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

#guardar
# readr::write_csv2(cead, "datos/cead_incivilidades.csv")
# arrow::write_parquet(cead, "datos/cead_delincuencia.parquet")
# cead <- arrow::read_parquet("datos/cead_delincuencia.parquet")
arrow::write_parquet(cead_unido, "app/cead_delincuencia.parquet")
