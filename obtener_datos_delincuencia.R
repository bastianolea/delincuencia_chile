library(dplyr)
library(rvest)
library(purrr)
library(RCurl)

#https://cead.spd.gov.cl/estadisticas-delictuales/

#este proceso genera requests para descargar las estadísticas de CEAD usando su API (privada), con un reposo entre request del
#doble de tiempo en que tardó en entregar los datos. Los delitos se eligen con el argumento `delito` de la función cead_generar_request()

source("funciones.R")
source("delincuencia/funciones_delincuencia.R")

#comunas a calcular
comunas_por_calcular <- cargar_comunas()$cut_comuna
años_elegidos = 2010:2023

# scraping por api ----

#por cada comuna
datos_cead <- map(comunas_por_calcular |> set_names(), \(comuna) {
  message("inciando comuna ", comuna)
  
  #por cada año especificado
  data <- map(años_elegidos |> set_names(), \(año) {
    message("año ", año)
    
    #generar request
    xml.request = cead_generar_request(año_elegido = año, 
                                       comuna, 
                                       delitos = "incivilidades y robos") #definidos en la función, se sacan del request
    
    #obtener datos
    inicio = Sys.time()
    data = tryCatch(cead_realizar_request(xml.request), 
                    error = function(e) {
                      message("error:", e)
                      return(NULL)
                    })
    final = Sys.time()
    
    Sys.sleep((final-inicio)*2)
    return(data)
  })
  return(data)
})


# guardar información cruda
readr::write_rds(datos_cead, "datos/cead_delincuencia_crudo.rds", compress = "gz")


#—----


#este proceso carga los datos descargados y los va extrayendo como tablas
#mediante un loop que va por comuna y por años. Luego, los datos son ordenados y filtrados para ser recibidos


# limpieza ----

#por cada comuna
cead_limpiada <- map_df(comunas_por_calcular, \(.comuna) {
  message("obteniendo ", .comuna)
  
  #extraer la comuna desde los datos names(cead)
  cead_comuna <- datos_cead |> 
    pluck(.comuna)
  
  #por cada año
  datos_comuna_año <- map(names(cead_comuna), \(.año) {
    message("obteniendo año ", .año)
    
    #extraer los datos desde la comuna
    cead_comuna_año <- cead_comuna |> 
      pluck(.año) |>
      cead_obtener_tabla()
    
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

#guardar
# readr::write_csv2(cead, "datos/cead_incivilidades.csv")
arrow::write_parquet(cead, "datos/cead_delincuencia.parquet")
# cead <- arrow::read_parquet("datos/cead_delincuencia.parquet")
arrow::write_parquet(cead, "app/cead_delincuencia.parquet")
