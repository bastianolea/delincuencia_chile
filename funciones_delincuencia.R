
cargar_comunas <- function() {
  # comunas <- arrow::read_feather("~/Turismo/sernatur_bigdata/resultados/sernaturbd_cut_comunas.feather")
  # readr::write_csv2(comunas, "datos/comunas_chile_cut.csv")
  readr::read_csv2("datos/comunas_chile_cut.csv", show_col_types = FALSE)
}



# obtener datos ----

# para nuevas request: entrar a #https://cead.spd.gov.cl/estadisticas-delictuales/, hacer query por varios delitos, entrar al inspector, 
# pestaña red, encontrar get_estadisticas_delictuales.php, clic derecho y copiar como cURL.


# función que genera el texto de request en base a los delitos que se necesita obtener
cead_generar_request <- function(año_elegido, comuna_numero) {
  
  # existe un ejemplo de una request en la carpeta otros
  request <- paste0("medida=1&tipoVal=1%2C2",
                    "&anio%5B%5D=", año_elegido, 
                    "&trimestre%5B%5D=4&trimestre%5B%5D=3&trimestre%5B%5D=2&trimestre%5B%5D=1",
                    "&mes%5B%5D=12&mes%5B%5D=11&mes%5B%5D=10&mes%5B%5D=9&mes%5B%5D=8&mes%5B%5D=7&mes%5B%5D=6&mes%5B%5D=5&mes%5B%5D=4&mes%5B%5D=3&mes%5B%5D=2&mes%5B%5D=1",
                    "&mes_nombres%5B%5D=Diciembre&mes_nombres%5B%5D=Noviembre&mes_nombres%5B%5D=Octubre&mes_nombres%5B%5D=Septiembre&mes_nombres%5B%5D=Agosto&mes_nombres%5B%5D=Julio&mes_nombres%5B%5D=Junio&mes_nombres%5B%5D=Mayo&mes_nombres%5B%5D=Abril&mes_nombres%5B%5D=Marzo&mes_nombres%5B%5D=Febrero&mes_nombres%5B%5D=Enero",
                    "&comuna%5B%5D=", comuna_numero,
                    "&familia%5B%5D=1&familia%5B%5D=2&familia%5B%5D=3&familia%5B%5D=4&familia%5B%5D=5&familia%5B%5D=99&familia_nombres%5B%5D=Delitos+violentos&familia_nombres%5B%5D=Delitos+asociados+a+drogas&familia_nombres%5B%5D=Delitos+asociados+a+armas&familia_nombres%5B%5D=Delitos+contra+la+propiedad+no+violentos&familia_nombres%5B%5D=Incivilidades&familia_nombres%5B%5D=Otros+delitos+o+faltas&grupo%5B%5D=101&grupo%5B%5D=102&grupo%5B%5D=103&grupo%5B%5D=104&grupo%5B%5D=105&grupo%5B%5D=106&grupo%5B%5D=107&grupo%5B%5D=108&grupo%5B%5D=109&grupo%5B%5D=110&grupo%5B%5D=201&grupo%5B%5D=301&grupo%5B%5D=302&grupo%5B%5D=401&grupo%5B%5D=402&grupo%5B%5D=403&grupo%5B%5D=404&grupo%5B%5D=405&grupo%5B%5D=501&grupo%5B%5D=502&grupo%5B%5D=503&grupo%5B%5D=504&grupo%5B%5D=999&grupo_nombres%5B%5D=Homicidios+y+femicidios&grupo_nombres%5B%5D=Violaciones+y+delitos+sexuales&grupo_nombres%5B%5D=Robos+con+violencia+o+intimidaci%C3%B3n&grupo_nombres%5B%5D=Robo+por+sorpresa&grupo_nombres%5B%5D=Lesiones+graves+o+grav%C3%ADsimas&grupo_nombres%5B%5D=Lesiones+menos+graves&grupo_nombres%5B%5D=Lesiones+leves&grupo_nombres%5B%5D=Violencia+intrafamiliar&grupo_nombres%5B%5D=Amenazas+con+armas&grupo_nombres%5B%5D=Amenazas+o+ri%C3%B1a&grupo_nombres%5B%5D=Cr%C3%ADmenes+y+simples+delitos+ley+de+drogas&grupo_nombres%5B%5D=Cr%C3%ADmenes+y+simples+delitos+ley+de+armas&grupo_nombres%5B%5D=Porte+de+arma+cortante+o+punzante&grupo_nombres%5B%5D=Robos+en+lugares+habitados+y+no+habitados&grupo_nombres%5B%5D=Robos+en+veh%C3%ADculos+y+sus+accesorios&grupo_nombres%5B%5D=Otros+robos+con+fuerza+en+las+cosas&grupo_nombres%5B%5D=Hurtos&grupo_nombres%5B%5D=Receptaci%C3%B3n&grupo_nombres%5B%5D=Consumo+de+alcohol+y+drogas+en+la+v%C3%ADa+p%C3%BAblica&grupo_nombres%5B%5D=Da%C3%B1os&grupo_nombres%5B%5D=Des%C3%B3rdenes+p%C3%BAblicos&grupo_nombres%5B%5D=Otras+incivilidades&grupo_nombres%5B%5D=Otros+delitos+o+faltas&subgrupo%5B%5D=10101&subgrupo%5B%5D=10102&subgrupo%5B%5D=10103&subgrupo%5B%5D=10104&subgrupo%5B%5D=10105&subgrupo%5B%5D=10106&subgrupo%5B%5D=10107&subgrupo%5B%5D=10201&subgrupo%5B%5D=10202&subgrupo%5B%5D=10203&subgrupo%5B%5D=10204&subgrupo%5B%5D=10205&subgrupo%5B%5D=10301&subgrupo%5B%5D=10302&subgrupo%5B%5D=10401&subgrupo%5B%5D=10501&subgrupo%5B%5D=10601&subgrupo%5B%5D=10701&subgrupo%5B%5D=10801&subgrupo%5B%5D=10802&subgrupo%5B%5D=10803&subgrupo%5B%5D=10804&subgrupo%5B%5D=10805&subgrupo%5B%5D=10901&subgrupo%5B%5D=11001&subgrupo%5B%5D=20101&subgrupo%5B%5D=20102&subgrupo%5B%5D=20103&subgrupo%5B%5D=20104&subgrupo%5B%5D=30101&subgrupo%5B%5D=30102&subgrupo%5B%5D=30103&subgrupo%5B%5D=30201&subgrupo%5B%5D=40101&subgrupo%5B%5D=40102&subgrupo%5B%5D=40201&subgrupo%5B%5D=40202&subgrupo%5B%5D=40301&subgrupo%5B%5D=40401&subgrupo%5B%5D=40501&subgrupo%5B%5D=50101&subgrupo%5B%5D=50201&subgrupo%5B%5D=50301&subgrupo%5B%5D=50401&subgrupo%5B%5D=50402&subgrupo%5B%5D=50403&subgrupo%5B%5D=50404&subgrupo%5B%5D=99901&subgrupo%5B%5D=99902&subgrupo%5B%5D=99903&subgrupo_nombres%5B%5D=Femicidio&subgrupo_nombres%5B%5D=Femicidio+no+%C3%ADntimo&subgrupo_nombres%5B%5D=Suicidio+femicida&subgrupo_nombres%5B%5D=Violaci%C3%B3n+con+homicidio&subgrupo_nombres%5B%5D=Robo+con+homicidio&subgrupo_nombres%5B%5D=Auxilio+al+suicidio&subgrupo_nombres%5B%5D=Otros+homicidios&subgrupo_nombres%5B%5D=Tortura+o+apremios+ileg%C3%ADtimos+con+violaci%C3%B3n&subgrupo_nombres%5B%5D=Robo+con+violaci%C3%B3n&subgrupo_nombres%5B%5D=Otras+violaciones&subgrupo_nombres%5B%5D=Abusos+sexuales&subgrupo_nombres%5B%5D=Otros+delitos+sexuales&subgrupo_nombres%5B%5D=Robos+con+violencia+o+intimidaci%C3%B3n&subgrupo_nombres%5B%5D=Robo+violento+de+veh%C3%ADculo+motorizado&subgrupo_nombres%5B%5D=Robo+por+sorpresa&subgrupo_nombres%5B%5D=Lesiones+graves+o+grav%C3%ADsimas&subgrupo_nombres%5B%5D=Lesiones+menos+graves&subgrupo_nombres%5B%5D=Lesiones+leves&subgrupo_nombres%5B%5D=Violencia+intrafamiliar+a+mujer&subgrupo_nombres%5B%5D=Violencia+intrafamiliar+a+ni%C3%B1o+o+ni%C3%B1a&subgrupo_nombres%5B%5D=Violencia+intrafamiliar+a+adulto+mayor&subgrupo_nombres%5B%5D=Violencia+intrafamiliar+a+hombre&subgrupo_nombres%5B%5D=Violencia+intrafamiliar+no+clasificada&subgrupo_nombres%5B%5D=Amenazas+con+armas&subgrupo_nombres%5B%5D=Amenazas+o+ri%C3%B1a&subgrupo_nombres%5B%5D=Tr%C3%A1fico+de+sustancias&subgrupo_nombres%5B%5D=Microtr%C3%A1fico+de+sustancias&subgrupo_nombres%5B%5D=Elaboraci%C3%B3n+o+producci%C3%B3n+de+sustancias&subgrupo_nombres%5B%5D=Otras+infracciones+a+la+ley+de+drogas&subgrupo_nombres%5B%5D=Disparo+injustificado&subgrupo_nombres%5B%5D=Porte+%2F+posesi%C3%B3n+de+armas+o+explosivos&subgrupo_nombres%5B%5D=Otras+infracciones+a+la+ley+de+armas&subgrupo_nombres%5B%5D=Porte+de+arma+cortante+o+punzante&subgrupo_nombres%5B%5D=Robo+en+lugar+habitado&subgrupo_nombres%5B%5D=Robos+en+lugar+no+habitado&subgrupo_nombres%5B%5D=Robo+de+veh%C3%ADculo+motorizado&subgrupo_nombres%5B%5D=Robo+de+objetos+de+o+desde+veh%C3%ADculo&subgrupo_nombres%5B%5D=Otros+robos+con+fuerza+en+las+cosas&subgrupo_nombres%5B%5D=Hurtos&subgrupo_nombres%5B%5D=Receptaci%C3%B3n&subgrupo_nombres%5B%5D=Consumo+de+alcohol+y+drogas+en+la+v%C3%ADa+p%C3%BAblica&subgrupo_nombres%5B%5D=Da%C3%B1os&subgrupo_nombres%5B%5D=Des%C3%B3rdenes+p%C3%BAblicos&subgrupo_nombres%5B%5D=Animales+sueltos+en+la+v%C3%ADa+p%C3%BAblica&subgrupo_nombres%5B%5D=Comercio+ilegal&subgrupo_nombres%5B%5D=Ofensas+al+pudor&subgrupo_nombres%5B%5D=Otras+incivilidades&subgrupo_nombres%5B%5D=Abigeato&subgrupo_nombres%5B%5D=Robo+frustrado&subgrupo_nombres%5B%5D=Otros+delitos+o+faltas&seleccion=2&descarga=false"
  )
  return(request)
}

# función que ejecuta la request o solicitud en base al texto generado con la función anterior
cead_realizar_request <- function(xml.request) {
  getURL(
    # url = "https://cead.spd.gov.cl/wp-content/themes/gobcl-wp-master/data/get_estadisticas_delictuales.php",
    url = "https://cead.minsegpublica.gob.cl/wp-content/themes/gobcl-wp-master/data/get_estadisticas_delictuales.php",
    
    postfields = xml.request,
    httpheader = c(Connection = "close", 
                   'Content-Type' = "application/x-www-form-urlencoded; charset=UTF-8", #"application/xml",
                   'Content-length' = nchar(xml.request)
    )
  )
}

# limpieza
# cead_limpiar <- function(data, .comuna, .año) {
#   paso1 <- data |> 
#     purrr::pluck(.comuna) |> 
#     rvest::read_html() |> 
#     rvest::html_table() |> 
#     purrr::pluck(1) |> 
#     janitor::row_to_names(2) |> 
#     rename(delitos = 1)
#   
#   paso2 <- paso1 |> 
#     tidyr::pivot_longer(cols = 2:length(paso1), names_to = "mes", values_to = "cifra") |> 
#     mutate(año = .año,
#            comuna = .comuna,
#            cut_comuna = isdt_obtener_numero_comuna(.comuna),
#            region = isdt_obtener_nombre_region(.comuna),
#            cut_region = isdt_obtener_numero_region(isdt_obtener_nombre_region(.comuna))
#     )
#   
#   return(paso2)
# }





cead_descargar_datos <- function(años_elegidos, comunas_por_calcular) {
  
  require(purrr)
  
  datos_cead <- map(comunas_por_calcular |> set_names(), \(comuna) {
    message("inciando comuna ", comuna)
    # comuna = 1101 #comunas_por_calcular[100]
    
    # por cada año especificado
    data <- map(años_elegidos |> set_names(), \(año) {
      # año = 2024
      message("año ", año)
      
      # generar request
      xml.request = cead_generar_request(año_elegido = año, 
                                         comuna)
      
      # obtener datos
      inicio = Sys.time()
      
      data = tryCatch(cead_realizar_request(xml.request), 
                      error = function(e) {
                        message("error:", e)
                        return(NULL)
                      })
      
      final = Sys.time()
      
      # cead_obtener_tabla(data)
      
      Sys.sleep((final-inicio) * 2) #espera para no saturar al servidor
      
      # if (nchar(data) < 30000) message("(!) tabla sin suficiente información")
      
      return(data)
    })
    return(data)
  })
  return(datos_cead)
}


# recibe el dato crudo que retorna cada request al cead, y lo transforma en una tabla tibble
# se usa dentro de cead_limpiar_resultados()
cead_obtener_tabla <- function(cead_comuna_año) {
  
  if (is.null(cead_comuna_año)) {
    warning("cead_obtener_tabla(): nulo")
    return(tibble())
  }
  # browser()
  
  data2 <- cead_comuna_año |> 
    rvest::read_html() |> 
    rvest::html_table() |> 
    purrr::pluck(1) |> 
    janitor::row_to_names(2) |> 
    rename(delitos = 1)
  
  if (nrow(data2) == 0) message("error, filas insuficientes")
  return(data2)
}


# itera sobre el objeto datos_cead, retornado por cead_descargar_datos(), que es el resultado del scraping, para convertir los datos crudos en tablas, y retornar todos los datos de comunas y años en una sola tabla 
cead_limpiar_resultados <- function(datos_cead, comunas_por_calcular) {
  
  require(purrr)
  require(furrr)
  require(future)
  plan(multisession, workers = 4)
  
  cead_limpiada <- furrr::future_map(comunas_por_calcular |> as.character(), \(.comuna) {
    message("obteniendo ", .comuna)
    # .comuna <- comunas_por_calcular[56] |> as.character()
    # .comuna = "1101"
    
    #extraer la comuna desde los datos names(cead)
    cead_comuna <- datos_cead |> 
      pluck(.comuna)
    
    #por cada año
    datos_comuna_año <- map(names(cead_comuna), \(.año) {
      message("obteniendo año ", .año)
      # .año <- "2023"
      
      # data |> cead_obtener_tabla()
      
      #extraer los datos desde la comuna
      cead_comuna_año <- cead_comuna |> 
        pluck(.año)
      
      cead_tabla <- cead_comuna_año |> 
        cead_obtener_tabla()
      
      if (nrow(cead_tabla) == 0) {
        message("sin datos para comuna ", .comuna, " en año ", .año)
        return(NULL)
      }
      
      #pivotar y agregar datos de caracterización
      cead_datos <- cead_tabla |> 
        tidyr::pivot_longer(cols = 2:length(cead_tabla), names_to = "mes", values_to = "cifra") |> 
        mutate(año = .año,
               cut_comuna = .comuna)
      
      return(cead_datos)
    })
    return(datos_comuna_año)
  })
  
  cead_limpiada_2 <- cead_limpiada |> 
    list_flatten() |> 
    list_rbind()
  
  return(cead_limpiada_2)
}