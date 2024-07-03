
cargar_comunas <- function() {
  # comunas <- arrow::read_feather("~/Turismo/sernatur_bigdata/resultados/sernaturbd_cut_comunas.feather")
  # readr::write_csv2(comunas, "datos/comunas_chile_cut.csv")
  readr::read_csv2("datos/comunas_chile_cut.csv")
}

# obtener datos ----

# para nuevas request: entrar a #https://cead.spd.gov.cl/estadisticas-delictuales/, hacer query por varios delitos, entrar al inspector, 
# pestaña red, encontrar get_estadisticas_delictuales.php, clic derecho y copiar como cURL.


# función que genera el texto de request en base a los delitos que se necesita obtener
cead_generar_request <- function(año_elegido, comuna_numero, delitos = "todos") {
  request_fechas = "&trimestre%5B%5D=1&trimestre%5B%5D=2&trimestre%5B%5D=3&trimestre%5B%5D=4&mes%5B%5D=1&mes%5B%5D=2&mes%5B%5D=3&mes%5B%5D=4&mes%5B%5D=5&mes%5B%5D=6&mes%5B%5D=7&mes%5B%5D=8&mes%5B%5D=9&mes%5B%5D=10&mes%5B%5D=11&mes%5B%5D=12&mes_nombres%5B%5D=Enero&mes_nombres%5B%5D=Febrero&mes_nombres%5B%5D=Marzo&mes_nombres%5B%5D=Abril&mes_nombres%5B%5D=Mayo&mes_nombres%5B%5D=Junio&mes_nombres%5B%5D=Julio&mes_nombres%5B%5D=Agosto&mes_nombres%5B%5D=Septiembre&mes_nombres%5B%5D=Octubre&mes_nombres%5B%5D=Noviembre&mes_nombres%5B%5D=Diciembre"
  
  if  (delitos == "incivilidades") {
    #para ica 9
    request_delitos = "&delitos_agrupados%5B%5D=3&delitos_agrupados%5B%5D=4&delitos_agrupados%5B%5D=7&delitos_agrupados_nombres%5B%5D=Delitos+de+mayor+connotaci%C3%B3n+social&delitos_agrupados_nombres%5B%5D=Incivilidades&delitos_agrupados_nombres%5B%5D=Robo+frustrado&delitos%5B%5D=16&delitos%5B%5D=17&delitos%5B%5D=20&delitos%5B%5D=22&delitos%5B%5D=23&delitos%5B%5D=21&delitos%5B%5D=5&delitos%5B%5D=6&delitos%5B%5D=19&delitos%5B%5D=13&delitos%5B%5D=30&delitos%5B%5D=8&delitos_nombres%5B%5D=Comercio+ambulante+o+clandestino&delitos_nombres%5B%5D=Consumo+alcohol+v%C3%ADa+p%C3%BAblica&delitos_nombres%5B%5D=Ebriedad&delitos_nombres%5B%5D=Ri%C3%B1a+p%C3%BAblica&delitos_nombres%5B%5D=Ruidos+molestos&delitos_nombres%5B%5D=Otras+incivilidades&delitos_nombres%5B%5D=Lesiones+leves&delitos_nombres%5B%5D=Lesiones+menos+graves%2C+graves+o+grav%C3%ADsimas&delitos_nombres%5B%5D=Des%C3%B3rdenes&delitos_nombres%5B%5D=Robo+por+sorpresa&delitos_nombres%5B%5D=Robo+frustrado&delitos_nombres%5B%5D=Robo+con+violencia+o+intimidaci%C3%B3n"
  
    } else if (delitos == "delitos sexuales") {
    request_delitos = "&delitos_agrupados%5B%5D=2&delitos_agrupados%5B%5D=8&delitos_agrupados_nombres%5B%5D=Abusos+sexuales+y+otros+delitos+sexuales&delitos_agrupados_nombres%5B%5D=Violencia+intrafamiliar"
  
    } else if (delitos == "robos") {
    #para ica 15
    request_delitos = "&delitos_agrupados%5B%5D=3&delitos_agrupados%5B%5D=7&delitos_agrupados_nombres%5B%5D=Delitos+de+mayor+connotaci%C3%B3n+social&delitos_agrupados_nombres%5B%5D=Robo+frustrado&delitos%5B%5D=8&delitos%5B%5D=10&delitos%5B%5D=9&delitos%5B%5D=11&delitos%5B%5D=12&delitos%5B%5D=30&delitos%5B%5D=13&delitos%5B%5D=7&delitos%5B%5D=4&delitos_nombres%5B%5D=Robo+con+violencia+o+intimidaci%C3%B3n&delitos_nombres%5B%5D=Robo+de+veh%C3%ADculo+motorizado&delitos_nombres%5B%5D=Robo+de+objetos+de+o+desde+veh%C3%ADculo&delitos_nombres%5B%5D=Robo+en+lugar+habitado&delitos_nombres%5B%5D=Robo+en+lugar+no+habitado&delitos_nombres%5B%5D=Robo+frustrado&delitos_nombres%5B%5D=Robo+por+sorpresa&delitos_nombres%5B%5D=Otros+robos+con+fuerza&delitos_nombres%5B%5D=Hurtos"
  
    } else if (delitos == "incivilidades y robos") {
    request_delitos = "&delitos_agrupados%5B%5D=3&delitos_agrupados%5B%5D=4&delitos_agrupados%5B%5D=7&delitos_agrupados_nombres%5B%5D=Delitos+de+mayor+connotaci%C3%B3n+social&delitos_agrupados_nombres%5B%5D=Incivilidades&delitos_agrupados_nombres%5B%5D=Robo+frustrado&delitos%5B%5D=16&delitos%5B%5D=17&delitos%5B%5D=20&delitos%5B%5D=22&delitos%5B%5D=23&delitos%5B%5D=21&delitos%5B%5D=5&delitos%5B%5D=6&delitos%5B%5D=19&delitos%5B%5D=13&delitos%5B%5D=30&delitos%5B%5D=8&delitos_nombres%5B%5D=Comercio+ambulante+o+clandestino&delitos_nombres%5B%5D=Consumo+alcohol+v%C3%ADa+p%C3%BAblica&delitos_nombres%5B%5D=Ebriedad&delitos_nombres%5B%5D=Ri%C3%B1a+p%C3%BAblica&delitos_nombres%5B%5D=Ruidos+molestos&delitos_nombres%5B%5D=Otras+incivilidades&delitos_nombres%5B%5D=Lesiones+leves&delitos_nombres%5B%5D=Lesiones+menos+graves%2C+graves+o+grav%C3%ADsimas&delitos_nombres%5B%5D=Des%C3%B3rdenes&delitos_nombres%5B%5D=Robo+por+sorpresa&delitos_nombres%5B%5D=Robo+frustrado&delitos_nombres%5B%5D=Robo+con+violencia+o+intimidaci%C3%B3n&delitos_agrupados%5B%5D=3&delitos_agrupados%5B%5D=7&delitos_agrupados_nombres%5B%5D=Delitos+de+mayor+connotaci%C3%B3n+social&delitos_agrupados_nombres%5B%5D=Robo+frustrado&delitos%5B%5D=8&delitos%5B%5D=10&delitos%5B%5D=9&delitos%5B%5D=11&delitos%5B%5D=12&delitos%5B%5D=30&delitos%5B%5D=13&delitos%5B%5D=7&delitos%5B%5D=4&delitos_nombres%5B%5D=Robo+con+violencia+o+intimidaci%C3%B3n&delitos_nombres%5B%5D=Robo+de+veh%C3%ADculo+motorizado&delitos_nombres%5B%5D=Robo+de+objetos+de+o+desde+veh%C3%ADculo&delitos_nombres%5B%5D=Robo+en+lugar+habitado&delitos_nombres%5B%5D=Robo+en+lugar+no+habitado&delitos_nombres%5B%5D=Robo+frustrado&delitos_nombres%5B%5D=Robo+por+sorpresa&delitos_nombres%5B%5D=Otros+robos+con+fuerza&delitos_nombres%5B%5D=Hurtos"
  
    
    } else if (delitos == "todos") {
      # query donde se seleccionaron todos los delitos disponibles en el sitio
      request_delitos = "&delitos_agrupados%5B%5D=3&delitos_agrupados%5B%5D=5&delitos_agrupados%5B%5D=4&delitos_agrupados%5B%5D=1&delitos_agrupados%5B%5D=2&delitos_agrupados%5B%5D=8&delitos_agrupados%5B%5D=6&delitos_agrupados%5B%5D=7&delitos_agrupados_nombres%5B%5D=Delitos+de+mayor+connotaci%C3%B3n+social&delitos_agrupados_nombres%5B%5D=Infracci%C3%B3n+a+ley+de+armas&delitos_agrupados_nombres%5B%5D=Incivilidades&delitos_agrupados_nombres%5B%5D=Abigeato&delitos_agrupados_nombres%5B%5D=Abusos+sexuales+y+otros+delitos+sexuales&delitos_agrupados_nombres%5B%5D=Violencia+intrafamiliar&delitos_agrupados_nombres%5B%5D=Receptaci%C3%B3n&delitos_agrupados_nombres%5B%5D=Robo+frustrado&delitos%5B%5D=35&delitos%5B%5D=34&delitos%5B%5D=33&delitos%5B%5D=32&delitos%5B%5D=31&delitos%5B%5D=14&delitos%5B%5D=28&delitos%5B%5D=23&delitos%5B%5D=13&delitos%5B%5D=30&delitos%5B%5D=12&delitos%5B%5D=11&delitos%5B%5D=10&delitos%5B%5D=9&delitos%5B%5D=8&delitos%5B%5D=22&delitos%5B%5D=29&delitos%5B%5D=27&delitos%5B%5D=7&delitos%5B%5D=26&delitos%5B%5D=21&delitos%5B%5D=6&delitos%5B%5D=5&delitos%5B%5D=4&delitos%5B%5D=3&delitos%5B%5D=25&delitos%5B%5D=20&delitos%5B%5D=19&delitos%5B%5D=18&delitos%5B%5D=17&delitos%5B%5D=16&delitos%5B%5D=15&delitos%5B%5D=2&delitos%5B%5D=1&delitos%5B%5D=24&delitos_nombres%5B%5D=Violencia+intrafamiliar+no+clasificado&delitos_nombres%5B%5D=Violencia+intrafamiliar+a+ni%C3%B1o&delitos_nombres%5B%5D=Violencia+intrafamiliar+a+mujer&delitos_nombres%5B%5D=Violencia+intrafamiliar+a+hombre&delitos_nombres%5B%5D=Violencia+intrafamiliar+a+adulto+mayor&delitos_nombres%5B%5D=Violaciones&delitos_nombres%5B%5D=Tenencia+ilegal+de+armas+o+explosivos&delitos_nombres%5B%5D=Ruidos+molestos&delitos_nombres%5B%5D=Robo+por+sorpresa&delitos_nombres%5B%5D=Robo+frustrado&delitos_nombres%5B%5D=Robo+en+lugar+no+habitado&delitos_nombres%5B%5D=Robo+en+lugar+habitado&delitos_nombres%5B%5D=Robo+de+veh%C3%ADculo+motorizado&delitos_nombres%5B%5D=Robo+de+objetos+de+o+desde+veh%C3%ADculo&delitos_nombres%5B%5D=Robo+con+violencia+o+intimidaci%C3%B3n&delitos_nombres%5B%5D=Ri%C3%B1a+p%C3%BAblica&delitos_nombres%5B%5D=Receptaci%C3%B3n&delitos_nombres%5B%5D=Porte+de+armas&delitos_nombres%5B%5D=Otros+robos+con+fuerza&delitos_nombres%5B%5D=Otros+ley+de+armas&delitos_nombres%5B%5D=Otras+incivilidades&delitos_nombres%5B%5D=Lesiones+menos+graves%2C+graves+o+grav%C3%ADsimas&delitos_nombres%5B%5D=Lesiones+leves&delitos_nombres%5B%5D=Hurtos&delitos_nombres%5B%5D=Homicidios&delitos_nombres%5B%5D=Hallazgo+de+armas+o+explosivos&delitos_nombres%5B%5D=Ebriedad&delitos_nombres%5B%5D=Des%C3%B3rdenes&delitos_nombres%5B%5D=Da%C3%B1os&delitos_nombres%5B%5D=Consumo+alcohol+v%C3%ADa+p%C3%BAblica&delitos_nombres%5B%5D=Comercio+ambulante+o+clandestino&delitos_nombres%5B%5D=Amenazas&delitos_nombres%5B%5D=Abusos+sexuales+y+otros+delitos+sexuales&delitos_nombres%5B%5D=Abigeato&delitos_nombres%5B%5D=Abandono+de+armas"
    }
  
  request <- paste0("medida=1&tipoVal=2", # tipoVal 1 es "casos policiales", 2 es "denuncias", medida es si es frecuencia o  tasa
                    "&anio%5B%5D=", año_elegido, request_fechas, 
                    request_delitos,
                    #"&region%5B%5D=", numero_region,
                    #"&provincia%5B%5D=", numero_provincia, 
                    "&comuna%5B%5D=", comuna_numero, 
                    "&seleccion=2&descarga=false"
  )
  return(request)
}

# función que ejecuta la request o solicitud en base al texto generado con la función anterior
cead_realizar_request <- function(xml.request) {
  getURL(url = "https://cead.spd.gov.cl/wp-content/themes/gobcl-wp-master/data/get_estadisticas_delictuales.php",
         postfields = xml.request,
         httpheader = c(Connection = "close", 
                        'Content-Type' = "application/x-www-form-urlencoded; charset=UTF-8", #"application/xml",
                        'Content-length' = nchar(xml.request)
         )
  )
}

# limpieza ----
cead_limpiar <- function(data, .comuna, .año) {
  paso1 <- data |> 
    purrr::pluck(.comuna) |> 
    rvest::read_html() |> 
    rvest::html_table() |> 
    purrr::pluck(1) |> 
    janitor::row_to_names(2) |> 
    rename(delitos = 1)
  
  paso2 <- paso1 |> 
    tidyr::pivot_longer(cols = 2:length(paso1), names_to = "mes", values_to = "cifra") |> 
    mutate(año = .año,
           comuna = .comuna,
           cut_comuna = isdt_obtener_numero_comuna(.comuna),
           region = isdt_obtener_nombre_region(.comuna),
           cut_region = isdt_obtener_numero_region(isdt_obtener_nombre_region(.comuna))
    )
  
  return(paso2)
}


cead_obtener_tabla <- function(data) {
  
  if (is.null(data)) {
    warning("cead_obtener_tabla(): nulo")
    return(tibble())
  }
  
  data |> 
    rvest::read_html() |> 
    rvest::html_table() |> 
    purrr::pluck(1) |> 
    janitor::row_to_names(2) |> 
    rename(delitos = 1)
}



cead_descargar_datos <- function(años_elegidos, comunas_por_calcular) {
  datos_cead <- map(comunas_por_calcular |> set_names(), \(comuna) {
    message("inciando comuna ", comuna)
    # comuna = 1101 #comunas_por_calcular[100]
    
    # por cada año especificado
    data <- map(años_elegidos |> set_names(), \(año) {
      # año = 2024
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
  return(datos_cead)
}


cead_limpiar_resultados <- function(datos_cead, comunas_por_calcular) {
  cead_limpiada <- map_df(comunas_por_calcular |> as.character(), \(.comuna) {
    message("obteniendo ", .comuna)
    # .comuna <- comunas_por_calcular[56] |> as.character()
    
    #extraer la comuna desde los datos names(cead)
    cead_comuna <- datos_cead |> 
      pluck(.comuna)
    
    #por cada año
    datos_comuna_año <- map(names(cead_comuna), \(.año) {
      message("obteniendo año ", .año)
      #.año <- "2024"
      
      # data |> cead_obtener_tabla()
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
  return(cead_limpiada)
}