# delincuencia ----

#para nuevas request: entrar a #https://cead.spd.gov.cl/estadisticas-delictuales/, hacer query por varios delitos, entrar al inspector, pestaña red, encontrar get_estadisticas_delictuales.php, clic derecho y copiar 

# fetch("https://cead.spd.gov.cl/wp-content/themes/gobcl-wp-master/data/get_estadisticas_delictuales.php", {
#   "body": "medida=1&tipoVal=1%2C2&anio%5B%5D=2022&anio%5B%5D=2023&delitos_agrupados%5B%5D=3&delitos_agrupados%5B%5D=7&delitos_agrupados_nombres%5B%5D=Delitos+de+mayor+connotaci%C3%B3n+social&delitos_agrupados_nombres%5B%5D=Robo+frustrado&delitos%5B%5D=8&delitos%5B%5D=10&delitos%5B%5D=9&delitos%5B%5D=11&delitos%5B%5D=12&delitos%5B%5D=30&delitos%5B%5D=13&delitos%5B%5D=7&delitos%5B%5D=4&delitos_nombres%5B%5D=Robo+con+violencia+o+intimidaci%C3%B3n&delitos_nombres%5B%5D=Robo+de+veh%C3%ADculo+motorizado&delitos_nombres%5B%5D=Robo+de+objetos+de+o+desde+veh%C3%ADculo&delitos_nombres%5B%5D=Robo+en+lugar+habitado&delitos_nombres%5B%5D=Robo+en+lugar+no+habitado&delitos_nombres%5B%5D=Robo+frustrado&delitos_nombres%5B%5D=Robo+por+sorpresa&delitos_nombres%5B%5D=Otros+robos+con+fuerza&delitos_nombres%5B%5D=Hurtos&region%5B%5D=5&region%5B%5D=13&region%5B%5D=8&region_nombres%5B%5D=Regi%C3%B3n+de+Valpara%C3%ADso&region_nombres%5B%5D=Regi%C3%B3n+Metropolitana&region_nombres%5B%5D=Regi%C3%B3n+del+Biob%C3%ADo&provincia%5B%5D=56&provincia%5B%5D=135&provincia%5B%5D=83&provincias_nombres%5B%5D=Provincia+de+San+Antonio&provincias_nombres%5B%5D=Provincia+de+Melipilla&provincias_nombres%5B%5D=Provincia+de+Biob%C3%ADo&comuna%5B%5D=5602&comuna%5B%5D=13502&comuna%5B%5D=8314&comuna_nombres%5B%5D=Algarrobo&comuna_nombres%5B%5D=Alhu%C3%A9&comuna_nombres%5B%5D=Alto+Biob%C3%ADo&seleccion=2&descarga=false",
#   "cache": "default",
#   "credentials": "include",
#   "headers": {
#     "Accept": "*/*",
#     "Accept-Language": "es-419,es;q=0.9",
#     "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
#     "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.5.1 Safari/605.1.15",
#     "X-Requested-With": "XMLHttpRequest"
#   },
#   "method": "POST",
#   "mode": "cors",
#   "redirect": "follow",
#   "referrer": "https://cead.spd.gov.cl/estadisticas-delictuales/",
#   "referrerPolicy": "strict-origin-when-cross-origin"
# })

cargar_comunas <- function() {
  # comunas <- arrow::read_feather("~/Turismo/sernatur_bigdata/resultados/sernaturbd_cut_comunas.feather")
  # readr::write_csv2(comunas, "datos/comunas_chile_cut.csv")
  readr::read_csv2("datos/comunas_chile_cut.csv")
}


cead_generar_request <- function(año_elegido, comuna_numero, delitos = "protocolo") {
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
  }
  
  request <- paste0("medida=1&tipoVal=2",
                    "&anio%5B%5D=", año_elegido, request_fechas, 
                    request_delitos,
                    #"&region%5B%5D=", numero_region,
                    #"&provincia%5B%5D=", numero_provincia, 
                    "&comuna%5B%5D=", comuna_numero, 
                    "&seleccion=2&descarga=false"
  )
  return(request)
}

cead_realizar_request <- function(xml.request) {
  getURL(url = "https://cead.spd.gov.cl/wp-content/themes/gobcl-wp-master/data/get_estadisticas_delictuales.php",
         postfields = xml.request,
         httpheader = c(Connection = "close", 
                        'Content-Type' = "application/x-www-form-urlencoded; charset=UTF-8", #"application/xml",
                        'Content-length' = nchar(xml.request)
         )
  )
}

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
  data |> 
    rvest::read_html() |> 
    rvest::html_table() |> 
    purrr::pluck(1) |> 
    janitor::row_to_names(2) |> 
    rename(delitos = 1)
}

