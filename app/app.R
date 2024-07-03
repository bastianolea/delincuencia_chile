library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(fresh)

library(arrow)
library(dplyr)
library(ggplot2)
library(gt)
library(slider)
library(lubridate)
library(stringr)
library(forcats)
library(glue)

options(scipen = 9999)

# datos ----
# setwd("app")
delincuencia <- arrow::read_parquet("cead_delincuencia.parquet") |> 
  rename(delitos = delito_n)

# delincuencia |> 
#   summarize(delitos = sum(delitos), .by = delito) |> 
#   arrange(desc(delitos)) |> 
#   slice(1:10) |> 
#   pull(delito) |> 
#   as.character() |> 
#   dput()

datos_año_min = min(year(delincuencia$fecha))

delitos_graves <- c("Hurtos", "Robos con violencia o intimidación", "Robo en lugar habitado",
                    "Robo de vehículo motorizado",
                    "Robo de objetos de o desde vehículo",
                    "Robo por sorpresa",
                    "Robo frustrado",
                    "Homicidios",
                    "Violencia intrafamiliar a mujer")

lista_delitos <- as.character(unique(delincuencia$delito)) |> sort()

periodos_presidenciales_0 <- readr::read_csv("periodos_presidenciales_chile.csv", show_col_types = F) |> 
  select(presidente = nombre, presidente_fecha_inicio = fecha_inicio, presidente_fecha_termino = fecha_termino)

censo <- arrow::read_parquet("censo_proyecciones_año.parquet")


# colores ----
color_fondo = "#1f272b"
color_texto = "#cdf2ef"
color_secundario = "#317773"
color_enlaces = "#3d9691"
color_detalle = "#1e3534"
color_destacado = "#cf5a13"

color_positivo = "#91b423"
color_negativo = "#c03426"


css <- function(text) {
  tags$style(glue(text, .open = "{{", .close = "}}"))
}

ui <- fluidPage(title = "Estadísticas de delincuencia en Chile", 
                lang = "es",
                
                useShinyjs(),
                use_googlefont("Open Sans"), #cargar fuente o tipo de letra
                use_googlefont("Song Myung"),
                
                use_theme(create_theme(
                  theme = "default",
                  bs_vars_input(bg = color_fondo),
                  bs_vars_global(body_bg = color_fondo, 
                                 text_color = color_texto, 
                                 link_color = color_texto,
                                 border_radius_base = "6px"),
                  bs_vars_font(size_base = "14px", #aumentar globalmente tamaño de letra  
                               family_sans_serif = "Open Sans" #cargar fuente o tipo de letra
                  ),
                  bs_vars_button(
                    default_color = color_fondo,
                    default_bg = color_secundario,
                    default_border = color_fondo,
                    border_radius_base = "6px"
                  )
                )),
                
                
                #css ----
                
                css("body {
      background-color: {{color_fondo}};
  }"),
                
                css("p {
      color: {{color_texto}};
  }
  a {
    color: {{color_enlaces}};
    font-weight: bold !important;
  }
   hr {
  border-top: 3px solid {{color_detalle}} ;
  }"),
                
                css("h1, h2, h3 {
      font-family: Song Myung;
  }"),
                
                css("h1 {
      font-weight: bold;
      color: {{color_secundario}} !important;
  }"),
                
                css("h2 {
      margin-top: 38px !important;
      margin-bottom: 12px !important;
  }"),
                
                
                #estilo de barra slider
                css("
  /*fondo de barra, o sección inactiva*/
  .irs--shiny .irs-line {
  background: none;
  background-color: {{color_detalle}};
  }
  
  /*sección izquierda de barra activa*/
  .irs--shiny .irs-bar {
  background-color: {{color_secundario}};
  border: 3px solid {{color_detalle}};
  }
  
   /*pelota de slider*/
  .irs--shiny .irs-handle {
  background-color: {{color_secundario}};
  box-shadow: none;
  border: 1px solid {{color_detalle}};
  height: 20px; width: 20px;
  }
  
  /*pelota de slider hover*/
  .irs--shiny .irs-handle:hover, .irs--shiny .irs-handle:active {
  background-color: {{color_detalle}};
  }
  
  .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to {
  background-color: {{color_detalle}};
  }
  
  .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-single {
  font-size: 80%;
  margin-top: 0;
  }
  
  /*grosor de barra*/
  .irs--shiny .irs-line, .irs--shiny .irs-bar {
  top: 24px;
  height: 6px;
  border: 0;
  }
  
  /*etiqueta de barra*/
  .control-label {
  margin-bottom: 0px;
  }
  .irs--shiny .irs-grid-pol {
  display: none;
  }
  "),
                
                #estilo checkboxes
                css(".checkbox-primary input[type='checkbox']:checked+label::before, .checkbox-primary input[type='radio']:checked+label::before {
      background-color: {{color_secundario}};
      border: none;
      top: 1px;
  }
  .awesome-checkbox label::before {
     background-color: {{color_detalle}};
  border: none;
  top: 0;
  }
  .awesome-checkbox label {
  margin-top: 0;
  color: {{color_texto}};
  }"),
                
                #colores pickers
                tags$style(paste0(".btn.dropdown-toggle { /* color del picker mismo */
                   color: black;
  }
                   
         .dropdown-menu, .divider {
          color: white !important;
         background: ", color_detalle, " !important;
         }
  
         .dropdown-header {
         color: white !important;
         font-weight: bold;
         font-size: 110%;
         }
         
         .text {
         color: white;
         font-size: 80%;
         }
         
         .form-control {
         color: ", color_texto, " !important;
         box-shadow: none;
         }
         
         .no-results {
         color: black !important;
         background: ", color_detalle, " !important;
         }
         
         .selected {
         background-color: ", color_secundario, " !important;
         color: ", color_detalle, " !important;
         }
         
         .bs-placeholder, .bs-placeholder:active, bs-placeholder:focus, .bs-placeholder:hover {
         color: ", color_fondo, " !important;
         }
  
         /*color de fondo de opción elegida*/
         .dropdown-item.selected {
         background-color: ", color_destacado, " !important;
         color: black !important;
         }
         
         /*color del fondo de la opción en hover*/
         .dropdown-item:hover {
         color: red;
         background-color: ", color_secundario, " !important;
         }
  ")),
                
                #estilo radio buttons
                css(".btn-default.active, .btn-default:focus, .btn-default:active:focus {
      color: {{color_texto}} !important;
      border: none;
  }"),
                
                
                #—----
                
                #header ----
                fluidRow(
                  column(12,
                         div(style = "margin-bottom: 12px;",
                             h1("Estadísticas de delincuencia en Chile"),
                             em("Bastián Olea Herrera")
                         ),
                         
                         markdown("Este visualizador contiene gráficos que representan **estadísticas delictuales oficiales** entregadas por el [Centro de Estudio y Análisis del Delito (CEAD)](https://cead.spd.gov.cl/estadisticas-delictuales/), quienes a su vez obtienen los datos desde reportes de Carabineros y la Policía de Investigaciones de Chile al Ministerio del Interior y Seguridad Pública."), 
                         
                         markdown("Según el [CEAD](https://cead.spd.gov.cl/estadisticas-delictuales/), cada dato se compone por: _denuncias formales que la ciudadanía realiza en alguna unidad policial posterior a la ocurrencia del delito, más los delitos de los que la policía toma conocimiento al efectuar una detención en flagrancia, es decir, mientras ocurre el ilícito._"),
                         
                         markdown("El objetivo de esta plataforma es transparentar **datos objetivos sobre la delincuencia en el país,** 
             otorgándoles contexto para tratar el tema con seriedad en lugar de sensacionalismo y provecho político."
                         ),
                         
                         hr()
                  )
                ),
                
                #selectores ----
                fluidRow(
                  column(4,
                         
                         div(style = glue("margin-bottom: 12px; opacity: .8; color: {color_enlaces}"),
                             em("Seleccione una región, y opcionalmente una comuna, y luego seleccione si desea visualizar los datos a nivel regional o comunal. Por defecto se elige una comuna al azar."),
                         ),
                         
                         pickerInput("region", 
                                     label = h4("Región"),
                                     width = "100%",
                                     choices = as.character(unique(delincuencia$region)),
                                     selected = "Metropolitana de Santiago", 
                                     multiple = F
                         ),
                         
                         pickerInput("comuna",
                                     label = h4("Comuna"),
                                     width = "100%",
                                     multiple = FALSE,
                                     choices = NULL,
                                     options = list(width = FALSE, noneSelectedText = "Sin selección")
                         ),
                         actionButton("azar_comuna", "Elegir comuna al azar", style = "margin-bottom: 14px;"),
                         
                         radioGroupButtons("unidad", 
                                           h4("Unidad a visualizar:"), 
                                           choices = c("Comuna" = "comuna", "Región" = "region"), 
                                           width = "100%", justified = T, individual = F),
                         
                         div(
                           p("Los datos disponibles para esta comuna llegan hasta:", textOutput("max_fecha_datos", inline = T),
                             style = glue("margin-bottom: 12px; opacity: .8; color: {color_enlaces}")
                           )
                         ),
                         
                         div(style = "margin-top: 24px; margin-bottom: 12px;",
                             actionButton("mostrar_opciones", label = "Mostrar/ocultar opciones", 
                                          size = "xs",
                                          style = glue("margin: auto; height: 28px; 
                        font-size: 84%; padding-top: 5px; 
                        background-color: {color_detalle};
                        color: {color_enlaces};")
                             )
                         ),
                         
                         div(id = "opciones",
                             sliderTextInput(
                               inputId = "suavizar",
                               label = h4("Suavizar datos"), 
                               choices = c("21 días", "14 días", "7 días", "No") |> rev(),
                               selected = "14 días", 
                             ),
                             
                             shinyWidgets::awesomeCheckbox("sumar", label = "Sumar delitos", value = FALSE),
                             
                             shinyWidgets::awesomeCheckbox("tendencia", label = "Mostrar tendencia", value = FALSE),
                             
                             shinyWidgets::awesomeCheckbox("promedios", label = "Mostrar promedios", value = TRUE),
                             
                             shinyWidgets::awesomeCheckbox("pandemia", label = "Mostrar pandemia", value = TRUE),
                             
                             sliderInput(
                               inputId = "fecha",
                               label = h4("Rango de fechas"),
                               # min = dmy("11-03-2010"),
                               min = ymd(paste0(datos_año_min, "-01-01")), #dmy("01-01-2018"),
                               max = today(), 
                               width = "100%",
                               value = c(dmy("01-01-2019"), today()), 
                               timeFormat = "%Y"
                             )
                         ) |> hidden()
                         
                  ),
                  
                  #grafico líneas----
                  column(8,
                         h2(textOutput("titulo_grafico_lineas")),
                         p("En este gráfico se puede observar la ocurrencia mensual de delitos en la comuna o región elegidas. Puedes seleccionar los delitos en el selector que se presenta a continuación. En el fondo del gráfico se observan, como contexto, los periodos presidenciales, y el inicio y fin de la pandemia. Adicionalmente, líneas horizontales indican los promedios de delitos ocurridos durante el periodo presidencial anterior y el actual, como indicador general de la tendencia en materia de delincuencia del último tiempo."),
                         
                         pickerInput("delitos",
                                     label = h4("Delitos"),
                                     width = "100%",
                                     multiple = TRUE,
                                     choices = lista_delitos,
                                     selected = c("Hurtos", 
                                                  "Robos con violencia o intimidación", 
                                                  "Robo de vehículo motorizado",
                                                  "Robo en lugar habitado", 
                                                  "Robo por sorpresa"),
                                     options = list(maxOptions = 8, 
                                                    maxOptionsText = "Máximo 8",
                                                    noneSelectedText = "Sin selección",
                                                    width = FALSE)
                         ),
                         
                         plotOutput("grafico", width = "100%", height = 720) |> 
                           withSpinner(color = color_secundario, type = 8),
                         
                         
                  )
                ),
                
                #gráfico anuales y presidentes ----
                fluidRow(
                  column(7,
                         h2(textOutput("titulo_grafico_anual")),
                         p("Esta visualización representa la evolución de la cantidad de delitos totales ocurridos por año en la comuna o región seleccionada, indicando cambios históricos en la delincuencia. Las líneas horizontales destacan los puntos mínimos, promedio y máximos de la cantidad anual de delitos, en colores verde, naranjo y rojo, respectivamente."),
                         
                         plotOutput("grafico_anuales", height = 400) |> 
                           withSpinner(color = color_secundario, type = 8)
                  ),
                  column(5, style = "height: 400;",
                         h2(textOutput("titulo_grafico_presidentes")),
                         p("Las barras de este gráfico representan cada uno de los periodos presidenciales más recientes, y la cifra indicada corresponde a la cantidad de delitos mensuales promedio ocurridos en dicho periodo. Esto permite comparar la frecuencia con la que acontecieron delitos en cada gobierno."),
                         
                         div(style = "padding-top: 24px; padding-bottom: 48px;",
                             plotOutput("grafico_presidentes", height = 260) |> 
                               withSpinner(color = color_secundario, type = 8)
                         )
                  )
                ),
                
                #gráfico comparación años  ----
                
                fluidRow(
                  column(12,
                         h2(textOutput("titulo_grafico_comparativo")),
                         markdown("Selecciona dos años para comparar las **tasas de delitos** en la comuna seleccionada. 
             Esta medida permite analizar si los delitos aumentaron o disminuyeron entre ambas fechas. 
             Al utilizar la tasa de delitos; es decir, la cantidad de delitos reportados por cada 1.000 habitantes,
             es posible comparar delitos entre distintas fechas al considerar en el cálculo los cambios en la población de la comuna (es decir, si en una comuna los delitos se mantienen entre dos años, pero la población disminuye, entonces los delitos _bajan)_.
             Los datos de población se obtienen desde las [proyecciones de población del INE.](https://bastianoleah.shinyapps.io/censo_proyecciones/)"),
                         div(style = "opacity: 0.6; font-size: 80%;",
                             markdown("La idea de este gráfico fue originalmente concebida por [Ernesto Laval](https://x.com/elaval/status/1768858137979740248?s=20), quien la implementó en [su propio visualizador de datos.](https://observablehq.com/@elaval/comparacion-tasa-de-delitos)")
                         ),
                         
                         div(style = "margin-top: 12px;display: inline-block;",
                             pickerInput("comparativo_año_1", label = "Primer año",
                                         choices = datos_año_min:2023, selected = 2019, 
                                         multiple = F, inline = T),
                             pickerInput("comparativo_año_2", label = "Segundo año",
                                         choices = datos_año_min:2023, selected = 2023,
                                         multiple = F, inline = T)
                         ),
                         
                         div(style = "margin-top: 24px;",
                             plotOutput("grafico_comparativo", height = 600) |> 
                               withSpinner(color = color_secundario, type = 8)
                         )
                  )
                ),
                
                #gráfico delitos principales por año ----
                fluidRow(
                  column(12, style = "margin-top: 20px;",
                         h2(textOutput("titulo_delitos_principales")),
                         p("En este gráfico se representan, por cada año del que se poseen datos oficiales, los tres delitos más frecuentes en la comuna o región elegida. El color de cada barra corresponde a un delito distinto, indicado en la leyenda de abajo. Al costado derecho del gráfico se presentan las cifras y años donde cada uno de los principales delitos alcanzó su máximo."),
                         
                         div(style = "margin-top: 24px;",
                             plotOutput("grafico_principales", height = 600) |> 
                               withSpinner(color = color_secundario, type = 8)
                         )
                  )
                ),
                
                # tabla ----
                fluidRow(
                  column(12, style = "margin-top: 30px;",
                         # h2("Datos de delincuencia"),
                         h2(textOutput("titulo_tabla")),
                         p("En esta tabla se disponen todos los datos de delitos correspondientes a la comuna o región seleccionada. A continuación, selecicone un año y especifique los delitos a considerar para visualizar los datos en la tabla."),
                         
                         div(style = "max-width: 460px;",
                             sliderInput(
                               inputId = "año_tabla",
                               label = h4("Seleccione un año"),
                               min = datos_año_min,
                               max = 2023,
                               value = 2023, sep = "", width = "100%"
                             )
                         ),
                         
                         pickerInput("delitos_tabla",
                                     label = h4("Delitos"),
                                     width = "100%",
                                     multiple = TRUE,
                                     choices = lista_delitos,
                                     selected = c("Homicidios", "Amenazas o riña", "Consumo de alcohol y drogas en la vía pública", 
                                                  "Hurtos", "Daños", "Violencia intrafamiliar a mujer", "Robos con violencia o intimidación", 
                                                  "Lesiones leves", "Robo de objetos de o desde vehículo", "Robo en lugar habitado", 
                                                  "Robos en lugar no habitado"),
                                     options = list(maxOptions = 8, 
                                                    maxOptionsText = "Máximo 8",
                                                    noneSelectedText = "Sin selección",
                                                    width = FALSE)
                         ),
                         
                         br(),
                         h3(textOutput("titulo_tabla2")),
                         
                         div(style = "margin-top: -10px;",
                             gt_output("tabla") |> withSpinner(color = color_secundario, type = 8)
                         )
                  )
                ),
                
                # firma ----
                fluidRow(
                  column(12, style = "padding: 28px; font-size: 90%;",
                         hr(),
                         
                         markdown("Desarrollado en R+Shiny por [Bastián Olea Herrera.](https://bastian.olea.biz)"),
                         
                         markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
                         
                         markdown("Fuente de los datos: [Centro de Estudio y Análisis del Delito (CEAD)](https://cead.spd.gov.cl/estadisticas-delictuales/)"),
                         
                         markdown("Código de fuente de esta app y del procesamiento de los datos [disponible en GitHub.](https://github.com/bastianolea/delincuencia_chile)"),
                         
                         markdown("Los datos se obtuvieron desde CEAD haciendo uso de [técnicas de web scraping en R, detalladas en este tutorial](https://bastianolea.github.io/tutorial_r_datos_delincuencia/)")
                  )
                )
                
)

#—----
server <- function(input, output, session) {
  # browser()
  
  updatePickerInput(session, "comuna",
                    options = list(width = FALSE, noneSelectedText = "Sin selección"))
  
  #selectores ----
  
  lista_comunas <- reactive({
    delincuencia |> 
      select(region, comuna) |> 
      filter(region == input$region) |> 
      pull(comuna) |> 
      unique() |> 
      as.character() |> 
      sort()
  })
  
  observeEvent(input$region, {
    req(length(input$region) > 0)
    
    updatePickerInput(session,
                      inputId = "comuna",
                      choices = lista_comunas(),
                      selected = sample(lista_comunas(), 1),
                      options = list(width = FALSE,
                                     noneSelectedText = "Sin selección")
    )
  })
  
  #comunas al azar
  observeEvent(input$azar_comuna, {
    updatePickerInput(session,
                      inputId = "comuna",
                      selected = sample(lista_comunas(), 1),
                      options = list(width = FALSE,
                                     noneSelectedText = "Sin selección")
    )
  })
  
  
  # botones ----
  
  observeEvent(input$mostrar_opciones, toggle("opciones", anim = T))
  
  
  # datos ----
  
  ## datos comuna o región ----
  datos_comuna <- reactive({
    req(length(input$comuna) == 1)
    
    delincuencia |> 
      filter(comuna == input$comuna)
  })
  
  datos_region <- reactive({
    req(length(input$region) == 1)
    
    delincuencia |> 
      filter(region == input$region) |> 
      summarize(delitos = sum(delitos), .by = c(fecha, delito, region, cut_region))
    # browser()
  })
  
  datos_unidad <- reactive({
    if (input$unidad == "comuna") {
      datos <- datos_comuna() |> rename(unidad = comuna)
    } else if (input$unidad == "region") {
      datos <- datos_region() |> rename(unidad = region)
    }
    return(datos)
  })
  
  output$max_fecha_datos <- renderText({
    fecha <- max(datos_unidad()$fecha, na.rm = T)
    # browser()
    fecha2 <- fecha + months(1) - days(1)
    fecha_formateada <- format(fecha2, "%d/%m/%Y")
    return(fecha_formateada)
  })
  
  ## datos delito ----
  datos_filtrados <- reactive({
    req(length(input$delitos) > 0)
    req(length(input$delitos) <= 8)
    
    # if (input$unidad == "comuna") {
    #   datos_comuna() |> filter(delito %in% input$delitos) |> 
    #     rename(unidad = comuna)
    # } else if (input$unidad == "region") {
    #   datos_region() |> filter(delito %in% input$delitos) |> 
    #     rename(unidad = region)
    # }
    datos_unidad() |> filter(delito %in% input$delitos)
  })
  
  ## sumar datos ----
  datos_sumados <- reactive({
    req(datos_filtrados())
    
    if (input$sumar == FALSE) {
      datos_filtrados_2 <- datos_filtrados()
      
    } else if (input$sumar == TRUE) {
      datos_filtrados_2 <- datos_filtrados() |> 
        group_by(fecha, unidad) |> 
        summarize(delitos = sum(delitos, na.rm = T), .groups = "drop") |> 
        mutate(delito = "Suma de delitos")
    }
    return(datos_filtrados_2)
  })
  
  ## suavizar con media móvil ----
  datos_suavizados <- reactive({
    if (input$suavizar != "No") {
      .suavizar_dias <- case_when(input$suavizar == "21 días" ~ 21,
                                  input$suavizar == "14 días" ~ 14,
                                  input$suavizar == "7 días" ~ 7)
      datos_sumados_2 <- datos_sumados() |> 
        group_by(delito) |> 
        mutate(delitos_mm = slide_dbl(delitos, mean, .before = .suavizar_dias)) |> 
        ungroup()
    } else {
      datos_sumados_2 <- datos_sumados() |> 
        mutate(delitos_mm = delitos) |> 
        ungroup()
    }
    return(datos_sumados_2)
  })
  
  datos <- reactive(datos_suavizados())
  
  delitos_maximo <- reactive(max(datos()$delitos_mm))
  
  
  
  ## presidentes ----
  periodos_presidenciales <- reactive({
    periodos_presidenciales_0 |> 
      mutate(presidente_fecha_termino = case_when(is.na(presidente_fecha_termino) ~ as_date(max(datos()$fecha)), 
                                                  .default = presidente_fecha_termino)) |> 
      filter(presidente_fecha_termino >= min(datos()$fecha)) |> 
      rowwise() |> 
      mutate(fecha = mean(c(presidente_fecha_inicio, presidente_fecha_termino))) |>
      ungroup() |> 
      mutate(alterno = 1:n(),
             alterno = ifelse(alterno %% 2 == 0, color_fondo, color_secundario))
  })
  
  presidentes_fecha <- reactive({
    presidentes <- periodos_presidenciales_0 |> 
      select(presidente, fecha_inicio = presidente_fecha_inicio, fecha_termino = presidente_fecha_termino)
    
    presidentes |> 
      group_by(presidente) |> 
      mutate(id = 1:n()) |> 
      ungroup() |> 
      mutate(presidente_id = paste(str_remove(presidente, "\\w+$"), id)) |> 
      mutate(fecha = fecha_inicio) |> 
      tidyr::complete(fecha = seq(paste0(presidentes$fecha_inicio |> min() |> year(), "-01-01") |> ymd(), 
                                  paste0(delincuencia$fecha |> max() |> year(), "-12-01") |> ymd(), 
                                  by='months')) |>
      arrange(desc(fecha)) |> 
      tidyr::fill(c(presidente, presidente_id, fecha_inicio, fecha_termino), .direction = "downup") |> 
      select(-id)
  })
  
  ## pandemia ----
  fecha_inicio_pandemia = reactive(dmy("11-03-2020"))
  
  fecha_fin_cuarentena = reactive(dmy("29-06-2021"))
  
  valor_inicio_pandemia = reactive(datos() |> filter(fecha >= dmy("01-03-2020") & fecha < dmy("30-03-2020")) |> 
                                     summarize(max(delitos_mm)) |> pull())
  
  
  ## promedios por periodo ----
  piñera <- reactive(periodos_presidenciales() |> filter(presidente == "Sebastián Piñera Echenique") |> slice_max(fecha))
  boric <- reactive(periodos_presidenciales() |> filter(presidente == "Gabriel Boric Font") |> slice_max(fecha))
  
  promedio_delitos_periodo_piñera <- reactive({
    datos() |> 
      filter(fecha >= piñera()$presidente_fecha_inicio,
             fecha <= piñera()$presidente_fecha_termino) |>
      #solo datos desde antes de la pandemia
      filter(fecha <= fecha_inicio_pandemia()) |> 
      #solo datos desde antes de la pandemia o de después del fin de la última cuarentena
      # filter(fecha <= fecha_inicio_pandemia() | fecha >= fecha_fin_cuarentena()) |> 
      summarize(delitos = mean(delitos), .groups = "drop") |> 
      pull()
  })
  
  promedio_delitos_periodo_boric <- reactive({
    datos() |> 
      filter(fecha >= boric()$presidente_fecha_inicio,
             fecha <= boric()$presidente_fecha_termino) |> 
      summarize(delitos = mean(delitos)) |> 
      pull()
  })
  
  # textos ----
  
  texto_unidad <- reactive({
    if (input$unidad == "comuna") {
      input$comuna
    } else if (input$unidad == "region") {
      input$region
    }
  })
  
  output$comuna3 <- output$comuna2 <- output$comuna1 <- output$comuna <- renderText({
    req(input$comuna)
    input$comuna
  })
  
  output$region <- renderText({
    req(input$region)
    input$region
  })
  
  
  output$titulo_grafico_lineas <- renderText({
    if (input$unidad == "comuna") {
      paste("Ocurrencia de delitos en la comuna de", input$comuna)
    } else if (input$unidad == "region") {
      if (input$region == "Metropolitana de Santiago") {
        paste("Ocurrencia de delitos en la región", input$region)
      } else {
        paste("Ocurrencia de delitos en", input$region)
      }
    }
  })
  
  output$titulo_tabla2 <- output$titulo_tabla <- renderText({
    if (input$unidad == "comuna") {
      paste("Estadísticas de delitos denunciados en la comuna de", input$comuna)
    } else if (input$unidad == "region") {
      if (input$region == "Metropolitana de Santiago") {
        paste("Estadísticas de delitos denunciados en la región", input$region)
      } else {
        paste("Estadísticas de delitos denunciados en", input$region)
      }
    }
  })
  
  
  texto_unidad_redactado <- reactive({
    req(length(input$comuna) > 0)
    req(length(input$region) > 0)
    
    if (input$unidad == "comuna") {
      paste("la comuna de", input$comuna)
      
    } else if (input$unidad == "region") {
      if (input$region == "Metropolitana de Santiago") {
        paste("la región", input$region)
        
      } else if (input$region %in% c("Maule", "Libertador Gral. Bernardo O'Higgins")) {
        paste("la región del", input$region)
        
      } else {
        paste("la región de", input$region)
      }
    }
  })
  
  
  output$titulo_grafico_anual <- renderText({
    paste("Delitos totales al año en", texto_unidad_redactado())
  })
  
  output$titulo_grafico_presidentes <- renderText({
    paste("Promedio de delitos mensuales en", texto_unidad_redactado(), "por periodo de gobierno")
  })
  
  output$titulo_delitos_principales <- renderText({
    paste("Delitos principales por año en", texto_unidad_redactado())
  })
  
  
  output$titulo_grafico_comparativo <- renderText({
    
    paste("Comparación de delitos en años", input$comparativo_año_1, "y", input$comparativo_año_2, "en", texto_unidad_redactado())
  })
  
  # gráficos ----
  ## gráfico líneas ----
  output$grafico <- renderPlot({
    req(length(input$comuna) == 1)
    req(datos())
    message("grafico...")
    
    transparencia_periodos = 0.1
    transparencia_pandemia = 0.2
    
    # browser()
    p <- datos() |> 
      mutate(delito = str_wrap(delito, 20)) |> 
      ggplot(aes(fecha, delitos_mm, col = delito)) +
      #fondo presidentes
      geom_rect(data = periodos_presidenciales(), 
                aes(fill = alterno,
                    xmin = presidente_fecha_inicio, xmax = presidente_fecha_termino,
                    ymin = -Inf, ymax = delitos_maximo()*1.17),
                alpha = transparencia_periodos, inherit.aes = F, show.legend = F) +
      scale_fill_identity() +
      #texto presidentes
      geom_text(data = periodos_presidenciales() |> filter(presidente_fecha_inicio >= min(datos()$fecha)), 
                aes(x = fecha, y = delitos_maximo()*1.22,
                    label = presidente |> str_remove("\\w+$") |> str_wrap(20)),
                size = 3.5, color = color_texto, alpha = 0.7, inherit.aes = F, show.legend = F)
    
    if (input$pandemia) {
      p <- p +
        #marca pandemia
        annotate(geom = "segment", y = 0, yend = delitos_maximo()*1.17, 
                 x = fecha_inicio_pandemia(), xend = fecha_inicio_pandemia(),
                 color = color_texto, alpha = transparencia_pandemia) +
        annotate(geom = "segment", y = 0, yend = delitos_maximo()*1.17, 
                 x = fecha_fin_cuarentena(), xend = fecha_fin_cuarentena(),
                 color = color_texto, alpha = transparencia_pandemia) +
        geom_hline(yintercept = valor_inicio_pandemia(), linetype = "dashed", 
                   color = color_texto, alpha = transparencia_pandemia) +
        annotate(geom = "text", label = "Pandemia", size = 3.5,
                 x = fecha_inicio_pandemia() + 20, y = valor_inicio_pandemia() * 1.02,
                 color = color_texto, alpha = transparencia_pandemia, 
                 vjust = 0, hjust = 0)
    }
    
    if (input$tendencia == TRUE) {
      #tendencia
      p <- p + 
        stat_smooth(method = "lm",
                    linewidth = 2.5, col = color_fondo, linetype = "solid",
                    se = FALSE, fullrange = T, show.legend = F) + #sombra
        stat_smooth(method = "lm",
                    linewidth = 1.5, col = color_secundario, linetype = "dashed",
                    se = FALSE, fullrange = T, show.legend = F)
    }
    
    p <- p +
      #línea delitos
      geom_line(linewidth = 1, lineend = "round", alpha = 0.8)
    
    
    if (input$promedios) {
      
      color_piñera = ifelse(promedio_delitos_periodo_boric() > promedio_delitos_periodo_piñera(),
                            color_positivo, color_negativo)
      color_boric = ifelse(promedio_delitos_periodo_boric() < promedio_delitos_periodo_piñera(),
                           color_positivo, color_negativo)
      
      #marca promedios por periodo
      p <- p +
        #línea piñera
        annotate(geom = "segment",
                 y = promedio_delitos_periodo_piñera(), yend = promedio_delitos_periodo_piñera(), 
                 x = piñera()$presidente_fecha_inicio, xend = piñera()$presidente_fecha_termino,
                 color = color_fondo, linewidth = 2.5) + #sombra
        annotate(geom = "segment",
                 y = promedio_delitos_periodo_piñera(), yend = promedio_delitos_periodo_piñera(), 
                 x = piñera()$presidente_fecha_inicio, xend = piñera()$presidente_fecha_termino,
                 color = color_piñera, linewidth = 1) +
        #línea boric
        annotate(geom = "segment",
                 y = promedio_delitos_periodo_boric(), yend = promedio_delitos_periodo_boric(), 
                 x = boric()$presidente_fecha_inicio, xend = boric()$presidente_fecha_termino,
                 color = color_fondo, linewidth = 2.5) + #sombra
        annotate(geom = "segment",
                 y = promedio_delitos_periodo_boric(), yend = promedio_delitos_periodo_boric(), 
                 x = boric()$presidente_fecha_inicio, xend = boric()$presidente_fecha_termino,
                 color = color_boric, linewidth = 1) +
        #flecha
        annotate(geom = "segment", 
                 x = boric()$presidente_fecha_inicio, xend = boric()$presidente_fecha_inicio,
                 y = promedio_delitos_periodo_piñera(), yend = promedio_delitos_periodo_boric(),
                 color = color_fondo,
                 linewidth = 2.5, lineend= "round", arrow = arrow(length = unit(0.02, "npc"))) + #sombra
        annotate(geom = "segment", 
                 x = boric()$presidente_fecha_inicio, xend = boric()$presidente_fecha_inicio,
                 y = promedio_delitos_periodo_piñera(), yend = promedio_delitos_periodo_boric(),
                 color = color_boric,
                 linewidth = 1, lineend= "round", arrow = arrow(length = unit(0.02, "npc")))
    }
    
    
    
    p <- p +
      #escalas
      scale_x_date(limits = c(min(datos()$fecha), max(datos()$fecha)),
                   expand = c(0, 0), oob = scales::squish, 
                   date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(expand = expansion(c(0, 0.015)), labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
      scale_color_brewer(palette = "Spectral", type = "qual", direction = -1) +
      coord_cartesian(clip = "on", xlim = c(input$fecha[1], input$fecha[2])) +
      guides(color = guide_legend(position = "bottom", nrow = 2)) +
      #temas
      theme(text = element_text(color = color_texto),
            rect = element_rect(fill = color_fondo),
            axis.text = element_text(colour = color_texto),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks = element_blank(),
            axis.ticks.x = element_line(colour = color_detalle),
            axis.ticks.length.x = unit(-0.25, "cm"),
            plot.title = element_text(face = "bold"),
            panel.grid.major.y = element_line(color = color_detalle),
            axis.title = element_text(color = color_secundario),
            axis.text.x = element_text(angle = -90, vjust = 0.5, margin = margin(t = 5))) +
      theme(legend.title = element_blank(), 
            legend.key = element_rect(fill = color_fondo, linewidth = 0),
            legend.text = element_text(color = color_texto, size = 10, margin = margin(l= 3, r = 6))) +
      theme(panel.background = element_blank(), 
            plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
      labs(y = paste("Cantidad de delitos en", texto_unidad_redactado()),
           title = paste("Cantidad de delitos denunciados en", texto_unidad_redactado()),
           x = NULL)
    
    plot(p)
  }, res = 95)
  
  
  ## gráfico barras delitos anuales ----
  output$grafico_anuales <- renderPlot({
    req(length(input$comuna) == 1)
    
    #delitos por año
    datos <- datos_unidad() |> 
      mutate(año = year(fecha)) |> 
      group_by(año) |> 
      summarize(delitos = sum(delitos), .groups = "drop")
    
    datos |> 
      ggplot(aes(as.factor(año), delitos)) +
      geom_hline(yintercept = mean(datos$delitos), linetype = "solid", color = color_destacado, linewidth = 1, alpha = .8) +
      geom_hline(yintercept = min(datos$delitos), linetype = "solid", color = color_positivo, linewidth = 1, alpha = .8) +
      geom_hline(yintercept = max(datos$delitos), linetype = "solid", color = color_negativo, linewidth = 1, alpha = .8) +
      geom_col(fill = color_secundario, width = 0.5) +
      geom_text(aes(label = format(round(delitos, 0), big.mark = ".", decimal.mark = ","),
                    y = delitos * 0.99),
                hjust = 0, angle = -90, color = color_texto, fontface = "bold") +
      scale_y_continuous(expand = expansion(c(0.01, 0.03)), labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
      #temas
      theme(text = element_text(color = color_texto),
            rect = element_rect(fill = color_fondo),
            axis.text = element_text(colour = color_texto),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_line(color = color_detalle),
            plot.title = element_text(face = "bold", margin = margin(t = 6, b = 6)),
            plot.title.position = "plot",
            axis.title = element_text(color = color_secundario),
            axis.title.y = element_text(margin = margin(r = 8)),
            axis.title.x = element_text(margin = margin(t = 8)),
            axis.text.x = element_text()) +
      theme(panel.background = element_rect(fill = color_fondo), 
            plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
      labs(y = paste("Cantidad de delitos anuales"),
           title = paste("Delitos anuales denunciados \nen", texto_unidad_redactado()),
           x = paste("Delitos totales anuales en", texto_unidad_redactado())
      )
  }, res = 90)
  
  
  ## gráfico barras delitos principales ---- 
  output$grafico_principales <- renderPlot({
    req(length(input$comuna) == 1)
    
    # browser()
    
    datos <- datos_unidad() |> 
      mutate(año = year(fecha)) |> 
      filter(año >= year(input$fecha[1])) |> 
      group_by(año, delito) |>
      summarize(delitos = sum(delitos), .groups = "drop") |> 
      arrange(desc(año), desc(delitos)) |> 
      group_by(año) |> 
      slice_max(delitos, n = 3) |> 
      group_by(año) |> 
      mutate(delito = delito |> str_wrap(16) |> as.factor() |> fct_reorder2(año, delitos))
    
    maximos <- datos |> 
      group_by(delito) |> 
      slice_max(delitos)
    
    datos |> 
      ggplot(aes(delito, delitos, 
                 fill = delito, color = delito)) +
      #lineas de maximos
      geom_hline(data = maximos |> select(-año),
                 aes(yintercept = delitos, color = delito),
                 linetype = "solid", linewidth = 0.8, alpha = 0.4) +
      geom_col(position = position_dodge2(), width = 0.5) +
      ggrepel::geom_text_repel(data = maximos |> rename(año_max = año) |> mutate(año = max(datos$año)),
                               aes(label = glue(" {año_max}: {format(round(delitos, 0), big.mark='.', decimal.mark = ',')}"),
                                   x = 4), hjust = 0, vjust = 0.5, size = 3,
                               direction = "y", xlim = c(4.5, Inf)
      ) +
      geom_text(data = datos |> filter(año == max(datos$año)),
                aes(label = format(round(delitos, 0), big.mark='.', decimal.mark = ","),
                    y = delitos*1.04),
                hjust = 1, vjust = 0.5, angle = -90, size = 3
      ) +
      facet_wrap(~año, nrow = 1, scales = "free_x", strip.position = "bottom") +
      scale_y_continuous(expand = expansion(c(0.02, 0.08)), labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
      scale_fill_brewer(palette = "Spectral", type = "qual", direction = -1) +
      scale_color_brewer(palette = "Spectral", type = "qual", direction = -1) +
      coord_cartesian(clip = "off") +
      #temas
      theme(text = element_text(color = color_texto),
            rect = element_rect(fill = color_fondo),
            axis.text = element_text(colour = color_texto),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(face = "bold", margin = margin(b=8)),
            panel.grid.major.y = element_line(color = color_detalle),
            axis.title = element_text(color = color_secundario),
            axis.text.x = element_blank())+
      theme(strip.background = element_rect(fill = color_secundario),
            strip.text = element_text(color = color_texto)) +
      theme(plot.margin = unit(c(0, 25, 0, 0), "mm")) +
      theme(legend.position = "bottom",
            legend.title = element_blank(), 
            legend.key = element_rect(fill = color_fondo),
            legend.text = element_text(color = color_texto, size = 10, margin = margin(l = 3, r=8, t = 4, b = 4))) +
      theme(panel.background = element_rect(fill = color_detalle), 
            plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
      labs(y = NULL, #paste("Delitos anuales en", texto_unidad_redactado()),
           x = paste("Delitos principales", texto_unidad_redactado(), "en cada año"),
           title = paste("Delitos principales en", texto_unidad_redactado()),
      ) +
      guides (fill = guide_legend(nrow = 2, keywidth = unit(1, "mm")))
  }, res = 95)
  
  
  ## gráfico mensuales gobierno ----
  output$grafico_presidentes <- renderPlot({
    req(length(input$comuna) == 1)
    
    #delitos diarios promedio durante el periodo de cada presidente
    datos <- datos_unidad() |> 
      #poner presidentes de cada fecha
      left_join(presidentes_fecha(), join_by(fecha)) |> 
      mutate(presidente_id = presidente_id |> str_remove_all (" 1") |> 
               str_trim() |> 
               fct_reorder(fecha)) |>
      mutate(presidente_id = presidente_id |> fct_rev()) |> 
      #meses
      mutate(año = year(fecha),
             mes = month(fecha)) |> 
      summarize(delitos = sum(delitos), .by = c(año, mes, presidente_id)) |> #delitos totales mensuales
      summarize(delitos = mean(delitos), .by = c(presidente_id)) #promedio de delitos mensuales
    
    # browser()
    
    datos |> 
      ggplot(aes(y = presidente_id, 
                 x = delitos)) +
      geom_vline(xintercept = max(datos$delitos), linetype = "solid", color = color_negativo, linewidth = 1, alpha = .8) +
      geom_vline(xintercept = min(datos$delitos), linetype = "solid", color = color_positivo, linewidth = 1, alpha = .8) +
      geom_col(fill = color_secundario, width = 0.5) +
      geom_text(aes(label = format(round(delitos, 0), big.mark = ".", decimal.mark = ","),
                    x = delitos * 0.98),
                hjust = 1, color = color_texto, fontface = "bold") +
      scale_x_continuous(expand = expansion(c(0, 0.05))) +
      scale_y_discrete(expand = expansion(0.1)) +
      #temas
      theme(text = element_text(color = color_texto),
            rect = element_rect(fill = color_fondo),
            axis.text = element_text(colour = color_texto),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.y = element_line(color = color_detalle),
            axis.title = element_text(color = color_secundario),
            axis.title.y = element_text(margin = margin(r = 8)),
            axis.text.x = element_blank()) +
      theme(panel.background = element_rect(fill = color_fondo), 
            plot.title = element_text(face = "bold", margin = margin(t = 0, b = 10)),
            plot.title.position = "plot",
            plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
      labs(y = paste("Periodo presidencial"),
           x = "Promedio de delitos mensuales por periodo presidencial",
           title = paste("Delitos mensuales promedio denunciados \nsegún periodo de gobierno, en", texto_unidad_redactado()))
  }, res = 95)
  
  
  ## gráfico comparativo tasas ----
  # censo <- reactive(arrow::read_parquet("censo_proyecciones_año.parquet"))
  
  datos_comparativo <- reactive({
    req(datos_unidad())
    # browser()
    
    datos <- datos_unidad() |> 
      mutate(año = year(fecha)) |> 
      filter(año %in% c(input$comparativo_año_1, input$comparativo_año_2)) |> 
      filter(delito %in% delitos_graves)
    
    # agregar datos del censo
    if (input$unidad == "comuna") {
      datos <- datos |> 
        left_join(censo, by = join_by(cut_comuna, año))
      
    } else if (input$unidad == "region") {
      # browser()
      
      censo_region <- censo |> 
        summarize(población = sum(población), .by = c(cut_region, region, año))
      
      datos <- datos |> 
        left_join(censo_region, by = join_by(cut_region, año))
    }
    
    datos |> 
      group_by(delito, año) |> 
      summarize(delitos = sum(delitos), 
                poblacion = first(población)) |> 
      ungroup() |> 
      mutate(tasa = (delitos / poblacion) * 1000)
  })
  
  
  output$grafico_comparativo <- renderPlot({
    req(length(input$comuna) == 1)
    req(input$comparativo_año_1,
        input$comparativo_año_2)
    
    datos <- datos_comparativo() |> 
      group_by(delito) |> 
      mutate(delito = str_wrap(delito, 15)) |> 
      # mutate(diferencia = 1-min(delitos)/max(delitos)) |> 
      mutate(diferencia = abs(tasa-lag(tasa))) |> 
      tidyr::fill(diferencia, .direction = "up") |> 
      mutate(delitos = tasa) |> ###
      mutate(relacion = if_else(delitos == min(delitos), "menor", "mayor")) |> 
      mutate(delitos_etiqueta = format(round(delitos, 1), big.mark = ".", decimal.mark = ",", trim = T))
    # browser()
    
    datos_wide <- datos_comparativo() |> 
      mutate(delito = str_wrap(delito, 15)) |> 
      mutate(delitos = round(tasa, digits = 2)) |>
      # mutate(delitos = round(tasa, digits = 1)) |>
      # mutate(delitos = floor(tasa*10)/10) |> 
      tidyr::pivot_wider(id_cols = delito, names_from = año, values_from = delitos) |>
      rename(año_inicial = 2, año_final = 3) |> 
      mutate(cambio = case_when(año_final > año_inicial ~ "aumenta", 
                                año_final < año_inicial ~ "disminuye",
                                año_final == año_inicial ~ "mantiene"))
    
    # browser()
    # dev.new()
    
    datos |> 
      ggplot(aes(x = delitos, y = delito, color = as.factor(año))) +
      geom_segment(data = datos_wide, 
                   aes(y = delito, x = año_inicial, xend = año_final), inherit.aes = F,
                   color = color_texto, alpha = 0.4, linewidth = 1) +
      geom_point(aes(size = relacion)) +
      geom_text(data = datos |> filter(delitos == min(delitos)),
                aes(label = paste(delitos_etiqueta, "  ")), 
                hjust = 1, size = 3.5, show.legend = F) +
      geom_text(data = datos |> filter(delitos == max(delitos)),
                aes(label = paste("   ", delitos_etiqueta)), 
                hjust = 0, size = 3.5, show.legend = F) +
      #años
      geom_text(data = datos |> filter(diferencia > 0.4), 
                aes(label = año, x = delitos), hjust = 0.5, vjust = 0, 
                nudge_y = 0.25, size = 2.5, show.legend = F, alpha = .7) +
      #años juntos 
      geom_text(data = datos |> filter(diferencia <= 0.4) |> 
                  arrange(delitos) |> 
                  mutate(año = paste(año, collapse = "/"),
                         delitos = mean(delitos)), check_overlap = TRUE, show.legend = F,
                aes(label = año, x = delitos), hjust = 0.5, vjust = 0,
                nudge_y = 0.25, size = 2.5, color = color_enlaces, alpha = .7) +
      #flechitas
      geom_point(data = datos_wide |> filter(cambio == "aumenta"),
                 aes(x = -0.9, y = delito), 
                 inherit.aes = F, shape = 17, color = color_negativo, size = 3) +
      geom_point(data = datos_wide |> filter(cambio == "disminuye"),
                 aes(x = -0.9, y = delito), 
                 inherit.aes = F, shape = 25, color = color_positivo, fill = color_positivo, size = 3) +
      #escalas
      coord_cartesian(clip = "off",
                      # xlim = c(min(datos$delitos), max(datos$delitos))) +
                      xlim = c(0, max(datos$delitos))
      ) +
      scale_x_continuous(expand = expansion(add = c(0.4, 0.8))) +
      # scale_size_continuous(range = c(1, 10)) +
      scale_y_discrete(expand = expansion(c(0.05, 0.05))) +
      scale_size_manual(values = c(6, 4)) +
      scale_color_manual(breaks = c(input$comparativo_año_1, input$comparativo_año_2),
                         values = c(#color_secundario, 
                           color_enlaces,
                           color_destacado
                         )) +
      coord_cartesian(clip = "off") +
      theme(legend.position = "top",
            legend.background = element_rect(fill = "transparent"),
            axis.text.x = element_text(margin = margin(t = -4, b = 4)),
            axis.text.y = element_text(size = 10),
            legend.box.margin = margin(t = -4, b = -14),
            panel.grid.major.y = element_blank()) +
      guides(size = guide_none(), 
             color = guide_legend(override.aes = list(size = 5))) +
      labs(color = "Años a comparar", y = NULL, 
           title = paste("Tasa de delitos (delitos por cada mil habitantes) en", texto_unidad_redactado()),
           x = paste("Tasa de delitos por cada 1.000 habitantes en", input$comparativo_año_1, "y", input$comparativo_año_2)
      ) +
      #temas
      theme(text = element_text(color = color_texto),
            rect = element_rect(fill = color_fondo),
            axis.text = element_text(colour = color_texto),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(face = "bold"),
            panel.grid.major.y = element_line(color = color_detalle),
            axis.title = element_text(color = color_secundario),
            axis.title.y = element_text(margin = margin(r = 8))) +
      theme(panel.background = element_rect(fill = color_fondo), 
            plot.background = element_rect(fill = color_fondo, linewidth = 0))
    
  }, res = 95)
  
  
  # tablas ----
  datos_tabla <- reactive({
    req(length(input$comuna) == 1)
    req(datos())
    message("tabla...")
    # browser()
    
    datos_filt <- datos_unidad() |> 
      select(fecha, unidad, delito, delitos) |> 
      arrange(fecha, desc(delitos)) |> 
      filter(year(fecha) == input$año_tabla) |>
      filter(delito %in% input$delitos_tabla)
    
    # datos_filt |> print(n=Inf)
    # 
    # datos_filt |> 
    #   group_by(fecha) |> 
    #   count(delito) |> 
    #   arrange(desc(n))
    # 
    # datos_filt |> 
    #   filter(fecha == "2023-12-01")
    # 
    # datos_filt |> 
    #   mutate(mes = month(fecha)) |> 
    #   filter(mes == 4)
    #   summarize(delitos = sum(delitos), .by = c(unidad, delito))
    
    datos_tabla <- datos_filt |> 
      tidyr::pivot_wider(names_from = delito, values_from = delitos) |> 
      mutate(mes = month(fecha),
             mes = recode(mes, 
                          "1" = "enero", "2" = "febrero", "3" = "marzo",
                          "4" = "abril", "5" = "mayo", "6" = "junio",
                          "7" = "julio", "8" = "agosto", "9" = "septiembre",
                          "10" = "octubre", "11" = "noviembre", "12" = "diciembre")) |> 
      select(-fecha, -unidad) |> 
      relocate(mes, .before = 1) 
    # browser()
    return(datos_tabla)
  })
  
  output$tabla <- render_gt({
    datos_tabla() |> 
      gt() |> 
      tab_style(style = cell_text(size = px(13)), 
                locations = cells_body()) |> 
      tab_style(style = cell_text(weight = "bold", size = px(13)),
                locations = cells_column_labels(everything())) |> 
      cols_align(align = "right", columns = mes) |>
      data_color(columns = where(is.numeric), direction = "column",
                 fn = scales::col_numeric(palette = c(color_fondo, color_secundario), #color_destacado), 
                                          domain = NULL)
                 
      ) |> 
      # palette = color_detalle2, na_color = color_texto) |>
      # fmt_number(columns = monto, sep_mark = ".", decimals = 0) |> 
      tab_style(style = cell_borders(color = color_fondo, weight = px(2), style = "solid"),
                locations = cells_body()
      ) |> 
      cols_label(mes = "Mes") |> 
      tab_options(table.font.color = color_texto, table.font.color.light = color_texto, 
                  table_body.hlines.color = color_detalle,
                  table_body.vlines.color = color_detalle,
                  column_labels.border.top.color = color_fondo, 
                  column_labels.border.bottom.color = color_detalle, 
                  table_body.border.bottom.color = color_detalle,
                  table.background.color = color_fondo,
                  row_group.font.weight = "bold", 
                  row_group.border.bottom.color = color_fondo, 
                  row_group.border.top.width = px(20), row_group.border.top.color = color_fondo, 
                  table.font.names = "Open Sans")
  })
  
}


shinyApp(ui = ui, server = server)
