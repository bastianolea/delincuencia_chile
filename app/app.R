library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(slider)
library(lubridate)
library(stringr)
library(forcats)
library(glue)
library(fresh)
library(shinycssloaders)
library(shinyjs)

delincuencia <- arrow::read_parquet("cead_delincuencia.parquet") |> 
  rename(delitos = delito_n)

periodos_presidenciales_0 <- readr::read_csv("periodos_presidenciales_chile.csv", show_col_types = F) |> 
  select(presidente = nombre, presidente_fecha_inicio = fecha_inicio, presidente_fecha_termino = fecha_termino)


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

ui <- fluidPage(
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
         font-family: Aleo;
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
  
  
  #—----
  
  #header ----
  fluidRow(
    column(12,
           div(style = "margin-bottom: 12px;",
               h1("Visualizador de delincuencia en Chile"),
               em("Bastián Olea Herrera")
           ),
           
           p("En este visualizador se presentan gráficos con datos estadísticas delictuales entregadas por el", 
             tags$a("Centro de Estudio y Análisis del Delito (CEAD),", 
                    href = "https://cead.spd.gov.cl/estadisticas-delictuales/",
                    target = "_blank"),
             "quienes a su vez obtienen los datos desde reportes de Carabineros y la Policía de Investigaciones de Chile al Ministerio del Interior y Seguridad Pública."), 
           
           p("Según el",
             tags$a("CEAD,",
                    href = "https://cead.spd.gov.cl/estadisticas-delictuales/",
                    target = "_blank"),
             "cada dato se compone por: ",
             em("denuncias formales que la ciudadanía realiza en alguna unidad policial posterior a la ocurrencia del delito, más los delitos de los que la policía toma conocimiento al efectuar una detención en flagrancia, es decir, mientras ocurre el ilícito.")
           ),
           
           p("El objetivo de esta plataforma es transparentar datos objetivos de la delincuencia en el país, 
             otorgándoles contexto para tratar el tema con seriedad en lugar de sensacionalismo y provecho político."
           ),
           
           hr()
    )
  ),
  
  #selectores ----
  fluidRow(
    column(4,
           pickerInput("region", 
                       label = h4("Región:"),
                       width = "100%",
                       choices = as.character(unique(delincuencia$region)),
                       selected = "Metropolitana de Santiago", 
                       multiple = F
           ),
           
           pickerInput("comuna",
                       label = h4("Comuna:"),
                       width = "100%",
                       multiple = FALSE,
                       choices = NULL,
                       options = list(width = FALSE, noneSelectedText = "Sin selección")
           ),
           actionButton("azar_comuna", "Elegir comuna al azar", style = "margin-bottom: 14px;"),
           
           
           pickerInput("delitos",
                       label = h4("Delitos:"),
                       width = "100%",
                       multiple = TRUE,
                       choices = as.character(unique(delincuencia$delito)),
                       selected = c("Hurtos", "Robo con violencia o intimidación", "Robo en lugar habitado"),
                       options = list(maxOptions = 8, 
                                      maxOptionsText = "Máximo 8",
                                      noneSelectedText = "Sin selección",
                                      width = FALSE)
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
             min = dmy("11-03-2010"),
             max = today(), width = "100%",
             # value = c(dmy("11-03-2010"), today()), 
             value = c(dmy("01-01-2017"), today()), 
             timeFormat = "%Y"
           )
           ) |> hidden()
           
    ),
    
    #grafico líneas----
    column(8,
           plotOutput("grafico", width = "100%", height = 720) |> 
             withSpinner(color = color_secundario, type = 8),
           
           
    )
  ),
  
  #gráfico anuales y presidentes ----
  fluidRow(
    column(7,
           h2("Delitos anuales en", textOutput("comuna1", inline = T)),
           plotOutput("grafico_anuales", height = 400) |> 
             withSpinner(color = color_secundario, type = 8)
    ),
    column(5, style = "height: 400;",
           h2("Promedio de delitos mensuales en", textOutput("comuna3", inline = T), "por periodo presidencial"),
           div(style = "padding-top: 48px; padding-bottom: 48px;",
               
               plotOutput("grafico_presidentes_dias", height = 260) |> 
                 withSpinner(color = color_secundario, type = 8)
           )
    )
  ),
  
  #gráfico delitos principales por año ----
  fluidRow(
    column(12,
           h2("Delitos principales por año en", textOutput("comuna2", inline = T)),
           plotOutput("grafico_principales", height = 600) |> 
             withSpinner(color = color_secundario, type = 8)
    )
  ),
  
  # firma ----
  fluidRow(
    column(12, style = "padding: 28px; font-size: 90%;",
           hr(),
           p("Desarrollado por",
             tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
           
           markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
           
           markdown("Fuente de los datos: [Centro de Estudio y Análisis del Delito (CEAD)](https://cead.spd.gov.cl/estadisticas-delictuales/)"),
           
           p(
             "Código de fuente de esta app y del procesamiento de los datos",
             tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/delincuencia_chile")
           ),
           p("Los datos se obtuvieron desde CEAD haciendo uso de",
             tags$a("técnicas de web scraping en R, detalladas en este tutorial.",
                    href = "https://bastianolea.github.io/tutorial_r_datos_delincuencia/",
                    target = "_blank")
           )
           
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
      filter(region == input$region)
  })
  
  ## datos delito ----
  datos_filtrados <- reactive({
    req(length(input$delitos) > 0)
    req(length(input$delitos) <= 8)
    
    datos_comuna() |> 
      filter(delito %in% input$delitos)
  })
  
  ## sumar datos ----
  datos_sumados <- reactive({
    req(datos_filtrados())
    
    if (input$sumar == FALSE) {
      datos_filtrados_2 <- datos_filtrados()
      
    } else if (input$sumar == TRUE) {
      datos_filtrados_2 <- datos_filtrados() |> 
        group_by(fecha, comuna, region, cut_comuna, cut_region) |> 
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
    # browser()
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
  output$comuna3 <- output$comuna2 <- output$comuna1 <- output$comuna <- renderText({
    req(input$comuna)
    input$comuna
  })
  
  output$region <- renderText({
    req(input$region)
    input$region
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
      #temas
      theme(text = element_text(color = color_texto),
            rect = element_rect(fill = color_fondo),
            axis.text = element_text(colour = color_texto),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks = element_blank(),
            axis.ticks.x = element_line(colour = color_detalle),
            axis.ticks.length.x = unit(-0.25, "cm"),
            panel.grid.major.y = element_line(color = color_detalle),
            axis.title = element_text(color = color_secundario),
            axis.text.x = element_text(angle = -90, vjust = 0.5, margin = margin(t = 5))) +
      theme(legend.position = "bottom",
            legend.title = element_blank(), 
            legend.key = element_rect(fill = color_fondo),
            legend.text = element_text(color = color_texto, size = 10, margin = margin(r = 6))) +
      theme(panel.background = element_blank(), 
            plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
      labs(y = paste("Cantidad de delitos en", input$comuna),
           x = NULL)
    
    
    
    plot(p)
  }, res = 95)
  
  
  ## gráfico barras delitos anuales ----
  output$grafico_anuales <- renderPlot({
    req(length(input$comuna) == 1)
    
    #delitos por año
    datos <- datos_comuna() |> 
      mutate(año = year(fecha)) |> 
      group_by(comuna, año) |> 
      summarize(delitos = sum(delitos), .groups = "drop")
    
    datos |> 
      ggplot(aes(as.factor(año), delitos)) +
      geom_hline(yintercept = mean(datos$delitos), linetype = "dashed", color = color_destacado, linewidth = 1) +
      geom_col(fill = color_secundario, width = 0.5) +
      geom_text(aes(label = format(delitos, big.mark =".", decimal.mark = ","),
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
            axis.title = element_text(color = color_secundario),
            axis.title.y = element_text(margin = margin(r = 8)),
            axis.title.x = element_text(margin = margin(t = 8)),
            axis.text.x = element_text()) +
      theme(panel.background = element_rect(fill = color_fondo), 
            plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
      labs(y = paste("Cantidad de delitos anuales"),
           x = paste("Delitos totales anuales en la comuna de", input$comuna))
  }, res = 90)
  
  ## gráfico barras delitos principales ---- 
  output$grafico_principales <- renderPlot({
    req(length(input$comuna) == 1)
    
    datos <- datos_comuna() |> 
      mutate(año = year(fecha)) |> 
      filter(año >= year(input$fecha[1])) |> 
      group_by(comuna, año, delito) |>
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
                 linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
      geom_col(position = position_dodge2(), width = 0.5) +
      ggrepel::geom_text_repel(data = maximos |> rename(año_max = año) |> mutate(año = max(datos$año)),
                               aes(label = glue(" {año_max}: {format(delitos, big.mark='.', decimal.mark = ',')}"),
                                   x = 4), hjust = 0, vjust = 0.5, size = 3,
                               direction = "y", xlim = c(4.5, Inf)
      ) +
      geom_text(data = datos |> filter(año == max(datos$año)),
                aes(label = format(delitos, big.mark='.', decimal.mark = ","),
                    y = delitos*1.04),
                hjust = 1, vjust = 0.5, angle = -90, size = 3
      ) +
      facet_wrap(~año, nrow = 1, scales = "free_x", strip.position = "bottom") +
      scale_y_continuous(expand = expansion(c(0.02, 0.05)), labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
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
            panel.grid.major.y = element_line(color = color_detalle),
            axis.title = element_text(color = color_secundario),
            axis.text.x = element_blank())+
      theme(strip.background = element_rect(fill = color_secundario),
            strip.text = element_text(color = color_texto)) +
      theme(plot.margin = unit(c(0, 25, 0, 0), "mm")) +
      theme(legend.position = "bottom",
            legend.title = element_blank(), 
            legend.key = element_rect(fill = color_fondo),
            legend.text = element_text(color = color_texto, size = 10, margin = margin(r=8, t = 4, b = 4))) +
      theme(panel.background = element_rect(fill = color_detalle), 
            plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
      labs(y = paste("Delitos anuales en", input$comuna),
           x = paste("Delitos principales de la comuna de ", input$comuna, "en cada año")
      ) +
      guides (fill = guide_legend(nrow = 2, keywidth = unit(1, "mm")))
  }, res = 95)
  
  
  ## gráfico mensuales promedio presidente ----
  output$grafico_presidentes_dias <- renderPlot({
    req(length(input$comuna) == 1)
    
    #delitos diarios promedio durante el periodo de cada presidente
    datos <- datos_comuna() |> 
      #poner presidentes de cada fecha
      left_join(presidentes_fecha(), join_by(fecha)) |> 
      mutate(presidente_id = presidente_id |> str_remove_all (" 1") |> 
               fct_reorder(fecha)) |>
      mutate(presidente_id = presidente_id |> fct_rev()) |> 
      #meses
      mutate(año = year(fecha),
             mes = month(fecha)) |> 
      summarize(delitos = sum(delitos), .by = c(comuna, año, mes, presidente_id)) |> #delitos totales mensuales
      summarize(delitos = mean(delitos), .by = c(comuna, presidente_id)) #promedio de delitos mensuales
    
    datos |> 
      ggplot(aes(y = presidente_id, 
                 x = delitos)) +
      geom_col(fill = color_secundario, width = 0.5) +
      geom_text(aes(label = format(round(delitos, 0), big.mark = ".", decimal.mark = ","),
                    x = delitos * 0.98),
                hjust = 1, color = color_texto, fontface = "bold") +
      geom_vline(xintercept = max(datos$delitos), linetype = "dashed", color = color_negativo, linewidth = 1) +
      geom_vline(xintercept = min(datos$delitos), linetype = "dashed", color = color_positivo, linewidth = 1) +
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
            plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
      labs(y = paste("Periodo presidencial"),
           x = "Promedio de delitos mensuales por periodo presidencial")
  }, res = 95)
}


shinyApp(ui = ui, server = server)
