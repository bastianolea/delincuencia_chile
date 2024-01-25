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

delincuencia <- arrow::read_parquet("cead_delincuencia.parquet") |> 
  rename(delitos = delito_n)

periodos_presidenciales_0 <- readr::read_csv("periodos_presidenciales_chile.csv", show_col_types = F) |> 
  select(presidente = nombre, presidente_fecha_inicio = fecha_inicio, presidente_fecha_termino = fecha_termino)


color_fondo = "#1f272b"
color_texto = "white"
color_secundario = "#317773"
color_detalle = "#1e3534"
color_destacado = "#FF6600"

transparencia_periodos = 0.05
transparencia_pandemia = 0.2

.dias_media = 14

css <- function(text) {
  tags$style(glue(text, .open = "{{", .close = "}}"))
}

ui <- fluidPage(
  use_googlefont("Open Sans"), #cargar fuente o tipo de letra
  use_googlefont("Song Myung"),
  
  use_theme(create_theme(
    theme = "default",
    bs_vars_input(bg = color_fondo),
    bs_vars_global(body_bg = color_fondo, 
                   text_color = color_texto, 
                   link_color = color_texto),
    bs_vars_font(size_base = "12px", #aumentar globalmente tamaño de letra  
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
  }"),
  
  css("h1, h2, h3 {
      font-family: Song Myung;
  }"),
  
  css("h1 {
      font-weight: bold;
      color: {{color_secundario}} !important;
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
  
  #header ----
  fluidRow(
    column(12,
           h1("Visualizador de delincuencia en Chile")
    )
  ),
  
  #selectores ----
  fluidRow(
    column(4,
           pickerInput("region", 
                       label = h4("Región"),
                       choices = as.character(unique(delincuencia$region)),
                       selected = "Metropolitana de Santiago", 
                       multiple = F
           ),
           
           pickerInput("comuna",
                       label = h4("Comuna"),
                       width = "100%",
                       multiple = FALSE,
                       choices = NULL,
                       options = list(width = FALSE)
           ),
           actionButton("azar_comuna", "Elegir comuna al azar", style = "margin-bottom: 14px;"),
           
           pickerInput("delitos",
                       label = h4("Delitos"),
                       width = "100%",
                       multiple = TRUE,
                       choices = as.character(unique(delincuencia$delito)),
                       selected = c("Hurtos", "Robo con violencia o intimidación", "Robo en lugar habitado"),
                       options = list(maxOptions = 8, 
                                      maxOptionsText = "Máximo 8",
                                      noneSelectedText = "Sin selección",
                                      width = FALSE)
           ),
           
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
           
    ),
    
    #grafico ----
    column(7,
           plotOutput("grafico", width = "100%", height = "600px"),
           
           sliderInput(
             inputId = "fecha",
             label = h4("Rango de fechas"),
             min = dmy("11-03-2010"),
             max = today(), width = "100%",
             value = c(dmy("11-03-2010"), today()), timeFormat = "%Y"
           ),
    )
  )
)

#—----
server <- function(input, output, session) {
  # browser()
  
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
  
  
  #datos ----
  datos_filtrados <- reactive({
    req(length(input$delitos) > 0)
    req(length(input$delitos) <= 8)
    req(length(input$comuna) == 1)
    
    message("datos filtrados")
    delincuencia |> 
      filter(comuna == input$comuna) |> 
      filter(delito %in% input$delitos)
  })
  
  #sumar datos ----
  datos_1 <- reactive({
    req(datos_filtrados())
    
    message("datos")
    
    if (input$sumar == FALSE) {
      datos_1 <- datos_filtrados()
      
    } else if (input$sumar == TRUE) {
      datos_1 <- datos_filtrados() |> 
        group_by(fecha, comuna, region, cut_comuna, cut_region) |> 
        summarize(delitos = sum(delitos, na.rm = T), .groups = "drop") |> 
        mutate(delito = "Suma de delitos")
    }
    
    return(datos_1)
  })
  
  #suavizar con media móvil ----
  datos <- reactive({
    if (input$suavizar != "No") {
      .suavizar_dias <- case_when(input$suavizar == "21 días" ~ 21,
                                  input$suavizar == "14 días" ~ 14,
                                  input$suavizar == "7 días" ~ 7)
      datos_2 <- datos_1() |> 
        group_by(delito) |> 
        mutate(delitos_mm = slide_dbl(delitos, mean, .before = .suavizar_dias)) |> 
        ungroup()
    } else {
      datos_2 <- datos_1() |> 
        mutate(delitos_mm = delitos) |> 
        ungroup()
    }
    return(datos_2)
  })
  
  delitos_maximo <- reactive(max(datos()$delitos_mm))
  
  #filtrar periodos presidenciales ----
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
  
  
  # pandemia ----
  fecha_inicio_pandemia = reactive(dmy("11-03-2020"))
  
  valor_inicio_pandemia = reactive(datos() |> filter(fecha >= dmy("01-03-2020") & fecha < dmy("30-03-2020")) |> 
                                     summarize(max(delitos_mm)) |> pull())
  
  
  #promedios por periodo ----
  piñera <- reactive(periodos_presidenciales() |> filter(presidente == "Sebastián Piñera Echenique") |> slice_max(fecha))
  boric <- reactive(periodos_presidenciales() |> filter(presidente == "Gabriel Boric Font") |> slice_max(fecha))
  
  promedio_delitos_periodo_piñera <- reactive({
    datos() |> 
      filter(fecha >= piñera()$presidente_fecha_inicio,
             fecha <= piñera()$presidente_fecha_termino) |> 
      filter(fecha <= fecha_inicio_pandemia()) |> 
      summarize(delitos = mean(delitos)) |> 
      pull()
  })
  
  promedio_delitos_periodo_boric <- reactive({
    datos() |> 
      filter(fecha >= boric()$presidente_fecha_inicio,
             fecha <= boric()$presidente_fecha_termino) |> 
      summarize(delitos = mean(delitos)) |> 
      pull()
  })
  
  
  
  
  #grafico ----
  output$grafico <- renderPlot({
    req(datos())
    message("grafico...")
    
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
                size = 3, color = color_texto, alpha = 0.7, inherit.aes = F, show.legend = F)
    
    if (input$pandemia) {
      p <- p +
        #marca pandemia
        annotate(geom = "segment", y = 0, yend = delitos_maximo()*1.17, 
                 x = fecha_inicio_pandemia(), xend = fecha_inicio_pandemia(),
                 color = color_texto, alpha = transparencia_pandemia) +
        geom_hline(yintercept = valor_inicio_pandemia(), linetype = "dashed", 
                   color = color_texto, alpha = transparencia_pandemia) +
        annotate(geom = "text", label = "Pandemia", size = 3.5,
                 x = fecha_inicio_pandemia() + 20, y = valor_inicio_pandemia() * 1.02,
                 color = color_texto, alpha = transparencia_pandemia, 
                 vjust = 0, hjust = 0)
    }
    p <- p +
      #línea delitos
      geom_line(linewidth = 1, lineend = "round")
    
    if (input$promedios) {
      #marca promedios por periodo
      p <- p +
        # geom_hline(yintercept = promedio_delitos_periodo_piñera, color = "red") +
        annotate(geom = "segment",
                 y = promedio_delitos_periodo_piñera(), yend = promedio_delitos_periodo_piñera(), 
                 x = piñera()$presidente_fecha_inicio, xend = piñera()$presidente_fecha_termino,
                 color = color_secundario, linewidth = 1) +
        annotate(geom = "segment",
                 y = promedio_delitos_periodo_boric(), yend = promedio_delitos_periodo_boric(), 
                 x = boric()$presidente_fecha_inicio, xend = boric()$presidente_fecha_termino,
                 color = color_secundario, linewidth = 1) +
        annotate(geom = "segment", 
                 x = boric()$presidente_fecha_inicio, xend = boric()$presidente_fecha_inicio,
                 y = promedio_delitos_periodo_piñera(), yend = promedio_delitos_periodo_boric(),
                 color = color_secundario, linewidth = 1, arrow = arrow(length = unit(0.02, "npc")))
    }
    
    p <- p +
      #escalas
      scale_x_date(limits = c(min(datos()$fecha), max(datos()$fecha)),
                   expand = c(0, 0), oob = scales::squish, 
                   date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(expand = expansion(c(0, 0.015))) +
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
    
    if (input$tendencia == TRUE) {
      #tendencia
      p <- p + 
        stat_smooth(method = "lm",
                    linewidth = 1.5, col = color_secundario, linetype = "dashed",
                    se = FALSE, fullrange = T, show.legend = F)
    }
    
    plot(p)
  }, res = 90)
  
}


shinyApp(ui = ui, server = server)
