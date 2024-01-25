library(dplyr)
library(ggplot2)
library(slider)
library(lubridate)
library(stringr)
library(forcats)

#cargar datos ----
delincuencia <- arrow::read_parquet("datos/cead_delincuencia.parquet") |> 
  rename(delitos = delito_n)

periodos_presidenciales_0 <- readr::read_csv("datos/periodos_presidenciales_chile.csv", show_col_types = F) |>
  select(presidente = nombre, presidente_fecha_inicio = fecha_inicio, presidente_fecha_termino = fecha_termino)



transparencia_periodos = 0.2
transparencia_pandemia = 0.4


#filtrar delitos ----
unique(delincuencia$delito)

.comuna = "Estación Central"
.delitos <- c("Hurtos", "Robo con violencia o intimidación")
.dias_media = 14
.sumar = TRUE

datos_filtrados <- delincuencia |> 
  filter(comuna == .comuna) |> 
  filter(delito %in% .delitos)

if (.sumar == FALSE) {
  datos <- datos_filtrados |> 
    group_by(delito) |> 
    mutate(delitos_mm = slide_dbl(delitos, mean, .before = .dias_media)) |> 
    ungroup()
} else if (.sumar == TRUE) {
  datos <- datos_filtrados |> 
    group_by(fecha, comuna, region, cut_comuna, cut_region) |> 
    summarize(delitos = sum(delitos, na.rm = T), .groups = "drop") |> 
    mutate(delito = "Suma de delitos") |> 
    mutate(delitos_mm = slide_dbl(delitos, mean, .before = .dias_media))
}

#filtrar periodos presidenciales ----
periodos_presidenciales <- periodos_presidenciales_0 |> 
  mutate(presidente_fecha_termino = case_when(is.na(presidente_fecha_termino) ~ as_date(max(datos$fecha)), 
                                              .default = presidente_fecha_termino)) |> 
  filter(presidente_fecha_termino >= min(datos$fecha)) |> 
  rowwise() |> 
  mutate(fecha = mean(c(presidente_fecha_inicio, presidente_fecha_termino))) |>
  ungroup() |> 
  mutate(alterno = 1:n(),
         alterno = ifelse(alterno %% 2 == 0, "grey20", "grey40"))



#pandemia ----
fecha_inicio_pandemia = dmy("11-03-2020")

valor_inicio_pandemia = datos |> filter(fecha >= dmy("01-03-2020") & fecha < dmy("30-03-2020")) |> 
  summarize(max(delitos_mm)) |> pull()



piñera <- periodos_presidenciales |> filter(presidente == "Sebastián Piñera Echenique") |> slice_max(fecha)
boric <- periodos_presidenciales |> filter(presidente == "Gabriel Boric Font") |> slice_max(fecha)

promedio_delitos_periodo_piñera <- datos |> 
  filter(fecha >= piñera$presidente_fecha_inicio,
         fecha <= piñera$presidente_fecha_termino) |> 
  filter(fecha <= fecha_inicio_pandemia) |> 
  summarize(delitos = mean(delitos)) |> 
  pull()

promedio_delitos_periodo_boric <- datos |> 
  filter(fecha >= boric$presidente_fecha_inicio,
         fecha <= boric$presidente_fecha_termino) |> 
  summarize(delitos = mean(delitos)) |> 
  pull()


#graficar ----
datos |> 
  ggplot(aes(fecha, delitos_mm, col = delito)) +
  #fondo presidentes
  geom_rect(data = periodos_presidenciales, 
            aes(fill = alterno,
                xmin = presidente_fecha_inicio, xmax = presidente_fecha_termino,
                ymin = -Inf, ymax = max(datos$delitos_mm)*1.17),
            alpha = transparencia_periodos, inherit.aes = F, show.legend = F) +
  scale_fill_identity() +
  #texto presidentes
  geom_text(data = periodos_presidenciales |> filter(presidente_fecha_inicio >= min(datos$fecha)), 
            aes(x = fecha, y = max(datos$delitos_mm)*1.22,
                label = presidente |> str_remove("\\w+$") |> str_wrap(20)),
            size = 3, color = color_texto, alpha = 0.7, inherit.aes = F, show.legend = F) +
  #marca pandemia
  annotate(geom = "segment", y = 0, yend = max(datos$delitos_mm)*1.17, 
           x = fecha_inicio_pandemia, xend = fecha_inicio_pandemia,
           color = color_texto, alpha = transparencia_pandemia) +
  geom_hline(yintercept = valor_inicio_pandemia, linetype = "dashed", 
             color = color_texto, alpha = transparencia_pandemia) +
  annotate(geom = "text", label = "Pandemia", size = 3.5,
           x = fecha_inicio_pandemia + 20, y = valor_inicio_pandemia * 1.02,
           color = color_texto, alpha = transparencia_pandemia, 
           vjust = 0, hjust = 0) +
  #marca promedios por periodo
  # geom_hline(yintercept = promedio_delitos_periodo_piñera, color = "red") +
  annotate(geom = "segment",
           y = promedio_delitos_periodo_piñera, yend = promedio_delitos_periodo_piñera, 
           x = piñera$presidente_fecha_inicio, xend = piñera$presidente_fecha_termino,
           color = color_secundario, linewidth = 1) +
  annotate(geom = "segment",
           y = promedio_delitos_periodo_boric, yend = promedio_delitos_periodo_boric, 
           x = boric$presidente_fecha_inicio, xend = boric$presidente_fecha_termino,
           color = color_secundario, linewidth = 1) +
  annotate(geom = "segment", 
           x = boric$presidente_fecha_inicio, xend = boric$presidente_fecha_inicio,
           y = promedio_delitos_periodo_piñera, yend = promedio_delitos_periodo_boric,
           color = color_secundario, linewidth = 1, arrow = arrow(length = unit(0.02, "npc"))) +
  #línea delitos
  geom_line(linewidth = 1, lineend = "round") +
  #escalas
  scale_x_date(limits = c(min(datos$fecha), max(datos$fecha)),
               expand = c(0, 0), oob = scales::squish, 
               date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(c(0.005, 0.015))) +
  coord_cartesian(clip = "off") +
  #temas
  theme(text = element_text(colour = color_texto),
        axis.text = element_text(colour = color_texto),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.x = element_line(colour = "grey30"),
        axis.ticks.length.x = unit(-0.25, "cm"),
        panel.grid.major.y = element_line(color = "grey10")
  ) +
  theme(legend.position = "none") +
  theme(panel.background = element_blank(), 
        plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
  labs(y = paste("Cantidad de delitos en", .comuna),
       x = NULL)
