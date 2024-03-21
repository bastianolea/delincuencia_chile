library(dplyr)
library(ggplot2)
library(slider)
library(lubridate)
library(stringr)
library(forcats)
library(tidyr)
library(colorspace)

# datos ----
delincuencia <- arrow::read_parquet("app/cead_delincuencia.parquet") |> 
  rename(delitos = delito_n)

presidentes <- readr::read_csv("app/periodos_presidenciales_chile.csv", show_col_types = F) |>
  select(presidente = nombre, fecha_inicio, fecha_termino)

censo <- arrow::read_parquet("app/censo_proyecciones_año.parquet")

# colores ----
color_fondo = "#1f272b"
color_texto = "#cdf2ef"
color_secundario = "#317773"
color_enlaces = "#3d9691"
color_detalle = "#1e3534"
color_destacado = "#cf5a13"

color_positivo = "#91b423"
color_negativo = "#c03426"

color_texto = "black"
color_fondo = "white"
color_detalle = "#1e3534" |> lighten(.95) |> desaturate(.6)


# opciones ----
# .comuna = "La Florida"
# .comuna = "Santiago"
# .comuna = "Puente Alto"
# .comuna = "Ñuñoa"
# .comuna = "Estación Central"
.comuna = "Providencia"
año1 = 2019
año2 = 2023

#tasa comparativa ----
as.character(unique(delincuencia$delito))

delitos_graves <- c("Hurtos", 
                    "Lesiones menos graves, graves o gravísimas",
                    "Robo con violencia o intimidación", "Robo en lugar habitado",
                    "Robo de vehículo motorizado",
                    "Robo de objetos de o desde vehículo",
                    "Robo por sorpresa"
                    #"Robo frustrado"
                    )

datos2 <- delincuencia |> 
  filter(comuna == .comuna) |> 
  mutate(año = year(fecha)) |> 
  left_join(censo, by = join_by(cut_comuna, año)) |> 
  group_by(delito, año) |> 
  summarize(delitos = sum(delitos), 
            poblacion = first(población)) |> 
  ungroup() |> 
  mutate(tasa = (delitos / poblacion) * 1000)


datos <- datos2 |> 
  filter(año %in% c(año1, año2)) |> 
  filter(delito %in% delitos_graves) |> 
  group_by(delito) |> 
  mutate(delitos = tasa) |> ###
  mutate(relacion = if_else(delitos == min(delitos), "menor", "mayor")) |> 
  mutate(delitos_etiqueta = format(round(delitos, 1), big.mark = ".", decimal.mark = ",", trim = T)) |> 
  mutate(diferencia = 1-min(delitos)/max(delitos))

datos_wide <- datos |> 
  mutate(delitos = tasa) |> ###
  pivot_wider(id_cols = delito, names_from = año, values_from = delitos) |>
  rename(año_inicial = 2, año_final = 3) |> 
  mutate(cambio = case_when(año_final > año_inicial ~ "aumenta", 
                            año_final < año_inicial ~ "disminuye",
                            año_final == año_inicial ~ "sin cambio")) |> 
  mutate(año_max = max(año_inicial, año_final)) |> 
  mutate(diferencia = 1-año_inicial/año_final) |> 
  mutate(cambio = if_else(abs(diferencia) < 0.05, "sin cambio", cambio))

# graficar ----
posicion_flechas = max(datos_wide$año_max)/11

datos |> 
  ggplot(aes(x = delitos, y = delito, color = as.factor(año))) +
  geom_segment(data = datos_wide, 
               aes(y = delito, x = año_inicial, xend = año_final), inherit.aes = F) +
  geom_point(aes(size = relacion)) +
  geom_text(data = datos |> filter(delitos == min(delitos)),
            aes(label = paste(delitos_etiqueta, " ")), 
            hjust = 1, show.legend = F) +
  geom_text(data = datos |> filter(delitos == max(delitos)),
            aes(label = paste("  ", delitos_etiqueta)), 
            hjust = 0, show.legend = F) +
  #años
  geom_text(data = datos |> filter(diferencia > 0.45), 
            aes(label = año, x = delitos), hjust = 0.5, vjust = 0, 
            nudge_y = 0.2, size = 3, show.legend = F, alpha = .9) +
  #años juntos 
  geom_text(data = datos |> filter(diferencia <= 0.45) |> 
              arrange(delitos) |> 
              mutate(año = paste(año, collapse = "/"),
                     delitos = mean(delitos)), check_overlap = TRUE, show.legend = F,
            aes(label = año, x = delitos), hjust = 0.5, vjust = 0,
            nudge_y = 0.2, size = 3, color = color_enlaces, alpha = .9) +
  #flechitas
  geom_point(data = datos_wide |> filter(cambio == "disminuye"),
             aes(x = año_max+posicion_flechas, y = delito, shape = cambio), 
             inherit.aes = F, color = color_positivo, fill = color_positivo, size = 4) +
  geom_point(data = datos_wide |> filter(cambio == "aumenta" & diferencia > umbral_delitos_mantienen),
             aes(x = año_max+posicion_flechas, y = delito, shape = cambio), 
             inherit.aes = F, color = color_negativo, size = 4) +
  geom_point(data = datos_wide |> filter(abs(diferencia) <= umbral_delitos_mantienen & abs(diferencia) >= 0),
             aes(x = año_max+posicion_flechas, y = delito, shape = cambio), 
             inherit.aes = F, color = color_secundario, size = 4.5, alpha = .5) +
  #escalas
  coord_cartesian(clip = "off") +
  scale_shape_manual(values = c("disminuye" = 25, "aumenta" = 17, "sin cambio" = 15)) +
  scale_x_continuous(expand = expansion(0.1, 0.1)) +
  scale_y_discrete(expand = expansion(c(0.06, 0.07)), labels = ~str_wrap(.x, 25)) +
  scale_size_manual(values = c(6, 4)) +
  scale_color_manual(breaks = c(año1, año2), values = c(color_enlaces, color_destacado)) +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank()) +
  guides(size = guide_none(), 
         color = guide_legend(override.aes = list(size = 5), order = 1),
         shape = guide_legend(override.aes = list(color = color_secundario, fill = color_secundario))
         ) +
  labs(color = "Años a comparar", y = NULL, 
       x = paste("Tasa de delitos por cada 1.000 habitantes en", año1, "y", año2),
       title = paste("Comparación de tasa de delitos:", .comuna),
       subtitle = paste("Comuna de", .comuna, "en los años", año1, "y", año2),
       shape = "Cambio",
       caption = "Fuente: estadísticas delictuales oficiales del Centro de Estudio y Análisis del Delito (CEAD)"
  ) +
  #temas
  theme(text = element_text(color = color_texto),
        rect = element_rect(fill = color_fondo),
        axis.text = element_text(colour = color_secundario),
        axis.text.y = element_text(face = "bold", color = color_secundario,
                                   margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = 24, color = color_detalle),
        panel.grid.major.x = element_line(linewidth = .5, color = color_fondo),
        axis.ticks = element_blank(),
        axis.title = element_text(color = color_secundario),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8, b = 4))) +
  theme(panel.background = element_rect(fill = color_fondo), 
        plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
  theme(legend.title = element_text(face = "bold", color = color_secundario),
        legend.box.margin = margin(b=-10, t = 4),
        plot.title = element_text(face = "bold", color = color_secundario),
        plot.subtitle = element_text(face = "bold", color = color_secundario, margin = margin(b = -4)),
        plot.caption = element_text(color = color_secundario),
        plot.title.position = "plot"
  )

# guardar ----
nombre_comuna <- tolower(.comuna) |> str_replace_all(" ", "_")

ggsave(filename = paste0("graficos/grafico_comparativo_tasa_", nombre_comuna, "_", año1, "_", año2, ".png"), 
       width = 7, height = 6, scale = 1.3)
