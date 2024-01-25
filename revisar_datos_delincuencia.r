library(dplyr)
library(ggplot2)
library(slider)
library(lubridate)
library(stringr)
library(forcats)
library(tidyr)

#cargar datos ----
delincuencia <- arrow::read_parquet("app/cead_delincuencia.parquet") |> 
  rename(delitos = delito_n)

presidentes <- readr::read_csv("app/periodos_presidenciales_chile.csv", show_col_types = F) |>
  select(presidente = nombre, fecha_inicio, fecha_termino)


presidentes_2 <- presidentes |> 
  group_by(presidente) |> 
  mutate(id = 1:n()) |> 
  ungroup() |> 
  mutate(presidente_id = paste(str_remove(presidente, "\\w+$"), id)) |> 
  mutate(fecha = fecha_inicio) |> 
  tidyr::complete(fecha = seq(paste0(presidentes$fecha_inicio |> min() |> year(), "-01-01") |> ymd(), 
                                              paste0(delincuencia$fecha |> max() |> year(), "-12-01") |> ymd(), 
                                              by='months')) |>
  arrange(desc(fecha)) |> 
  tidyr::fill(c(presidente, presidente_id, fecha_inicio, fecha_termino), .direction = "downup")


delincuencia |> 
  arrange(comuna, desc(fecha)) |> 
  left_join(presidentes_2, join_by(fecha)) |> 
  group_by(comuna, delito)

.comuna = "La Florida"
color_fondo = "#1f272b"
color_texto = "#cdf2ef"
color_secundario = "#317773"
color_detalle = "#1e3534"
color_destacado = "#cf5a13"

color_positivo = "#91b423"
color_negativo = "#c03426"


#delitos por año
datos <- delincuencia |> 
  filter(comuna == .comuna) |> 
  mutate(año = year(fecha)) |> 
  group_by(comuna, año) |> 
  summarize(delitos = sum(delitos))

datos |> 
  ggplot(aes(as.factor(año), delitos)) +
  geom_hline(yintercept = mean(datos$delitos), linetype = "dashed", color = color_destacado, linewidth = 0.7) +
  geom_col(fill = color_secundario, width = 0.5) +
  geom_text(aes(label = format(delitos, big.mark ="."),
                y = delitos * 0.99),
            hjust = 0, angle = -90, color = color_texto, fontface = "bold") +
  
  scale_y_continuous(expand = expansion(c(0.01, 0.03)), labels = ~format(.x, big.mark = ".")) +
  # scale_y_discrete(expand = expansion(0.1)) +
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
       x = paste("Delitos totales anuales en la comuna de", .comuna))







#delitos mensuales promedio durante el periodo de cada presidente
delincuencia |> 
  filter(comuna == .comuna) |> 
  mutate(año = year(fecha),
         mes = month(fecha)) |> 
  group_by(comuna, año, mes) |> 
    summarize(delitos = sum(delitos)) |> 
  ungroup() |> 
  mutate(fecha = dmy(paste(1, mes, año))) |> 
  left_join(presidentes_2, join_by(fecha)) |> 
  group_by(comuna, presidente_id) |> 
  summarize(delitos = mean(delitos))


#delitos diarios promedio durante el periodo de cada presidente
datos <- delincuencia |> 
  filter(comuna == .comuna) |> 
  group_by(comuna, fecha) |> 
  left_join(presidentes_2, join_by(fecha)) |> 
  mutate(presidente_id = presidente_id |> str_remove_all (" 1") |> 
           fct_reorder(fecha)) |> 
  group_by(comuna, presidente_id) |> 
  summarize(delitos = mean(delitos)) |> 
  mutate(presidente_id = presidente_id|> fct_rev())


datos |> 
  ggplot(aes(y = presidente_id, 
             x = delitos)) +
  geom_col(fill = color_secundario, width = 0.5) +
  geom_text(aes(label = format(round(delitos, 1), big.mark = ".", decimal.mark = ","),
                x = delitos * 0.98),
            hjust = 1, color = color_texto, fontface = "bold") +
  geom_vline(xintercept = max(datos$delitos), linetype = "dashed", color = color_negativo, linewidth = 0.7) +
  geom_vline(xintercept = min(datos$delitos), linetype = "dashed", color = color_positivo, linewidth = 0.7) +
  scale_x_continuous(expand = expansion(0)) +
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
       x = "Promedio de delitos diarios")





#delitos maximos por año ----
datos <- delincuencia |> 
  filter(comuna == .comuna) |> 
  mutate(año = year(fecha)) |> 
  filter(año >= 2018) |> 
  group_by(comuna, año, delito) |>
  summarize(delitos = sum(delitos)) |> 
  arrange(desc(año), desc(delitos)) |> 
  group_by(año) |> 
  slice_max(delitos, n = 3) |> 
  group_by(año) |> 
  mutate(delito = delito |> str_wrap(20) |> as.factor() |> fct_reorder2(año, delitos))

maximos <- datos |> 
  group_by(delito) |> 
  slice_max(delitos)

datos |> 
  ggplot(aes(delito, delitos, 
             fill = delito, color = delito)) +
  geom_col(position = position_dodge2(), width = 0.5) +
  #lineas de maximos
  geom_hline(data = maximos |> select(-año),
             aes(yintercept = delitos, color = delito),
             linetype = "dashed") +
  ggrepel::geom_text_repel(data = maximos |> rename(año_max = año) |> mutate(año = max(datos$año)),
            aes(label = glue(" {año_max}: {format(delitos, big.mark='.')}"),
                x = 4), hjust = 0, vjust = 0.5,
            direction = "y", box.padding = 0, xlim = c(2, Inf)
            ) +
  geom_text(data = datos |> filter(año == max(datos$año)),
                           aes(label = format(delitos, big.mark='.'),
                               y = delitos*1.02),
                               hjust = 1, vjust = 0.5, angle = -90
  ) +
  facet_wrap(~año, nrow = 1, scales = "free_x", strip.position = "bottom") +
  scale_y_continuous(expand = expansion(c(0.01, 0.02)), labels = ~format(.x, big.mark = ".")) +
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
  theme(legend.position = "right",
        legend.title = element_blank(), 
        legend.key = element_rect(fill = color_fondo),
        legend.text = element_text(color = color_texto, size = 10, margin = margin(t=4, b = 4))) +
  theme(panel.background = element_rect(fill = color_detalle), 
        plot.background = element_rect(fill = color_fondo, linewidth = 0)) +
  labs(y = paste("Cantidad de delitos principales en", .comuna),
       x = NULL)
  