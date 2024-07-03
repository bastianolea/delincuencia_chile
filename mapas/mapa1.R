# usando datos de openstreetmap

# remotes::install_github("ropensci/osmdata")
library(dplyr)
library(ggplot2)
library(osmdata)
library(chilemapas)
library(ggmap)
library(rvest)
library(sf)

# obtener calles y otros desde OpenStreetMap ----
# available_features()

#explorar etiquetas disponibles
# available_tags("place")
# available_tags("waterway")
available_tags("landuse")
# available_tags("amenity")

#definir ciudad a obtener
# ciudad = "Santiago"
ciudad = "Región Metropolitana"
# getbb("Santiago")
# getbb("Región Metropolitana")
# ?getbb

estructuras <- getbb(ciudad) %>%
  opq(timeout = 500) %>%
  add_osm_feature(key = "landuse", 
                  value = c("residential", "commercial")) %>%
  osmdata_sf()


# asignar crs
st_crs(estructuras$osm_polygons) <- 4326


# obtener mapas de chile ----

#cargar polígono regional
mapa <- chilemapas::mapa_comunas %>%
  left_join(
    chilemapas::codigos_territoriales %>%
      select(matches("comuna"))) %>%
  filter(codigo_region=="13")

mapa_urbano <- chilemapas::mapa_zonas |> 
  filter(codigo_region == 13)

#solo el borde de la región
region <- chilemapas::mapa_comunas |> 
  filter(codigo_region == 13) |> 
  pull(geometry) |> 
  st_transform(crs = 4326) |>
  st_union()

# mapa_urbano |> 
#   ggplot() +
#   geom_sf(aes(geometry = geometry)) 
#   
# mapa |> 
#   ggplot() +
#   geom_sf(aes(geometry = geometry)) +
#   geom_sf(data = bibliotecas_osm$osm_points,
#           aes(geometry = geometry))



# recortar mapas para que solo sean las de dentro de la región
estructuras_region <- st_intersection(estructuras$osm_polygons, region)
# rios_region <- st_intersection(rios$osm_lines, region)
# calles_region <- st_intersection(calles_principales$osm_lines, region)

# seleccionar solo comunas urbanas 
mapa_filtrado <- mapa |> 
  # left_join(poblacion_censo) |> 
  # filter(poblacion > 100000) |> 
  # filter(!nombre_comuna %in% c("Lampa", "Colina", "Melipilla")) |> 
  filter(nombre_comuna %in% c("Pudahuel", "Cerro Navia", "Conchali", "La Pintana", "El Bosque", 
                              "Estacion Central", "Pedro Aguirre Cerda", "Recoleta", "Independencia", 
                              "La Florida", "Penalolen", "Las Condes", "Lo Barnechea", "Quinta Normal", 
                              "Maipu", "Macul", "Nunoa", "Puente Alto", "Quilicura", "Renca", 
                              "San Bernardo", "San Miguel", "La Granja", "Providencia", "Santiago",
                              "San Joaquin", "Lo Espejo", "La Reina", "San Ramon", "La Cisterna", "Lo Prado", "Cerrillos", "Vitacura", "Huechuraba"
  ))



# mapa_filtrado$nombre_comuna |> dput()
# mapa_filtrado
# st_crs(mapa_filtrado$geometry) <- 4326

# mapa_filtrado_2 <- mapa_filtrado$geometry |> st_transform(crs = 4326)

# transformar mapas
mapa_urbano_2 <- mapa_urbano |> 
  st_as_sf() |> 
  st_union() |> 
  st_transform(crs = 4326)

estructuras_region_2 <- estructuras_region |> 
  st_as_sf() |> 
  st_union() |> 
  st_transform(crs = 4326) |> 
  st_simplify() |> 
  st_make_valid()

sf_use_s2(FALSE)

# filtrar el mapa de comunas urbanas para dejar solo sectores urbanos de esas comunas
mapa_filtrado_urbano <- st_intersection(st_as_sf(mapa_filtrado), 
                                        mapa_urbano |> 
                                          st_as_sf() |> 
                                          st_union())

# estructuras_urbano <- st_intersection(estructuras_region_2, 
#                                       mapa_urbano |> 
#                                         st_as_sf() |> 
#                                         st_union() |> 
#                                         st_transform(crs = 4326))

# filtrar mapa de estructuras (uso de suelo) para que sea solo dentro del límite urbano
estructuras_urbano <- st_intersection(estructuras_region_2 |> 
                                        st_union(), 
                                      mapa_filtrado_urbano |> 
                                        st_as_sf() |> 
                                        st_union() |>
                                        st_transform(crs = 4326))


delincuencia <- arrow::read_parquet("app/cead_delincuencia.parquet") |> 
  rename(delitos = delito_n)

# bibliotecas_data <- bibliotecas_osm$osm_points |> 
#   as_tibble() |> 
#   select(name, amenity, geometry) |> 
#   filter(amenity == "library") |> 
#   filter(!stringr::str_detect(name, "Café|Librería|Libreria|Metro|metro|Archivo|UC")) |> 
#   print(n=Inf)


delitos_conteo <- delincuencia |> 
  filter(delito %in% c("Homicidios", "Hurtos", "Lesiones menos graves, graves o gravísimas",
                       "Violaciones", "Robo con violencia o intimidación", "Robo de objetos de o desde vehículo",
                       "Robo de vehículo motorizado", "Robo en lugar habitado", "Otros robos con fuerza", "Robo por sorpresa")) |>
  group_by(fecha, cut_comuna) |> 
  summarize(delitos = sum(delitos)) |> 
  mutate(cut_comuna = as.character(cut_comuna)) |> 
  filter(lubridate::year(fecha) >= 2018)


mapa_urbano_datos_delincuencia <- mapa_filtrado_urbano |> 
  left_join(delitos_conteo, by = c("codigo_comuna" = "cut_comuna"))


# graficar mapa
mapa_urbano_datos_delincuencia |> 
  filter(fecha == max(fecha)) |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf(color = "white", aes(fill = delitos)) +
  # geom_sf(data = estructuras_urbano, fill = "black", color = NA, alpha = 0.1) +
  # geom_sf(data = bibliotecas_data, size = 5, alpha = .5, color = "black") +
  coord_sf(xlim = c(-70.81, -70.44213), ylim = c(-33.66, -33.31), expand = F) +
  #temas
  theme_void() +
  theme(legend.position = c(.1, .12)) +
  theme(legend.title = element_text(face = "bold"),
        plot.margin = unit(c(5, 5, 5, 15), "mm")) +
  theme(plot.title.position = "plot", 
        plot.title = element_text(margin = margin(t=0, b = 6)),
        plot.subtitle = element_text(margin = margin(b = 8)))

# # guardar imagen
# ggsave(filename = "mapas/mapa_corrupcion_municipios_rm.png",
#        width = 10, height = 10
# )


library(gganimate)
library(transformr)
# install.packages('pak')
# pak::pak('thomasp85/gganimate')

mapa_urbano_datos_delincuencia |> 
  # filter(fecha == max(fecha))
  filter(fecha %in% c("2023-10-01", "2023-11-01", "2023-12-01")) |> 
  ggplot(aes(nombre_comuna, delitos, fill = delitos)) +
  geom_col() +
  transition_states(fecha)

fechas <- unique(mapa_urbano_datos_delincuencia$fecha)


# animación por fechas
mapa_urbano_datos_delincuencia |> 
  ggplot(aes(fill = delitos)) +
  geom_sf(color = "white") +
  coord_sf(xlim = c(-70.81, -70.44213), ylim = c(-33.66, -33.31), expand = F) +
  scale_fill_viridis_c(option = "magma") +
  transition_manual(fecha) +
  labs(title = '{format(fechas, "%B %Y")[frame]}') +
  theme_void() +
  theme(legend.position = c(.1, .12),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(5, 5, 5, 15), "mm"),
        plot.title.position = "plot", 
        plot.title = element_text(margin = margin(t=0, b = 6)),
        plot.subtitle = element_text(margin = margin(b = 8)))
