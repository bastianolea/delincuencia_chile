# Visualizador de estadísticas oficiales de delincuencia en Chile


_Bastián Olea Herrera_


## Aplicación

En este [visualizador web](https://bastianoleah.shinyapps.io/delincuencia_chile) se presentan gráficos con datos estadísticas delictuales entregadas por el [Centro de Estudio y Análisis del Delito (CEAD),](https://cead.minsegpublica.gob.cl/estadisticas-delictuales/) quienes a su vez obtienen los datos desde reportes de Carabineros y la Policía de Investigaciones de Chile al Ministerio del Interior y Seguridad Pública.

Según el [CEAD,](https://cead.minsegpublica.gob.cl/estadisticas-delictuales/) cada dato de delito se compone por: _denuncias formales que la ciudadanía realiza en alguna unidad policial posterior a la ocurrencia del delito, más los delitos de los que la policía toma conocimiento al efectuar una detención en flagrancia, es decir, mientras ocurre el ilícito._

El objetivo de esta plataforma es transparentar datos objetivos de la delincuencia en el país, otorgándoles contexto para tratar el tema con seriedad en lugar de sensacionalismo y provecho político.

[La aplicación web está disponible en shinyapps.io](https://bastianoleah.shinyapps.io/delincuencia_chile), o bien, puedes clonar este repositorio en tu equipo para usarla por medio de RStudio.


![Visualizador de datos de delincuencia en Chile](otros/pantallazo_delincuencia_chile.jpg "Visualizador de datos de delincuencia en Chile")



## Datos

Los datos se obtuvieron directamente desde CEAD haciendo uso de [técnicas de web scraping en R, detalladas en este tutorial.](https://bastianolea.rbind.io/blog/tutorial_delitos_cead/). En este repositorio, el script `obtener_datos_delincuencia.R` realiza el scraping del sitio web de CEAD para los años y comunas que se le indique, y guarda los datos crudos (las tablas en formato html). Luego, el script `procesar_datos_delincuencia.R` carga estos datos crudos y los transforma a tablas, las limpia, y guarda los datos en formato `parquet` para lectura rápida. No se puede guardar en formato Excel porque tiene más de un millón de filas.

Los datos limpios están disponibles en la carpeta `datos_procesados`.


### Descargar datos

La base de datos de delitos denunciados en Chile del Centro de Estudio y Análisis del Delito (CEAD), obtenida, ordenada y limpiada mediante el código de este repositorio, se encuentra disponible en formato `.parquet` [en este enlace.](https://github.com/bastianolea/delincuencia_chile/raw/main/datos_procesados/cead_delincuencia_chile.parquet)

**Nota:** los datos ya no están disponibles en formato `.csv`, porque la cantidad de observaciones aumentó y el archivo resultante pesaba más de 100 megas. En comparación, el formato `.parquet`, que es más eficiente y rápido, pesa sólo 1.1MB.


## Actualizaciones

**Actualización 13/11/2025:**
- Datos actualizados hasta junio de 2025 (máximo disponible en CEAD a la fecha)
- Actualización de código para cambio de dirección del CEAD después del ciberataque y cambio de ministerio

**Actualización 17/12/2024:**
- Datos actualizados hasta septiembre de 2024 (máximo disponible en CEAD a la fecha)
- Optimización de aplicación para que use tipografías locales en vez de cargarlas desde Google Fonts
- Se vuelve a usar el paquete `arrow` para cargar los datos, porque es más rápido que `nanoparquet`.
- Correcciones mínimas.

**Actualización 03/07/2024:**

- Datos actualizados hasta marzo de 2024 (máximo disponible en CEAD a la fecha)
- Actualización del código de scraping para que funcione con la actualización del sitio de CEAD
- Los datos ahora representan _casos policiales_ en vez de solo _denuncias._ Los casos policiales "consideran las denuncias de delitos que realiza la comunidad en las unidades policiales, más las detenciones que realizan las policías ante la ocurrencia de delitos flagrantes".
- Se cambia el paquete que carga los datos a `nanoparquet`, que tiene menos dependencias que `arrow`
- Se flexibiliza el código de la app para que se adapte a las fechas que vienen en los datos, para facilitar actualizaciones futuras

## Notas metodológicas

Fueron considerados delitos graves:
- Homicidios
- Violencia intrafamiliar 
- Robos con violencia o intimidación
- Robo violento de vehículo motorizado
- Robo de vehículo motorizado
- Robo en lugar habitado
- Robo por sorpresa
- Robo frustrado
- Hurtos

Fueron considerados delitos de mayor connotación social:
- Homicidios
- Violencia intrafamiliar 
- Robos con violencia o intimidación
- Robo violento de vehículo motorizado
- Robo en lugar habitado  
- Robo por sorpresa
- Robo frustrado
- Hurtos
- Robos en lugar no habitado
- Robo de vehículo motorizado
- Otros robos con fuerza en las cosas
- Lesiones graves o gravísimas
- Lesiones menos graves
- Lesiones leves
- Homicidios
- Femicidios
- Violaciones

Delitos omitidos por poca relevancia:
- Abigeato
- Otros delitos o faltas

Delitos omitidos por ser considerados demasiado específicos/minoritarios (pero de todas maneras son contabilizados al estar dentro de otro grupo de delitos):
- cubiertos por "Violencia intrafamiliar"
  - Violencia intrafamiliar con lesiones físicas
  - Violencia intrafamiliar con lesiones psicológicas
  - Maltrato habitual
  - Amenazas en contexto de violencia intrafamiliar
  - Violencia intrafamiliar no clasificada
- cubiertos por "Delitos asociados a drogas"
  - Tráfico de sustancias
  - Microtráfico de sustancias
  - Elaboración o producción de sustancias
  - Otras infracciones a la ley de drogas
- cubiertos por "Delitos asociados a armas"
  - Disparo injustificado
  - Porte / posesión de armas o explosivos
  - Otras infracciones a la ley de armas
- cubiertos por "Amenaza falta o riña"
  - Amenaza con armas (falta)
  - Riña Pública
- cubiertos por "Consumo de alcohol y drogas en la vía pública"
  - Consumo de drogas en la vía pública
  - Porte de drogas
  - Otras faltas a la ley de drogas
  - Consumo de alcohol en la vía pública
- cubiertos por "Otras incivilidades"
  - Animales sueltos en la vía pública
  - Comercio ilegal
  - Ofensas al pudor
  - Otras incivilidades