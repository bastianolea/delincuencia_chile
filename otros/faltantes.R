# cead_limpiada |> 
#   summarize(sum(as.numeric(cifra)), .by = delitos) |> 
#   print(n=Inf)
# 
# # mostrar tabla como html
# codigo_html <- datos_cead[[1]][[1]]
# 
# dir <- tempfile()
# dir.create(dir)
# htmlFile <- file.path(dir, "index.html")
# writeLines(codigo_html, con = htmlFile)
# rstudioapi::viewer(htmlFile)
# 
# 
# # por ser familia o grupos de delitos
# "Delitos contra la vida o integridad de las personas",
# "Homicidios y femicidios",
# "Violaciones y delitos sexuales",
# "Robos con violencia o intimidación", #(repetida)
# "Robos violentos",
# "Delitos contra la propiedad no violentos",
# "Robos en lugares habitados y no habitados",
# "Robos de vehículos y sus accesorios",
# "Incivilidades",
# 
# # cubiertos por "Violencia intrafamiliar"
# "Violencia intrafamiliar con lesiones físicas",
# "Violencia intrafamiliar con lesiones psicológicas",
# "Maltrato habitual",
# "Amenazas en contexto de violencia intrafamiliar",
# "Violencia intrafamiliar no clasificada",
# 
# # cubiertos por "Delitos asociados a drogas"
# "Tráfico de sustancias",
# "Microtráfico de sustancias",
# "Elaboración o producción de sustancias",
# "Otras infracciones a la ley de drogas",
# 
# # cubiertos por "Delitos asociados a armas"
# "Disparo injustificado",
# "Porte / posesión de armas o explosivos",
# "Otras infracciones a la ley de armas",
# 
# # cubiertos por "Amenaza falta o riña"
# "Amenaza con armas (falta)",
# "Riña Pública",
# 
# # cubiertos por "Consumo de alcohol y drogas en la vía pública"
# "Consumo de drogas en la vía pública",
# "Porte de drogas",
# "Otras faltas a la ley de drogas",
# "Consumo de alcohol en la vía pública",
# 
# # cubiertos por "Otras incivilidades"
# "Animales sueltos en la vía pública",
# "Comercio ilegal",
# "Ofensas al pudor",
# "Otras incivilidades",
# 
# # cubiertos por "Otros delitos o faltas"
# # "Abigeato",
# "Robo frustrado",
# "Otros delitos o faltas" #(repetido)