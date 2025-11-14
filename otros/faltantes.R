cead_limpiada |> 
  summarize(sum(as.numeric(cifra)), .by = delitos) |> 
  print(n=Inf)

Delitos contra la vida o integridad de las personas
Violaciones y delitos sexuales
Robos violentos
Violencia intrafamiliar 
Delitos asociados a drogas
Crímenes y simples delitos ley de armas


"Disparo injustificado", "Porte de arma cortante o punzante"


# faltan: 
# - Robos violentos: robo por sorpresa

# - Delitos contra la propiedad no violentos:
# Robo en lugar habitado
# Robos en lugar no habitado
# Robo de vehículo motorizado
# Robo de objetos de o desde vehículo
# Otros robos con fuerza en las cosas
# Hurtos
# Receptación
# 
# - Incivilidades:
# Amenaza con armas (falta)
# Riña Pública
# Consumo de drogas en la vía pública
# Porte de drogas
# Otras faltas a la ley de drogas
# Consumo de alcohol en la vía pública
# Daños
# Desórdenes públicos
# Animales sueltos en la vía pública
# Comercio ilegal
# Ofensas al pudor
# Otras incivilidades