delincuencia |> 
  # filter(comuna == "Santiago") |> 
  filter(delito == "Homicidios") |> 
  # filter(fecha >= "2019-01-01") |> 
  mutate(año = year(fecha)) |> 
  # group_by(fecha) |> 
  group_by(año) |> 
  summarize(n = sum(delito_n)) |> 
  left_join(censo_anual, by = "año") |> 
  mutate(tasa = (n/pob)*100000) |> 
  ggplot(aes(año, tasa)) +
  geom_line() +
  geom_point() +
  theme_minimal()
