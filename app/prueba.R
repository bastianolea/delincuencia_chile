library(arrow)


censo <- arrow::read_parquet("app/censo_proyecciones_año.parquet")
censo |> 
  readr::write_csv("app/censo_proyecciones_año.csv")

censo <- readr::read_csv("app/censo_proyecciones_año.csv")
