
# datos <- arrow::read_parquet("datos_procesados/cead_delincuencia_chile.parquet")
# readr::write_rds(datos, "datos_procesados/cead_delincuencia_chile.rds", compress = "none")

bench::mark(check = FALSE, iterations = 3, 
  "arrow parquet" = arrow::read_parquet("datos_procesados/cead_delincuencia_chile.parquet"),
  "nanoparquet" = nanoparquet::read_parquet("datos_procesados/cead_delincuencia_chile.parquet"),
  "readr rds" = readr::read_rds("datos_procesados/cead_delincuencia_chile.rds"),
  "base rds" = readRDS("datos_procesados/cead_delincuencia_chile.rds"),
  "base csv" = read.csv2("datos_procesados/cead_delincuencia_chile.csv"),
  "readr csv" = readr::read_csv2("datos_procesados/cead_delincuencia_chile.csv"),
  "arrow csv" = arrow::read_csv2_arrow("datos_procesados/cead_delincuencia_chile.csv")
)


bench::mark(check = FALSE, iterations = 20,
            arrow::read_parquet("datos_procesados/cead_delincuencia_chile.parquet"),
            nanoparquet::read_parquet("datos_procesados/cead_delincuencia_chile_b.parquet")
)
