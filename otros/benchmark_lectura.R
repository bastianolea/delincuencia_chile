
bench::mark(check = FALSE, iterations = 20,
  arrow::read_parquet("datos_procesados/cead_delincuencia_chile.parquet"),
  readr::read_csv2("datos_procesados/cead_delincuencia_chile.csv"),
  arrow::read_csv2_arrow("datos_procesados/cead_delincuencia_chile.csv"),
  nanoparquet::read_parquet("datos_procesados/cead_delincuencia_chile.parquet")
)


bench::mark(check = FALSE, iterations = 20,
            arrow::read_parquet("datos_procesados/cead_delincuencia_chile.parquet"),
            nanoparquet::read_parquet("datos_procesados/cead_delincuencia_chile_b.parquet")
)
