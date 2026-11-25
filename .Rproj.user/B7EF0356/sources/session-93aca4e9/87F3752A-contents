library(tidyverse)

tmp <- read_csv("data/raw/SEAROFilteredDataSpatial.csv", show_col_types = FALSE)

glimpse(tmp)

# cek ejaan lewat unique(tmp$adm_0_name) jika kosong
idn <- tmp |> filter(adm_0_name == "INDONESIA")

cat("Jumlah baris Indonesia:", nrow(idn), "\n")

# kombinasi resolusi waktu & wilayah
idn |> count(T_res, S_res)

# sebaran per tahun
idn |> count(Year) |> print(n = Inf)

idn |> summarise(
  kosong_dengue = sum(is.na(dengue_total)),
  duplikat      = sum(duplicated(idn)),
  min_tgl       = min(calendar_start_date),
  max_tgl       = max(calendar_end_date)
)
unique(idn$adm_1_name)