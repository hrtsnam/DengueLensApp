library(tidyverse)
library(DBI)
library(RSQLite)

DB_PATH <- "data/clean/dbd.sqlite"

baca_tabel <- function(nama) {
  con <- dbConnect(SQLite(), DB_PATH)
  on.exit(dbDisconnect(con))
  dbReadTable(con, nama) |>
    as_tibble() |>
    mutate(across(any_of(c("tanggal", "mulai", "akhir")), as.Date),
           across(any_of("cukup_untuk_prediksi"), as.logical))
}

# Deret bulanan lengkap (tanpa celah). Bulan hilang diisi interpolasi linear
# dan ditandai di kolom `diinterpolasi`.
get_deret <- function(level = c("nasional", "provinsi"), nama = NULL, interpolasi = TRUE) {
  level <- match.arg(level)
  if (level == "nasional") {
    d <- baca_tabel("kasus_nasional") |> select(tanggal, kasus)
  } else {
    if (is.null(nama)) stop("Nama provinsi harus diisi.")
    d <- baca_tabel("kasus_provinsi") |> filter(provinsi == nama) |> select(tanggal, kasus)
  }
  if (nrow(d) == 0) stop("Data tidak ditemukan untuk: ", nama)
  
  penuh <- tibble(tanggal = seq(min(d$tanggal), max(d$tanggal), by = "month")) |>
    left_join(d, by = "tanggal") |>
    mutate(diinterpolasi = is.na(kasus))
  
  if (interpolasi && any(penuh$diinterpolasi)) {
    x <- as.numeric(penuh$tanggal)
    penuh$kasus <- round(approx(x, penuh$kasus, xout = x)$y)
  }
  penuh |> mutate(tahun = year(tanggal), bulan = month(tanggal))
}