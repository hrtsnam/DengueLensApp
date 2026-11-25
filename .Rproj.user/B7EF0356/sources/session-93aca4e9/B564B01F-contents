library(tidyverse)
library(DBI)
library(RSQLite)

# ---- Konfigurasi ----
FILE_TEMPORAL      <- "data/raw/SEAROFilteredDataTemporal.csv"
DB_PATH            <- "data/clean/dbd.sqlite"
MIN_BULAN_PREDIKSI <- 48   # minimal jumlah bulan agar provinsi layak diprediksi

# ---- Penyeragaman nama provinsi ----
peta_prov <- c(
  "BABEL"                      = "KEPULAUAN BANGKA BELITUNG",
  "BANGKA BELITUNG"            = "KEPULAUAN BANGKA BELITUNG",
  "D.I YOGYA"                  = "DI YOGYAKARTA",
  "DAERAH ISTIMEWA YOGYAKARTA" = "DI YOGYAKARTA",
  "KALIMANTAN SELATA"          = "KALIMANTAN SELATAN",
  "SULAWESI SELATA"            = "SULAWESI SELATAN",
  "SUMATERA SELATA"            = "SUMATERA SELATAN",
  "NUSATENGGARA BARAT"         = "NUSA TENGGARA BARAT",
  "NUSATENGGARA TIMUR"         = "NUSA TENGGARA TIMUR",
  "KEPULAUAN-RIAU"             = "KEPULAUAN RIAU",
  "NANGGROE ACEH DARUSSALAM"   = "ACEH"
)

rapikan_nama <- function(x) {
  x |> str_to_title() |>
    str_replace("^Dki ", "DKI ") |>
    str_replace("^Di ", "DI ")
}

# ---- Kelompok pulau ----
pulau_ref <- enframe(list(
  "Sumatera" = c("Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Jambi",
                 "Sumatera Selatan", "Bengkulu", "Lampung",
                 "Kepulauan Bangka Belitung", "Kepulauan Riau"),
  "Jawa" = c("DKI Jakarta", "Jawa Barat", "Banten", "Jawa Tengah",
             "DI Yogyakarta", "Jawa Timur"),
  "Bali & Nusa Tenggara" = c("Bali", "Nusa Tenggara Barat", "Nusa Tenggara Timur"),
  "Kalimantan" = c("Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Selatan",
                   "Kalimantan Timur", "Kalimantan Utara"),
  "Sulawesi" = c("Sulawesi Utara", "Gorontalo", "Sulawesi Tengah", "Sulawesi Barat",
                 "Sulawesi Selatan", "Sulawesi Tenggara"),
  "Maluku & Papua" = c("Maluku", "Maluku Utara", "Papua", "Papua Barat",
                       "Papua Barat Daya", "Papua Pegunungan", "Papua Selatan",
                       "Papua Tengah")
), name = "pulau", value = "provinsi") |> unnest(provinsi)

# ---- Baca data ----
raw <- read_csv(FILE_TEMPORAL, show_col_types = FALSE)

# ---- Data provinsi (bulanan, Admin1) ----
prov <- raw |>
  filter(adm_0_name == "INDONESIA", T_res == "Month", S_res == "Admin1",
         case_definition_standardised == "Total") |>
  mutate(provinsi = rapikan_nama(coalesce(unname(peta_prov[adm_1_name]), adm_1_name))) |>
  transmute(provinsi,
            tanggal = calendar_start_date,
            tahun   = as.integer(Year),
            bulan   = month(calendar_start_date),
            kasus   = dengue_total) |>
  arrange(provinsi, tanggal)

stopifnot("Ada duplikat provinsi-bulan" = !anyDuplicated(prov[c("provinsi", "tanggal")]))

tak_terpetakan <- setdiff(unique(prov$provinsi), pulau_ref$provinsi)
if (length(tak_terpetakan) > 0) {
  stop("Provinsi belum masuk daftar pulau: ", paste(tak_terpetakan, collapse = ", "))
}

# ---- Data nasional (bulanan, Admin0) ----
nas <- raw |>
  filter(adm_0_name == "INDONESIA", T_res == "Month", S_res == "Admin0") |>
  transmute(tanggal = calendar_start_date,
            tahun   = as.integer(Year),
            bulan   = month(calendar_start_date),
            kasus   = dengue_total) |>
  arrange(tanggal)

stopifnot("Ada duplikat bulan nasional" = !anyDuplicated(nas$tanggal))

# ---- Bulan yang hilang di semua provinsi ----
kalender <- tibble(tanggal = seq(min(nas$tanggal), max(nas$tanggal), by = "month"))
bulan_hilang <- kalender |> anti_join(distinct(prov, tanggal), by = "tanggal")

# ---- Referensi provinsi: kelengkapan data ----
prov_ref <- prov |>
  group_by(provinsi) |>
  summarise(mulai = min(tanggal), akhir = max(tanggal),
            n_bulan_ada = n(), n_nol = sum(kasus == 0),
            total_kasus = sum(kasus), .groups = "drop") |>
  mutate(n_bulan_seharusnya = as.integer(interval(mulai, akhir) %/% months(1)) + 1L,
         n_bulan_hilang      = n_bulan_seharusnya - n_bulan_ada,
         cukup_untuk_prediksi = n_bulan_ada >= MIN_BULAN_PREDIKSI) |>
  left_join(pulau_ref, by = "provinsi") |>
  relocate(pulau, .after = provinsi)

# ---- Metadata (untuk halaman Informasi Dataset) ----
akhir_bulan <- function(d) as.character(ceiling_date(max(d), "month") - days(1))

metadata <- tibble(
  kunci = c("sumber", "url", "file", "definisi_kasus", "resolusi",
            "periode_mulai", "periode_akhir_nasional", "periode_akhir_provinsi",
            "tanggal_impor", "catatan"),
  nilai = c("OpenDengue (WHO SEARO)", "https://opendengue.org/data",
            basename(FILE_TEMPORAL), "Total (kasus dengue yang dilaporkan)",
            "Bulanan; nasional dan provinsi",
            as.character(min(nas$tanggal)), akhir_bulan(nas$tanggal),
            akhir_bulan(prov$tanggal), as.character(Sys.Date()),
            paste("Tidak ada data kabupaten/kota. Februari 2016 dan September 2020 tidak tersedia",
                  "untuk provinsi. Data provinsi berakhir Juli 2024 (Papua dan Papua Barat lebih awal).",
                  "Empat provinsi Papua hasil pemekaran hampir seluruhnya bernilai nol."))
)

# ---- Simpan ke SQLite ----
dir.create(dirname(DB_PATH), recursive = TRUE, showWarnings = FALSE)
con <- dbConnect(SQLite(), DB_PATH)

tulis <- function(nama, df) {
  df <- df |> mutate(across(where(~ inherits(.x, "Date")), as.character))
  dbWriteTable(con, nama, df, overwrite = TRUE)
}
tulis("kasus_provinsi", prov)
tulis("kasus_nasional", nas)
tulis("provinsi_ref",   prov_ref)
tulis("bulan_hilang",   bulan_hilang)
tulis("metadata",       metadata)

# ---- Ringkasan untuk dicek ----
cat("\n=== RINGKASAN ===\n")
cat("Tabel di database:", paste(dbListTables(con), collapse = ", "), "\n")
cat("Baris provinsi:", nrow(prov), "| Provinsi:", n_distinct(prov$provinsi),
    "| Baris nasional:", nrow(nas), "| Nol nasional:", sum(nas$kasus == 0), "\n")

cat("\nBulan yang tidak ada di SEMUA provinsi:\n")
print(bulan_hilang)

cat("\nKelengkapan per provinsi (urut dari yang paling sedikit):\n")
prov_ref |>
  arrange(n_bulan_ada) |>
  select(provinsi, pulau, mulai, akhir, n_bulan_ada, n_bulan_hilang, n_nol, cukup_untuk_prediksi) |>
  print(n = Inf)

cat("\nProvinsi dengan nilai nol:\n")
prov_ref |> filter(n_nol > 0) |> arrange(desc(n_nol)) |>
  select(provinsi, n_nol, n_bulan_ada) |> print(n = Inf)

dbDisconnect(con)