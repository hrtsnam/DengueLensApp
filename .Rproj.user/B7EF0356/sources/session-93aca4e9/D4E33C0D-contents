library(tidyverse)

tmp <- read_csv("data/raw/SEAROFilteredDataTemporal.csv", show_col_types = FALSE)
sp  <- read_csv("data/raw/SEAROFilteredDataSpatial.csv",  show_col_types = FALSE)

# --- 1. Definisi kasus & sumber data ---
tmp |> count(case_definition_standardised)
tmp |> mutate(sumber = str_extract(UUID, "^[^-]+")) |> count(sumber, T_res, S_res)

# --- 2. Seragamkan nama provinsi (sementara, untuk pengecekan) ---
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

bulanan <- tmp |>
  filter(T_res == "Month", S_res == "Admin1") |>
  mutate(provinsi = coalesce(unname(peta_prov[adm_1_name]), adm_1_name))

# --- 3. Berapa provinsi, dan apakah tiap provinsi lengkap 118 bulan? ---
n_distinct(bulanan$provinsi)
bulanan |> count(provinsi) |> arrange(n) |> print(n = Inf)

# --- 4. Duplikat provinsi-bulan setelah nama diseragamkan ---
dup <- bulanan |> count(provinsi, calendar_start_date) |> filter(n > 1)
nrow(dup)
bulanan |>
  semi_join(dup, by = c("provinsi", "calendar_start_date")) |>
  select(provinsi, calendar_start_date, dengue_total, UUID) |>
  arrange(provinsi, calendar_start_date) |>
  head(20)

# --- 5. Total nasional vs jumlah semua provinsi ---
nasional <- tmp |>
  filter(T_res == "Month", S_res == "Admin0") |>
  select(calendar_start_date, nasional = dengue_total)

jumlah_prov <- bulanan |>
  group_by(calendar_start_date) |>
  summarise(jumlah_prov = sum(dengue_total), n_prov = n_distinct(provinsi))

banding <- nasional |>
  left_join(jumlah_prov, by = "calendar_start_date") |>
  mutate(selisih = nasional - jumlah_prov)
nrow(nasional)
print(banding, n = 15)
summary(banding$selisih)

# --- 6. Apakah Temporal dan Spatial identik untuk level provinsi? ---
kolom <- c("adm_1_name", "T_res", "calendar_start_date", "calendar_end_date", "dengue_total")
a <- tmp |> filter(S_res == "Admin1") |> select(all_of(kolom))
b <- sp  |> filter(S_res == "Admin1") |> select(all_of(kolom))
nrow(anti_join(a, b, by = kolom))   # ada di Temporal, tidak ada di Spatial
nrow(anti_join(b, a, by = kolom))   # ada di Spatial, tidak ada di Temporal

# --- 7. Ringkasan nilai kasus bulanan (untuk cek nol & pencilan) ---
summary(bulanan$dengue_total)
sum(bulanan$dengue_total == 0)