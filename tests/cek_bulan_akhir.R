for (f in c("db", "forecast")) source(file.path("R", paste0(f, ".R")))

nas <- get_deret("nasional")
cat("\n== Rasio bulan terhadap bulan sebelumnya (Agu-Okt tiap tahun) ==\n")
nas |>
  mutate(rasio = round(kasus / lag(kasus), 2)) |>
  filter(bulan %in% c(8, 9, 10)) |>
  select(tahun, bulan, kasus, rasio) |>
  print(n = Inf)

cat("\n== Jawa Barat, 6 bulan terakhir ==\n")
print(tail(get_deret("provinsi", "Jawa Barat"), 6))

cat("\n== Nasional, hanya Oktober dibuang ==\n")
h1 <- jalankan_prediksi(nas, h = 6, buang_akhir = 1)
print(h1$evaluasi); cat("Terpilih:", h1$metode_terbaik, "\n"); print(h1$ramalan)
print(plot_prediksi(h1, "Nasional (Oktober 2024 dibuang)"))

cat("\n== Nasional, Agustus-Oktober dibuang ==\n")
h3 <- jalankan_prediksi(nas, h = 6, buang_akhir = 3)
print(h3$evaluasi); cat("Terpilih:", h3$metode_terbaik, "\n"); print(h3$ramalan)
print(plot_prediksi(h3, "Nasional (3 bulan terakhir dibuang)"))

cat("\n== Jawa Barat ==\n")
jb <- jalankan_prediksi(get_deret("provinsi", "Jawa Barat"), h = 6)
print(jb$evaluasi); cat("Terpilih:", jb$metode_terbaik, "\n"); print(jb$ramalan)
print(plot_prediksi(jb, "Jawa Barat"))

warnings()