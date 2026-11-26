for (f in c("db", "forecast")) source(file.path("R", paste0(f, ".R")))

cat("\n== Nasional (auto) ==\n")
waktu <- system.time(h <- jalankan_prediksi(get_deret("nasional"), h = 6))
cat("Bulan terakhir dibuang:", h$buang_akhir, "| waktu (detik):", round(waktu["elapsed"], 1), "\n")
print(h$evaluasi); cat("Terpilih:", h$metode_terbaik, "\n"); print(h$ramalan)
print(plot_prediksi(h, "Nasional"))

cat("\n== Jawa Barat ==\n")
jb <- jalankan_prediksi(get_deret("provinsi", "Jawa Barat"), h = 6)
print(jb$evaluasi); cat("Terpilih:", jb$metode_terbaik, "\n"); print(jb$ramalan)
print(plot_prediksi(jb, "Jawa Barat"))

cat("\n== Maluku (banyak nol) ==\n")
ml <- jalankan_prediksi(get_deret("provinsi", "Maluku"), h = 6)
print(ml$evaluasi); cat("Terpilih:", ml$metode_terbaik, "\n")

cat("\n== Pindai bulan terakhir semua provinsi layak prediksi ==\n")
ok <- baca_tabel("provinsi_ref") |> filter(cukup_untuk_prediksi) |> pull(provinsi)
pindai <- map(ok, \(p) {
  d <- get_deret("provinsi", p); n <- nrow(d)
  tibble(provinsi = p, bulan_akhir = d$tanggal[n], kasus_akhir = d$kasus[n],
         kasus_sebelumnya = d$kasus[n - 1], rasio = round(d$kasus[n] / d$kasus[n - 1], 2),
         dibuang = hitung_buang_akhir(d$kasus))
}) |> list_rbind()
print(pindai |> arrange(rasio), n = Inf)