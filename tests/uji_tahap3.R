for (f in c("db", "forecast"))
  source(file.path("R", paste0(f, ".R")))

nas <- get_deret("nasional")
cat("\n== 10 bulan terakhir (nasional) ==\n"); print(tail(nas, 10))

cat("\n== Nasional, semua bulan dipakai ==\n")
h0 <- jalankan_prediksi(nas, h = 6, buang_akhir = 0)
print(h0$evaluasi); cat("Terpilih:", h0$metode_terbaik, "\n"); print(h0$ramalan)

cat("\n== Nasional, 3 bulan terakhir dibuang ==\n")
h3 <- jalankan_prediksi(nas, h = 6, buang_akhir = 3)
print(h3$evaluasi); cat("Terpilih:", h3$metode_terbaik, "\n"); print(h3$ramalan)

print(plot_prediksi(h0, "Nasional (semua bulan)"))
print(plot_prediksi(h3, "Nasional (3 bulan terakhir dibuang)"))

cat("\n== Jawa Barat ==\n")
jb <- jalankan_prediksi(get_deret("provinsi", "Jawa Barat"), h = 6)
print(jb$evaluasi); cat("Terpilih:", jb$metode_terbaik, "\n"); print(jb$ramalan)
print(plot_prediksi(jb, "Jawa Barat"))

cat("\n== Maluku (banyak nol) ==\n")
ml <- jalankan_prediksi(get_deret("provinsi", "Maluku"), h = 6)
print(ml$evaluasi); cat("Terpilih:", ml$metode_terbaik, "\n")