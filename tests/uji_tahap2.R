for (f in c("db", "descriptive", "distribution", "correlation", "hypothesis"))
  source(file.path("R", paste0(f, ".R")))

nas <- get_deret("nasional")
cat("\n== Statistik deskriptif nasional ==\n");  print(statistik_deskriptif(nas$kasus))
cat("\nPerubahan bulan terakhir (%):", round(perubahan_terakhir(nas$kasus), 1), "\n")

cat("\n== Uji dispersi (Poisson) ==\n");         print(uji_dispersi_poisson(nas$kasus))
cat("\n== Poisson vs Binomial Negatif ==\n");    print(bandingkan_distribusi(nas$kasus)$tabel)

cat("\n== Korelasi ==\n")
print(korelasi_antarperiode(nas$kasus, lag = 1))
print(korelasi_waktu(nas$tanggal, nas$kasus))

cat("\n== Uji beda: kasus bulanan nasional 2022 vs 2023 ==\n")
d2 <- nas |> filter(tahun %in% c(2022, 2023))
hasil <- uji_beda(d2$kasus, d2$tahun)
print(hasil$ringkasan); print(hasil$normalitas); cat(hasil$interpretasi, "\n")

cat("\n== Uji beda: kasus bulanan 2023 antar-pulau (provinsi dengan data memadai) ==\n")
ok <- baca_tabel("provinsi_ref") |> filter(cukup_untuk_prediksi) |> select(provinsi, pulau)
d3 <- baca_tabel("kasus_provinsi") |> filter(tahun == 2023) |> inner_join(ok, by = "provinsi")
hasil2 <- uji_beda(d3$kasus, d3$pulau)
print(hasil2$ringkasan); cat(hasil2$interpretasi, "\n")