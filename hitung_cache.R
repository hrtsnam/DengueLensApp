# Menghitung prediksi lengkap (6 model, 5 jendela uji) untuk wilayah utama
# dan menyimpannya agar aplikasi tidak perlu menghitung ulang.
# Jalankan sekali (disarankan: Source as Background Job). Perkiraan 10-15 menit.

for (f in c("db", "forecast")) source(file.path("R", paste0(f, ".R")))

wilayah <- c("Nasional", "Jawa Barat", "Jawa Timur", "Jawa Tengah", "DKI Jakarta", "Bali")
file_cache <- "data/clean/cache_prediksi.rds"

cache <- if (file.exists(file_cache)) readRDS(file_cache) else list()

for (w in wilayah) {
  if (!is.null(cache[[w]])) { cat("Lewati (sudah ada):", w, "\n"); next }
  cat("Menghitung:", w, "...\n")
  t0 <- Sys.time()
  cache[[w]] <- jalankan_prediksi(get_deret_wilayah(w), h = 12)
  saveRDS(cache, file_cache)
  cat("  selesai dalam", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "detik\n")
}

cat("\nSelesai. Wilayah di cache:", paste(names(cache), collapse = ", "), "\n")
