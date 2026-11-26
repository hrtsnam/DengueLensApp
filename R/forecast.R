library(tidyverse)
library(forecast)

ts_bulanan <- function(d) {
  ts(d$kasus, start = c(year(d$tanggal[1]), month(d$tanggal[1])), frequency = 12)
}

# Moving Average: ramalan datar = rata-rata k bulan terakhir.
# Interval dihitung dari simpangan baku galat 1 langkah (pendekatan).
prediksi_ma <- function(y, k, h, level = 95) {
  y <- as.numeric(y); n <- length(y)
  if (n <= k + 2) stop("Data terlalu pendek untuk Moving Average.")
  rata <- as.numeric(stats::filter(y, rep(1 / k, k), sides = 1))
  galat <- y[(k + 1):n] - rata[k:(n - 1)]
  s <- sd(galat); m <- mean(tail(y, k)); z <- qnorm(0.5 + level / 200)
  list(mean = rep(m, h), lower = rep(m - z * s, h), upper = rep(m + z * s, h))
}

ambil <- function(f) {
  list(mean = as.numeric(f$mean), lower = as.numeric(f$lower), upper = as.numeric(f$upper))
}

prediksi_metode <- function(y, metode, h, level = 95) {
  ada_nol <- any(y <= 0)
  hasil <- switch(metode,
                  "Seasonal Naive" = ambil(snaive(y, h = h, level = level)),
                  "MA3"  = prediksi_ma(y, 3,  h, level),
                  "MA12" = prediksi_ma(y, 12, h, level),
                  "SES"  = ambil(ses(y, h = h, level = level)),
                  "Holt-Winters" = ambil(hw(y, h = h, level = level, seasonal = "additive", damped = TRUE,
                                            lambda = if (ada_nol) NULL else 0, biasadj = TRUE)),
                  "ARIMA" = ambil(forecast(auto.arima(y, lambda = if (ada_nol) NULL else 0, biasadj = TRUE),
                                           h = h, level = level)),
                  stop("Metode tidak dikenal: ", metode)
  )
  lapply(hasil, \(v) pmax(v, 0))   # kasus tidak mungkin negatif
}

# MAPE bermasalah bila ada aktual = 0, sehingga sMAPE disediakan sebagai pembanding
metrik <- function(aktual, prediksi) {
  e <- aktual - prediksi
  tibble(
    MAE   = mean(abs(e)),
    RMSE  = sqrt(mean(e^2)),
    sMAPE = mean(2 * abs(e) / (abs(aktual) + abs(prediksi)), na.rm = TRUE) * 100,
    MAPE  = if (any(aktual == 0)) NA_real_ else mean(abs(e / aktual)) * 100
  )
}

# Menentukan otomatis berapa bulan terakhir yang belum lengkap:
# bulan terakhir dibuang bila kurang dari `ambang` x bulan sebelumnya.
# Rasio terendah historis nasional 0,61, sedangkan Okt 2024 = 0,12.
hitung_buang_akhir <- function(kasus, ambang = 0.3, min_sebelumnya = 50, maks = 3) {
  x <- kasus; k <- 0
  while (k < maks && length(x) > 24) {
    n <- length(x)
    if (x[n - 1] >= min_sebelumnya && x[n] / x[n - 1] < ambang) {
      x <- x[-n]; k <- k + 1
    } else break
  }
  k
}

# Rolling-origin evaluation: n_fold jendela uji berurutan, masing-masing `langkah` bulan.
# buang_akhir = "auto" memakai hitung_buang_akhir(); bisa diisi angka manual.
jalankan_prediksi <- function(d, h = 6, n_fold = 5, langkah = 6,
                              buang_akhir = "auto", kriteria = "RMSE", level = 80) {
  if (identical(buang_akhir, "auto")) buang_akhir <- hitung_buang_akhir(d$kasus)
  if (buang_akhir > 0) d <- head(d, -buang_akhir)
  n <- nrow(d)
  
  n_fold <- min(n_fold, floor((n - 36 - langkah) / langkah) + 1)
  if (n_fold < 1) stop("Data terlalu pendek: butuh minimal 36 bulan latih + ", langkah, " bulan uji.")
  
  daftar <- c("Seasonal Naive", "MA3", "MA12", "SES", "Holt-Winters", "ARIMA")
  
  lipat <- map(seq_len(n_fold) - 1, \(k) {
    akhir <- n - langkah - k * langkah
    latih <- ts_bulanan(d[seq_len(akhir), ])
    uji   <- d[(akhir + 1):(akhir + langkah), ]
    preds <- map(daftar, \(m) tryCatch(prediksi_metode(latih, m, langkah)$mean,
                                       error = function(e) rep(NA_real_, langkah))) |>
      set_names(daftar)
    imap(preds, \(p, m) tibble(metode = m, lipatan = k + 1, tanggal = uji$tanggal,
                               aktual = uji$kasus, prediksi = p)) |> list_rbind()
  }) |> list_rbind()
  
  evaluasi <- lipat |>
    filter(!is.na(prediksi)) |>
    group_by(metode) |>
    group_modify(\(g, key) metrik(g$aktual, g$prediksi)) |>
    ungroup() |>
    arrange(.data[[kriteria]])
  
  terbaik <- evaluasi |> filter(!is.na(.data[[kriteria]])) |> slice(1) |> pull(metode)
  r <- prediksi_metode(ts_bulanan(d), terbaik, h, level)
  
  list(
    metode_terbaik = terbaik, kriteria = kriteria, evaluasi = evaluasi,
    uji = lipat |> filter(metode == terbaik),
    ramalan = tibble(tanggal = seq(max(d$tanggal), by = "month", length.out = h + 1)[-1],
                     prediksi = r$mean, bawah = r$lower, atas = r$upper),
    data = d, buang_akhir = buang_akhir, level = level,
    n_fold = n_fold, langkah = langkah
  )
}

plot_prediksi <- function(hasil, judul = "Prediksi kasus DBD", tampil_bulan = 60) {
  ggplot() +
    geom_line(data = tail(hasil$data, tampil_bulan), aes(tanggal, kasus), color = "grey30") +
    geom_line(data = hasil$uji, aes(tanggal, prediksi, group = lipatan),
              color = "orange", linetype = "dashed") +
    geom_ribbon(data = hasil$ramalan, aes(tanggal, ymin = bawah, ymax = atas),
                fill = "steelblue", alpha = 0.25) +
    geom_line(data = hasil$ramalan, aes(tanggal, prediksi), color = "steelblue", linewidth = 1) +
    labs(title = judul,
         subtitle = paste0("Model terpilih: ", hasil$metode_terbaik,
                           " | bulan terakhir dibuang: ", hasil$buang_akhir),
         caption = paste0("Abu = aktual | Oranye = prediksi pada ", hasil$n_fold,
                          " jendela uji | Biru = ramalan, interval ", hasil$level, "%"),
         x = NULL, y = "Kasus per bulan") +
    theme_minimal()
}