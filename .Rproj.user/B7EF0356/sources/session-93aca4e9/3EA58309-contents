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
                  "Holt-Winters" = ambil(hw(y, h = h, level = level,
                                            seasonal = if (ada_nol) "additive" else "multiplicative")),
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

# d: hasil get_deret(). h: jumlah bulan ke depan. n_uji: bulan terakhir untuk data uji.
# buang_akhir: jumlah bulan terakhir yang dianggap belum lengkap dan dikeluarkan.
jalankan_prediksi <- function(d, h = 6, n_uji = 12, buang_akhir = 0, kriteria = "RMSE") {
  if (buang_akhir > 0) d <- head(d, -buang_akhir)
  n <- nrow(d)
  if (n < 36) stop("Data kurang dari 36 bulan, terlalu pendek untuk prediksi.")
  
  y       <- ts_bulanan(d)
  y_latih <- ts_bulanan(head(d, n - n_uji))
  aktual  <- tail(d$kasus, n_uji)
  
  daftar <- c("Seasonal Naive", "MA3", "MA12", "SES", "Holt-Winters", "ARIMA")
  pred_uji <- map(daftar, \(m) tryCatch(prediksi_metode(y_latih, m, n_uji)$mean,
                                        error = function(e) NULL)) |> set_names(daftar)
  
  evaluasi <- imap(pred_uji, \(p, m) {
    if (is.null(p)) tibble(metode = m, MAE = NA_real_, RMSE = NA_real_, sMAPE = NA_real_, MAPE = NA_real_)
    else bind_cols(tibble(metode = m), metrik(aktual, p))
  }) |> list_rbind() |> arrange(.data[[kriteria]])
  
  terbaik <- evaluasi |> filter(!is.na(.data[[kriteria]])) |> slice(1) |> pull(metode)
  r <- prediksi_metode(y, terbaik, h)
  
  list(
    metode_terbaik = terbaik,
    kriteria = kriteria,
    evaluasi = evaluasi,
    uji = tibble(tanggal = tail(d$tanggal, n_uji), aktual = aktual,
                 prediksi = pred_uji[[terbaik]]),
    ramalan = tibble(tanggal = seq(max(d$tanggal), by = "month", length.out = h + 1)[-1],
                     prediksi = r$mean, bawah = r$lower, atas = r$upper),
    data = d
  )
}

plot_prediksi <- function(hasil, judul = "Prediksi kasus DBD") {
  ggplot() +
    geom_line(data = hasil$data, aes(tanggal, kasus), color = "grey30") +
    geom_line(data = hasil$uji, aes(tanggal, prediksi), color = "orange", linetype = "dashed") +
    geom_ribbon(data = hasil$ramalan, aes(tanggal, ymin = bawah, ymax = atas),
                fill = "steelblue", alpha = 0.25) +
    geom_line(data = hasil$ramalan, aes(tanggal, prediksi), color = "steelblue", linewidth = 1) +
    labs(title = judul,
         subtitle = paste0("Model terpilih: ", hasil$metode_terbaik,
                           " (abu = aktual, oranye = prediksi pada data uji, biru = ramalan + interval 95%)"),
         x = NULL, y = "Kasus per bulan") +
    theme_minimal()
}