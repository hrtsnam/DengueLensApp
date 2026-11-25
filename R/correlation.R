uji_korelasi <- function(x, y) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  if (length(x) < 5) stop("Data terlalu sedikit untuk uji korelasi.")
  p <- cor.test(x, y, method = "pearson")
  s <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
  tibble(
    metode  = c("Pearson", "Spearman"),
    r       = c(unname(p$estimate), unname(s$estimate)),
    p_value = c(p$p.value, s$p.value),
    n       = length(x)
  )
}

# Hubungan kasus bulan ini dengan `lag` bulan sebelumnya (autokorelasi)
korelasi_antarperiode <- function(x, lag = 1) {
  uji_korelasi(head(x, -lag), tail(x, -lag)) |> mutate(lag = lag)
}

# Hubungan kasus dengan waktu (indikasi tren naik/turun)
korelasi_waktu <- function(tanggal, x) uji_korelasi(as.numeric(tanggal), x)