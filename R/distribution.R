# Uji dispersi: H0 = data Poisson (varians = mean). Alternatif: overdispersi.
uji_dispersi_poisson <- function(x, alpha = 0.05) {
  x <- x[!is.na(x)]; n <- length(x); m <- mean(x); v <- var(x)
  if (n < 5 || m == 0) stop("Data terlalu sedikit atau seluruhnya nol.")
  stat <- (n - 1) * v / m
  p <- pchisq(stat, df = n - 1, lower.tail = FALSE)
  tibble(
    n = n, rata2 = m, varians = v, indeks_dispersi = v / m,
    chi2 = stat, df = n - 1, p_value = p,
    kesimpulan = if (p < alpha)
      "Overdispersi: varians jauh melebihi rata-rata, sehingga distribusi Poisson tidak cocok."
    else "Tidak ada bukti overdispersi: distribusi Poisson masih masuk akal."
  )
}

# Bandingkan kecocokan Poisson vs Binomial Negatif (AIC/BIC lebih kecil = lebih baik)
bandingkan_distribusi <- function(x) {
  x <- round(x[!is.na(x)])
  if (length(x) < 10 || mean(x) == 0) stop("Data terlalu sedikit atau seluruhnya nol.")
  f_pois <- fitdistrplus::fitdist(x, "pois")
  f_nb   <- tryCatch(fitdistrplus::fitdist(x, "nbinom"), error = function(e) NULL)
  
  tabel <- tibble(
    model = "Poisson", aic = f_pois$aic, bic = f_pois$bic,
    parameter = paste0("lambda = ", round(f_pois$estimate["lambda"], 2))
  )
  if (!is.null(f_nb)) {
    tabel <- bind_rows(tabel, tibble(
      model = "Binomial Negatif", aic = f_nb$aic, bic = f_nb$bic,
      parameter = paste0("size = ", round(f_nb$estimate["size"], 2),
                         ", mu = ", round(f_nb$estimate["mu"], 2))
    ))
  }
  list(tabel = tabel |> arrange(aic), fit_pois = f_pois, fit_nb = f_nb)
}