statistik_deskriptif <- function(x) {
  x <- x[!is.na(x)]
  tab <- table(x)
  modus <- if (length(x) > 0 && max(tab) > 1) as.numeric(names(tab)[tab == max(tab)][1]) else NA_real_
  tibble(
    n = length(x), total = sum(x), rata2 = mean(x), median = median(x),
    modus = modus, minimum = min(x), maksimum = max(x),
    varians = var(x), sd = sd(x),
    kv_persen = sd(x) / mean(x) * 100,          # koefisien variasi
    indeks_dispersi = var(x) / mean(x)          # varians / mean (untuk cek Poisson)
  )
}

# Persen perubahan tiap periode terhadap periode sebelumnya (NA jika pembagi 0)
perubahan_periode <- function(x) {
  sebelum <- dplyr::lag(x)
  out <- (x - sebelum) / sebelum * 100
  out[!is.finite(out)] <- NA_real_
  out
}

# Persen perubahan periode terakhir vs sebelumnya (untuk kartu dashboard)
perubahan_terakhir <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 2 || x[n - 1] == 0) return(NA_real_)
  (x[n] - x[n - 1]) / x[n - 1] * 100
}