cek_normal <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(unique(x)) < 3) return(NA_real_)
  shapiro.test(x)$p.value
}

# Memilih uji otomatis berdasarkan normalitas (Shapiro-Wilk) dan homogenitas varians (Fligner-Killeen)
#  2 kelompok : semua normal -> t-test Welch ; selain itu -> Mann-Whitney U
#  >2 kelompok: normal & homogen -> ANOVA ; selain itu -> Kruskal-Wallis
uji_beda <- function(nilai, kelompok, alpha = 0.05) {
  d <- tibble(nilai = nilai, kelompok = as.factor(kelompok)) |>
    filter(!is.na(nilai)) |>
    mutate(kelompok = droplevels(kelompok))
  k <- nlevels(d$kelompok)
  if (k < 2) stop("Dibutuhkan minimal dua kelompok.")
  if (any(table(d$kelompok) < 3)) stop("Setiap kelompok butuh minimal 3 pengamatan.")
  
  p_norm  <- tapply(d$nilai, d$kelompok, cek_normal)
  normal  <- all(!is.na(p_norm) & p_norm > alpha)
  p_var   <- tryCatch(fligner.test(nilai ~ kelompok, data = d)$p.value, error = function(e) NA_real_)
  homogen <- !is.na(p_var) && p_var > alpha
  
  if (k == 2) {
    if (normal) {
      h <- t.test(nilai ~ kelompok, data = d)
      metode <- "Independent Samples t-Test (Welch)"
    } else {
      h <- wilcox.test(nilai ~ kelompok, data = d, exact = FALSE)
      metode <- "Mann-Whitney U Test"
    }
    stat <- unname(h$statistic); p <- h$p.value
  } else if (normal && homogen) {
    a <- summary(aov(nilai ~ kelompok, data = d))[[1]]
    metode <- "ANOVA satu arah"; stat <- a$`F value`[1]; p <- a$`Pr(>F)`[1]
  } else {
    h <- kruskal.test(nilai ~ kelompok, data = d)
    metode <- "Kruskal-Wallis"; stat <- unname(h$statistic); p <- h$p.value
  }
  
  alasan <- if (k == 2) {
    if (normal) "semua kelompok berdistribusi normal" else "ada kelompok yang tidak normal"
  } else if (normal && homogen) {
    "semua kelompok normal dan variansnya homogen"
  } else "asumsi normalitas atau homogenitas varians tidak terpenuhi"
  
  list(
    ringkasan = tibble(metode = metode, statistik = stat, p_value = p,
                       signifikan = p < alpha, k_kelompok = k, n = nrow(d),
                       semua_normal = normal, p_homogenitas = p_var),
    normalitas = tibble(kelompok = names(p_norm), p_shapiro = as.numeric(p_norm)),
    interpretasi = paste0(
      metode, " dipilih karena ", alasan, ". ",
      if (p < alpha) "Terdapat perbedaan yang signifikan" else "Tidak terdapat perbedaan yang signifikan",
      " antarkelompok pada taraf signifikansi ", alpha, " (p = ", signif(p, 3), ")."
    )
  )
}