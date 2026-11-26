library(shiny)
library(bslib)
library(plotly)

for (f in c("db", "descriptive", "distribution", "correlation", "hypothesis", "forecast"))
  source(file.path("R", paste0(f, ".R")))

# ---------- Data dan konstanta ----------
prov_ref   <- baca_tabel("provinsi_ref")
kasus_prov <- baca_tabel("kasus_provinsi")
meta       <- baca_tabel("metadata")

akhir_nol <- kasus_prov |>
  arrange(provinsi, tanggal) |>
  group_by(provinsi) |>
  summarise(nol = sum(tail(kasus, 3)) == 0, .groups = "drop") |>
  filter(nol) |> pull(provinsi)

pilihan_semua    <- c("Nasional", sort(prov_ref$provinsi))
pilihan_prediksi <- c("Nasional",
                      sort(setdiff(prov_ref$provinsi[prov_ref$cukup_untuk_prediksi], akhir_nol)))

CACHE_FILE <- "data/clean/cache_prediksi.rds"
cache_prediksi <- if (file.exists(CACHE_FILE)) readRDS(CACHE_FILE) else list()

bln <- c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Agu", "Sep", "Okt", "Nov", "Des")
fmt <- function(x) format(round(x), big.mark = ".", decimal.mark = ",")

buang_belum_lengkap <- function(d) {
  k <- hitung_buang_akhir(d$kasus)
  if (k > 0) head(d, -k) else d
}

get_bersih <- function(w) {
  get_deret_wilayah(w) |> buang_belum_lengkap() |> filter(!diinterpolasi)
}

aman <- function(expr) {
  tryCatch(expr, error = function(e) validate(paste("Tidak dapat dihitung:", conditionMessage(e))))
}

# ---------- UI ----------
ui <- page_navbar(
  title = "DengueLens",
  id = "nav",
  fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  nav_panel("Beranda",
    card(
      card_header("Aplikasi Analisis dan Prediksi Tren Kasus Demam Berdarah Dengue (DBD)"),
      p("Aplikasi ini mengolah data historis kasus dengue di Indonesia (2015 sampai 2024) menjadi informasi
         statistik yang mudah dipahami: pola kasus berdasarkan waktu dan wilayah, perbandingan antarprovinsi,
         serta prediksi tren kasus untuk beberapa bulan ke depan."),
      tags$ul(
        tags$li("Dashboard: ringkasan kasus, grafik tren, dan pola musiman."),
        tags$li("Analisis: statistik deskriptif, distribusi probabilitas, korelasi, dan uji hipotesis."),
        tags$li("Prediksi: ramalan beberapa bulan ke depan beserta evaluasi kesalahan model."),
        tags$li("Informasi Dataset: sumber, periode, dan keterbatasan data.")
      ),
      p(strong("Catatan:"), "Aplikasi ini bukan alat diagnosis dan bukan sistem peringatan wabah resmi.
         Prediksi hanya menggambarkan kecenderungan statistik."),
      actionButton("mulai", "Buka Dashboard", class = "btn-primary")
    )
  ),

  nav_panel("Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("wil", "Wilayah", pilihan_semua, selected = "Nasional"),
        sliderInput("thn", "Rentang tahun", min = 2015, max = 2024, value = c(2015, 2024), sep = "")
      ),
      uiOutput("kartu"),
      card(card_header("Tren kasus bulanan"), plotlyOutput("tren")),
      card(card_header("Pola musiman: rata-rata kasus per bulan kalender"), plotlyOutput("musiman"))
    )
  ),

  nav_panel("Analisis",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("wil2", "Wilayah", pilihan_semua, selected = "Nasional"),
        sliderInput("thn2", "Rentang tahun", min = 2015, max = 2024, value = c(2015, 2024), sep = "")
      ),
      navset_card_tab(
        nav_panel("Statistik deskriptif",
          layout_columns(col_widths = c(5, 7),
            tableOutput("desk"), plotlyOutput("hist")),
          p(em("Indeks dispersi = varians / rata-rata. Nilai jauh di atas 1 menandakan variasi kasus
                jauh lebih besar daripada yang diharapkan distribusi Poisson."))
        ),
        nav_panel("Distribusi probabilitas",
          h5("Uji dispersi Poisson"), tableOutput("disp"), textOutput("disp_ket"),
          h5("Poisson vs Binomial Negatif (AIC/BIC lebih kecil = lebih cocok)"), tableOutput("distr"),
          p(em("Distribusi Binomial tidak diterapkan karena membutuhkan jumlah percobaan
                (misalnya jumlah penduduk) yang tidak tersedia di dataset."))
        ),
        nav_panel("Korelasi",
          tableOutput("kor"),
          p(em("Cara membaca: r mendekati +1 atau -1 berarti hubungan kuat, mendekati 0 berarti lemah.
                Korelasi dengan bulan sebelumnya yang tinggi menunjukkan kasus bergantung pada bulan lalu,
                sehingga model deret waktu masuk akal. Korelasi dengan waktu yang kecil berarti tidak ada
                tren naik atau turun yang konsisten; bila positif dan signifikan, ada kecenderungan naik
                meski lemah. P-value cenderung terlalu optimis karena data bulanan saling berkaitan."))
        ),
        nav_panel("Uji hipotesis (dua tahun)",
          div(class = "d-flex gap-3",
              selectInput("thnA", "Tahun A", 2015:2024, selected = 2022),
              selectInput("thnB", "Tahun B", 2015:2024, selected = 2023)),
          tableOutput("uji2"), textOutput("uji2_ket"),
          p(em("Data bulanan memiliki autokorelasi dan musiman, sehingga asumsi independensi tidak sepenuhnya
                terpenuhi dan p-value cenderung terlalu optimis."))
        ),
        nav_panel("Antarwilayah",
          plotlyOutput("antar", height = 800), tableOutput("antar_uji"), textOutput("antar_ket"),
          p(em("Kasus belum dibagi jumlah penduduk, sehingga provinsi berpenduduk besar cenderung unggul.
                Setiap provinsi dihitung sebagai satu pengamatan (rata-rata kasus bulanan).
                Empat provinsi Papua hasil pemekaran dikeluarkan karena datanya hampir seluruhnya nol."))
        )
      )
    )
  ),

  nav_panel("Prediksi",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        selectInput("wil3", "Wilayah", pilihan_prediksi, selected = "Nasional"),
        sliderInput("h", "Jumlah bulan diprediksi", min = 3, max = 12, value = 6),
        actionButton("hitung", "Tampilkan prediksi", class = "btn-primary"),
        hr(),
        helpText("Wilayah utama memakai hasil hitung lengkap (6 model). Wilayah lain dihitung ulang
                  dengan mode cepat (4 model) dan memerlukan beberapa detik."),
        helpText("Maluku dan provinsi Papua hasil pemekaran tidak tersedia karena data terakhirnya nol.")
      ),
      uiOutput("info_prediksi"),
      card(card_header("Data historis dan hasil prediksi"), plotOutput("plot_pred", height = 420)),
      layout_columns(col_widths = c(6, 6),
         card(card_header("Estimasi (interval 80%)"), tableOutput("tab_ramalan")),
         card(card_header("Evaluasi model (rolling-origin)"), tableOutput("tab_eval"))
      ),
      p(em("Ramalan menggambarkan kecenderungan statistik, bukan kepastian jumlah kasus. Galat prediksi
            masih besar karena lonjakan kasus tidak selalu dapat diperkirakan dari pola historis.
            MAPE dikosongkan bila ada nilai aktual nol. Gunakan sMAPE sebagai pembanding."))
    )
  ),

  nav_panel("Informasi Dataset",
    card(
      card_header("Sumber dan deskripsi data"),
      tableOutput("meta_tab")
    ),
    card(
      card_header("Variabel yang digunakan"),
      tags$ul(
        tags$li("adm_1_name: nama provinsi (diseragamkan menjadi 38 provinsi)"),
        tags$li("calendar_start_date / calendar_end_date: periode data (bulanan)"),
        tags$li("Year: tahun pencatatan"),
        tags$li("dengue_total: jumlah kasus dengue yang dilaporkan"),
        tags$li("T_res / S_res: resolusi waktu dan wilayah (dipakai hanya Month + Admin1 dan Month + Admin0)")
      )
    ),
    card(
      card_header("Keterbatasan data"),
      tags$ul(
        tags$li("Tidak ada data tingkat kabupaten/kota."),
        tags$li("Februari 2016 dan September 2020 tidak tersedia untuk provinsi (diisi interpolasi linear untuk analisis deret waktu)."),
        tags$li("Data provinsi berakhir Juli 2024 (Papua dan Papua Barat lebih awal); data nasional berakhir Oktober 2024."),
        tags$li("Bulan terakhir bisa belum lengkap karena keterlambatan pelaporan dan dikeluarkan otomatis dari analisis dan prediksi bila anjlok tidak wajar. Bulan hasil interpolasi hanya dipakai untuk prediksi, tidak dihitung dalam total dan statistik."),
        tags$li("Empat provinsi Papua hasil pemekaran hampir seluruhnya bernilai nol (belum ada pelaporan sendiri)."),
        tags$li("Kasus yang dilaporkan tidak menggambarkan seluruh infeksi sebenarnya, dan definisi kasus bisa berbeda antarwilayah.")
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  observeEvent(input$mulai, nav_select("nav", "Dashboard"))

  # ----- Dashboard -----
  d_dash <- reactive({
    get_bersih(input$wil) |> filter(tahun >= input$thn[1], tahun <= input$thn[2])
  })

  output$kartu <- renderUI({
    d <- d_dash(); req(nrow(d) > 0)
    x <- d$kasus
    k <- hitung_buang_akhir(x)
    xl <- if (k > 0) head(x, -k) else x
    chg <- perubahan_terakhir(xl)
    bln_akhir <- format(d$tanggal[length(xl)], "%Y-%m")
    top <- kasus_prov |>
      filter(tahun >= input$thn[1], tahun <= input$thn[2]) |>
      group_by(provinsi) |> summarise(t = sum(kasus), .groups = "drop") |>
      slice_max(t, n = 1, with_ties = FALSE)

    layout_columns(fill = FALSE,
      value_box("Total kasus", fmt(sum(x)), p(paste(min(d$tahun), "-", max(d$tahun))), theme = "primary"),
      value_box("Rata-rata per bulan", fmt(mean(x)), p("kasus per bulan"), theme = "info"),
      value_box("Perubahan bulan terakhir",
                if (is.na(chg)) "-" else sprintf("%+.1f%%", chg),
                p(paste("pada", bln_akhir)), theme = if (!is.na(chg) && chg > 0) "danger" else "success"),
      value_box("Provinsi kasus tertinggi", top$provinsi, p(paste(fmt(top$t), "kasus (nasional)")), theme = "secondary")
    )
  })

  output$tren <- renderPlotly({
    d <- d_dash(); req(nrow(d) > 0)
    plot_ly(d, x = ~tanggal, y = ~kasus, type = "scatter", mode = "lines",
            line = list(color = "#2C3E50")) |>
      layout(title = paste("Kasus DBD bulanan:", input$wil),
             xaxis = list(title = "Bulan"), yaxis = list(title = "Jumlah kasus"))
  })

  output$musiman <- renderPlotly({
    d <- d_dash(); req(nrow(d) > 0)
    m <- d |> group_by(bulan) |> summarise(rata2 = mean(kasus), .groups = "drop") |>
      mutate(nama = factor(bln[bulan], levels = bln))
    plot_ly(m, x = ~nama, y = ~rata2, type = "bar", marker = list(color = "#18BC9C")) |>
      layout(xaxis = list(title = "Bulan"), yaxis = list(title = "Rata-rata kasus"))
  })

  # ----- Analisis -----
  d2 <- reactive({
    get_bersih(input$wil2) |> filter(tahun >= input$thn2[1], tahun <= input$thn2[2])
  })

  output$desk <- renderTable({
    aman({
      statistik_deskriptif(d2()$kasus) |>
        mutate(across(everything(), as.numeric)) |>
        pivot_longer(everything(), names_to = "Ukuran", values_to = "Nilai") |>
        mutate(Ukuran = recode(Ukuran, n = "Jumlah bulan", total = "Total kasus",
                               rata2 = "Rata-rata", median = "Median", modus = "Modus",
                               minimum = "Minimum", maksimum = "Maksimum", varians = "Varians",
                               sd = "Standar deviasi", kv_persen = "Koefisien variasi (%)",
                               indeks_dispersi = "Indeks dispersi"),
               Nilai = format(round(Nilai, 2), big.mark = ".", decimal.mark = ",", trim = TRUE))
    })
  })

  output$hist <- renderPlotly({
    d <- d2(); req(nrow(d) > 0)
    plot_ly(d, x = ~kasus, type = "histogram", marker = list(color = "#3498DB")) |>
      layout(title = "Distribusi kasus bulanan", xaxis = list(title = "Kasus per bulan"),
             yaxis = list(title = "Jumlah bulan"))
  })

  output$disp <- renderTable({
    aman({
      uji_dispersi_poisson(d2()$kasus) |>
        transmute(n, rata2 = round(rata2, 1), varians = round(varians, 1),
                  indeks_dispersi = round(indeks_dispersi, 1),
                  p_value = ifelse(p_value < 2.2e-16, "< 2.2e-16", format(p_value, digits = 3, scientific = TRUE)))
    })
  })
  output$disp_ket <- renderText({ aman(uji_dispersi_poisson(d2()$kasus)$kesimpulan) })

  output$distr <- renderTable({
    aman({
      bandingkan_distribusi(d2()$kasus)$tabel |> mutate(aic = round(aic, 1), bic = round(bic, 1))
    })
  })

  output$kor <- renderTable({
    aman({
      x <- d2()$kasus
      bind_rows(
        korelasi_antarperiode(x, 1) |> mutate(hubungan = "Kasus vs bulan sebelumnya") |> select(-lag),
        korelasi_waktu(d2()$tanggal, x) |> mutate(hubungan = "Kasus vs waktu")
      ) |>
        transmute(hubungan, metode, r = round(r, 3), p_value = format(p_value, digits = 3, scientific = TRUE), n)
    })
  })

  uji2 <- reactive({
    aman({
      a <- as.integer(input$thnA); b <- as.integer(input$thnB)
      if (a == b) stop("Pilih dua tahun yang berbeda.")
      d <- get_bersih(input$wil2) |> filter(tahun %in% c(a, b))
      uji_beda(d$kasus, d$tahun)
    })
  })
  output$uji2 <- renderTable({
    uji2()$ringkasan |>
      transmute(metode, statistik = round(statistik, 2),
                p_value = format(p_value, digits = 3, scientific = FALSE),
                signifikan = ifelse(signifikan, "Ya", "Tidak"), n)
  })
  output$uji2_ket <- renderText(uji2()$interpretasi)

  antar <- reactive({
    kasus_prov |>
      filter(tahun >= input$thn2[1], tahun <= input$thn2[2]) |>
      inner_join(prov_ref |> filter(cukup_untuk_prediksi) |> select(provinsi, pulau), by = "provinsi") |>
      group_by(provinsi, pulau) |>
      summarise(rata2 = mean(kasus), .groups = "drop") |>
      arrange(desc(rata2))
  })
  output$antar <- renderPlotly({
    a <- antar(); req(nrow(a) > 0)
    plot_ly(a, x = ~rata2, y = ~reorder(provinsi, rata2), color = ~pulau, type = "bar", orientation = "h") |>
      layout(title = "Rata-rata kasus bulanan per provinsi",
             xaxis = list(title = "Rata-rata kasus per bulan"), yaxis = list(title = ""))
  })
  antar_h <- reactive({ aman(uji_beda(antar()$rata2, antar()$pulau)) })
  output$antar_uji <- renderTable({
    antar_h()$ringkasan |>
      transmute(metode, statistik = round(statistik, 2),
                p_value = format(p_value, digits = 3, scientific = FALSE),
                signifikan = ifelse(signifikan, "Ya", "Tidak"), n_provinsi = n)
  })
  output$antar_ket <- renderText(antar_h()$interpretasi)

  # ----- Prediksi -----
  hasil <- eventReactive(input$hitung, {
    w <- input$wil3
    if (!is.null(cache_prediksi[[w]])) {
      r <- cache_prediksi[[w]]; r$mode <- "lengkap (6 model, 5 jendela uji)"
      return(r)
    }
    withProgress(message = "Menghitung prediksi (mode cepat)...", value = 0.5, {
      r <- jalankan_prediksi(get_deret_wilayah(w), h = 12, n_fold = 3,
                             daftar = c("Seasonal Naive", "MA3", "MA12", "SES"))
      r$mode <- "cepat (4 model, 3 jendela uji)"
      r
    })
  })

  hasil_h <- reactive({
    r <- hasil(); req(r)
    r$ramalan <- head(r$ramalan, input$h)
    r
  })

  output$info_prediksi <- renderUI({
    r <- hasil_h()
    card(p(strong("Wilayah: "), isolate(input$wil3), " | ", strong("Model terpilih: "), r$metode_terbaik,
           " | ", strong("Mode hitung: "), r$mode,
           " | ", strong("Bulan terakhir dibuang: "), r$buang_akhir,
           " | ", strong("Data terakhir: "), format(max(r$data$tanggal), "%Y-%m")))
  })

  output$plot_pred <- renderPlot({
    r <- hasil_h()
    plot_prediksi(r, paste("Prediksi kasus DBD:", isolate(input$wil3))) + theme_minimal(base_size = 15)
  })

  output$tab_ramalan <- renderTable({
    hasil_h()$ramalan |>
      transmute(Bulan = format(tanggal, "%Y-%m"), Estimasi = fmt(prediksi),
                Bawah = fmt(bawah), Atas = fmt(atas))
  })

  output$tab_eval <- renderTable({
    hasil_h()$evaluasi |>
      mutate(across(where(is.numeric), \(v) round(v, 1)))
  })

  # ----- Informasi dataset -----
  output$meta_tab <- renderTable({
    meta |> rename(Keterangan = kunci, Nilai = nilai)
  })
}

shinyApp(ui, server)
