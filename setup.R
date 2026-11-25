install.packages(c(
  "shiny", "bslib", "plotly", "DT", "shinycssloaders",
  "tidyverse", "lubridate", "janitor",
  "forecast", "tseries", "fitdistrplus",
  "DBI", "RSQLite",
  "rmarkdown", "knitr", "testthat", "renv"
))

# struktur folder
for (d in c("R", "modules", "data/raw", "data/clean", "report", "tests")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
