# Pakiety potrzebne do uruchomienia dashboardu Shiny oraz do regeneracji danych.
packages <- c(
  "shiny", "shinydashboard", "dplyr", "readr", "tidyr", "plotly",
  "DT", "scales", "stringr", "tibble", "htmltools",
  "janitor", "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata",
  "countrycode", "htmlwidgets", "httr2", "jsonlite"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) == 0) {
  message("Wszystkie wymagane pakiety są już zainstalowane.")
} else {
  install.packages(missing_packages, dependencies = TRUE)
}
  