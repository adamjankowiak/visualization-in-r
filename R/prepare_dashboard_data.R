# ============================================================
# Regeneracja danych dashboardu z surowego katalogu IKEA
# ============================================================
# Plik jest bezpieczny dla Shiny: po source() tylko definiuje funkcję.
# Shiny automatycznie wykonuje source() dla plików z katalogu R/,
# więc nie wolno tutaj automatycznie uruchamiać project1.
#
# Użycie ręczne z katalogu głównego projektu:
#   source("R/prepare_dashboard_data.R")
#   prepare_dashboard_data()
# ============================================================

prepare_dashboard_data <- function(
  project_root = getwd(),
  catalog_path = file.path("data", "IKEA_product_catalog.csv"),
  project_script = file.path("R", "project1_country_names_pl.R")
) {
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)

  required_packages <- c(
    "readr", "dplyr", "tidyr", "janitor", "stringr",
    "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata",
    "countrycode", "plotly", "htmlwidgets", "scales", "httr2", "jsonlite"
  )

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "Brakuje pakietów: ", paste(missing_packages, collapse = ", "),
        "\nZainstaluj je poleceniem:\nsource(\"install_packages.R\")"
      ),
      call. = FALSE
    )
  }

  catalog_full_path <- file.path(project_root, catalog_path)
  project_script_full_path <- file.path(project_root, project_script)

  if (!file.exists(catalog_full_path)) {
    stop(
      paste0(
        "Brak pliku: ", catalog_full_path,
        "\nDodaj surowy dataset IKEA jako data/IKEA_product_catalog.csv."
      ),
      call. = FALSE
    )
  }

  if (!file.exists(project_script_full_path)) {
    stop(
      paste0("Brak pliku: ", project_script_full_path),
      call. = FALSE
    )
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)

  pipeline_env <- new.env(parent = globalenv())
  source(project_script_full_path, local = pipeline_env)

  if (!exists("run_project1_country_names_pl", envir = pipeline_env, inherits = FALSE)) {
    stop(
      paste0(
        "Plik ", project_script_full_path,
        " nie definiuje funkcji run_project1_country_names_pl()."
      ),
      call. = FALSE
    )
  }

  result <- pipeline_env$run_project1_country_names_pl(project_root = project_root)

  expected_outputs <- c(
    file.path(project_root, "data", "ikea_country_price_statistics_eur.csv"),
    file.path(project_root, "data", "ikea_country_price_statistics_ppp.csv"),
    file.path(project_root, "data", "ikea_ppp_top_15_countries_diagnostics.csv"),
    file.path(project_root, "data", "ppp_conversion_factors_private_consumption.csv"),
    file.path(project_root, "www", "ikea_interactive_choropleth_price_map_eur.html"),
    file.path(project_root, "www", "ikea_price_ranking_plotly_eur.html"),
    file.path(project_root, "www", "ikea_static_price_map_ggplot_sf_eur.png"),
    file.path(project_root, "www", "ikea_static_price_map_ggplot_sf_ppp.png")
  )

  missing_outputs <- expected_outputs[!file.exists(expected_outputs)]

  if (length(missing_outputs) > 0) {
    warning(
      paste0(
        "Regeneracja zakończona, ale nie znaleziono części oczekiwanych plików:\n",
        paste(missing_outputs, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  message("Regeneracja zakończona. Uruchom aplikację: shiny::runApp()")
  invisible(result)
}
