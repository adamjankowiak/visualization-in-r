# Najprostszy start aplikacji z poziomu RStudio:
# source("run_app.R")

if (!requireNamespace("shiny", quietly = TRUE)) {
  source("install_packages.R")
}

shiny::runApp(appDir = ".", launch.browser = TRUE)

