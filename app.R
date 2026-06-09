# ============================================================
# IKEA Price Explorer — dashboard Shiny
# Autor projektu: wygenerowane na podstawie project1_country_names_pl.R
# Dane wejściowe: eksporty CSV i mapy wygenerowane w project1
# ============================================================

required_packages <- c(
  "shiny", "shinydashboard", "dplyr", "readr", "tidyr", "plotly",
  "DT", "scales", "stringr", "tibble", "htmltools"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Brakuje pakietów R: ", paste(missing_packages, collapse = ", "),
      "\nZainstaluj je poleceniem:\ninstall.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

lapply(required_packages, library, character.only = TRUE)

# ============================================================
# 1. Import danych dashboardowych
# ============================================================

data_dir <- "data"
www_dir <- "www"

eur_path <- file.path(data_dir, "ikea_country_price_statistics_eur.csv")
ppp_path <- file.path(data_dir, "ikea_country_price_statistics_ppp.csv")
ppp_diag_path <- file.path(data_dir, "ikea_ppp_top_15_countries_diagnostics.csv")
ppp_factors_path <- file.path(data_dir, "ppp_conversion_factors_private_consumption.csv")
raw_catalog_path <- file.path(data_dir, "IKEA_product_catalog.csv")

stop_if_missing <- function(path) {
  if (!file.exists(path)) {
    stop("Nie znaleziono pliku: ", path, call. = FALSE)
  }
}

invisible(lapply(c(eur_path, ppp_path, ppp_diag_path, ppp_factors_path), stop_if_missing))

country_eur <- readr::read_csv(eur_path, show_col_types = FALSE)
country_ppp <- readr::read_csv(ppp_path, show_col_types = FALSE)
ppp_diag <- readr::read_csv(ppp_diag_path, show_col_types = FALSE)
ppp_factors <- readr::read_csv(ppp_factors_path, show_col_types = FALSE)

ppp_slim <- country_ppp |>
  dplyr::select(
    country_clean, country_pl, iso3,
    median_price_ppp, mean_price_ppp, min_price_ppp, q1_price_ppp,
    q3_price_ppp, max_price_ppp, iqr_price_ppp, ppp_year,
    price_range_ppp, price_index_ppp, compared_products_ppp
  )

country_stats <- country_eur |>
  dplyr::left_join(
    ppp_slim,
    by = c("country_clean", "country_pl", "iso3")
  ) |>
  dplyr::mutate(
    country_label = paste0(country_pl, " (", iso3, ")"),
    price_position_eur = dplyr::case_when(
      price_index >= 110 ~ "drożej niż globalna mediana",
      price_index <= 90 ~ "taniej niż globalna mediana",
      TRUE ~ "blisko globalnej mediany"
    ),
    price_position_ppp = dplyr::case_when(
      price_index_ppp >= 110 ~ "drożej po korekcie PPP",
      price_index_ppp <= 90 ~ "taniej po korekcie PPP",
      TRUE ~ "blisko globalnej mediany PPP"
    )
  ) |>
  dplyr::arrange(country_pl)

has_raw_catalog <- file.exists(raw_catalog_path)

raw_catalog_preview <- NULL
if (has_raw_catalog) {
  raw_catalog_preview <- readr::read_csv(raw_catalog_path, show_col_types = FALSE, n_max = 10000)
}

metric_catalog <- tibble::tribble(
  ~metric_id,              ~metric_label,                         ~unit,            ~digits, ~higher_is,
  "median_price_eur",      "Mediana ceny w EUR",                  "EUR",           1,       "drożej",
  "mean_price_eur",        "Średnia cena w EUR",                  "EUR",           1,       "drożej",
  "price_index",           "Indeks cenowy tych samych produktów", "pkt",           1,       "drożej",
  "median_price_ppp",      "Mediana ceny PPP",                    "intl. $ PPP",   1,       "drożej",
  "mean_price_ppp",        "Średnia cena PPP",                    "intl. $ PPP",   1,       "drożej",
  "price_index_ppp",       "Indeks cenowy PPP",                   "pkt",           1,       "drożej"
)

metric_choices <- stats::setNames(metric_catalog$metric_id, metric_catalog$metric_label)
country_choices <- country_stats$country_pl
names(country_choices) <- country_stats$country_label

default_country <- if ("Polska" %in% country_stats$country_pl) "Polska" else country_stats$country_pl[1]

get_metric_meta <- function(metric_id, field) {
  value <- metric_catalog[[field]][match(metric_id, metric_catalog$metric_id)]
  if (length(value) == 0 || is.na(value)) return(NA_character_)
  value
}

format_metric <- function(x, metric_id) {
  unit <- get_metric_meta(metric_id, "unit")
  digits <- as.integer(get_metric_meta(metric_id, "digits"))
  if (is.na(digits)) digits <- 1

  ifelse(
    is.na(x),
    "brak danych",
    paste0(
      scales::number(x, accuracy = 10^(-digits), big.mark = " ", decimal.mark = ","),
      ifelse(is.na(unit) || unit == "", "", paste0(" ", unit))
    )
  )
}

format_plain <- function(x, accuracy = 1) {
  scales::number(x, accuracy = accuracy, big.mark = " ", decimal.mark = ",")
}

# ============================================================
# 2. UI
# ============================================================

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(
    title = span("IKEA Price Explorer", style = "font-weight: 700;")
  ),
  shinydashboard::dashboardSidebar(
    width = 310,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Panel główny", tabName = "overview", icon = icon("chart-line")),
      shinydashboard::menuItem("Analiza kraju", tabName = "country", icon = icon("flag")),
      shinydashboard::menuItem("O projekcie", tabName = "about", icon = icon("info-circle"))
    ),
    tags$hr(),
    selectInput(
      inputId = "metric",
      label = "Metryka na mapie i rankingach",
      choices = metric_choices,
      selected = "median_price_eur"
    ),
    selectizeInput(
      inputId = "countries",
      label = "Filtr krajów",
      choices = country_choices,
      multiple = TRUE,
      selected = character(0),
      options = list(
        plugins = list("remove_button"),
        placeholder = "Puste = wszystkie kraje"
      )
    ),
    sliderInput(
      inputId = "top_n",
      label = "Liczba krajów w rankingu",
      min = 5,
      max = min(30, nrow(country_stats)),
      value = 15,
      step = 1
    ),
    radioButtons(
      inputId = "rank_order",
      label = "Kierunek rankingu",
      choices = c("Najwyższe wartości" = "desc", "Najniższe wartości" = "asc"),
      selected = "desc"
    ),
    checkboxInput(
      inputId = "log_map",
      label = "Skala log10 na mapie",
      value = FALSE
    ),
    tags$hr(),
    downloadButton("download_filtered", "Pobierz dane filtrowane")
  ),
  shinydashboard::dashboardBody(
    tags$head(
      tags$style(HTML("\n        .content-wrapper, .right-side { background-color: #f4f6f9; }\n        .box { border-top: 3px solid #2454A6; }\n        .small-box h3 { font-size: 32px; }\n        .small-box p { font-size: 15px; }\n        .note-card { padding: 12px 14px; background: #ffffff; border-left: 4px solid #2454A6; margin-bottom: 12px; }\n        .project-iframe { border: 0; width: 100%; min-height: 680px; background: white; }\n        .static-map-img { width: 100%; max-width: 100%; border: 1px solid #ddd; background: white; }\n        .control-label { font-weight: 600; }\n        .shiny-output-error { color: #B00020; }\n      "))
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "overview",
        fluidRow(
          shinydashboard::valueBoxOutput("vbox_countries", width = 3),
          shinydashboard::valueBoxOutput("vbox_products", width = 3),
          shinydashboard::valueBoxOutput("vbox_median_eur", width = 3),
          shinydashboard::valueBoxOutput("vbox_median_ppp", width = 3)
        ),
        fluidRow(
          shinydashboard::box(
            width = 8,
            title = "Interaktywna mapa choropletyczna",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("map_dynamic", height = "620px")
          ),
          shinydashboard::box(
            width = 4,
            title = "Ranking krajów",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("rank_plot", height = "620px")
          )
        ),
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Szybki podgląd krajów po filtrach",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("country_snapshot")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "country",
        fluidRow(
          shinydashboard::box(
            width = 4,
            title = "Wybór kraju",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              inputId = "country_detail",
              label = "Kraj do analizy",
              choices = country_choices,
              selected = default_country
            ),
            uiOutput("country_note")
          ),
          shinydashboard::box(
            width = 4,
            title = "Indeks cenowy EUR",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("gauge_eur", height = "260px")
          ),
          shinydashboard::box(
            width = 4,
            title = "Indeks cenowy PPP",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("gauge_ppp", height = "260px")
          )
        ),
        fluidRow(
          shinydashboard::box(
            width = 7,
            title = "Profil cenowy kraju",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("country_profile_plot", height = "430px")
          ),
          shinydashboard::box(
            width = 5,
            title = "Parametry kraju",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("country_detail_table")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "about",
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Założenia projektu",
            status = "primary",
            solidHeader = TRUE,
            div(
              class = "note-card",
              tags$b("Cel projektu: "),
              "aplikacja Shiny umożliwia eksplorację różnic cen produktów IKEA między krajami. Porównania są prezentowane w EUR oraz po korekcie parytetem siły nabywczej PPP, dzięki czemu można zestawić nominalne ceny z ich ujęciem względnym."
            ),
            div(
              class = "note-card",
              tags$b("Zakres danych: "),
              "dashboard korzysta z przygotowanych plików CSV zawierających statystyki cenowe dla krajów, indeksy cenowe, metryki PPP, liczebność produktów, informacje o walutach oraz podstawowe charakterystyki katalogu."
            ),
            div(
              class = "note-card",
              tags$b("Funkcje panelu głównego: "),
              "użytkownik może wybrać metrykę, filtrować kraje, zmienić kierunek i długość rankingu, włączyć skalę log10 na mapie oraz pobrać przefiltrowany zestaw danych do pliku CSV."
            ),
            div(
              class = "note-card",
              tags$b("Wizualizacje: "),
              "aplikacja pokazuje interaktywną mapę choropletyczną, ranking krajów, liczniki podsumowujące bieżący widok, tabelę z filtrowanymi danymi oraz wykresy profilu wybranego kraju."
            ),
            div(
              class = "note-card",
              tags$b("Analiza kraju: "),
              "zakładka szczegółowa prezentuje wybrany kraj przez indeks cenowy EUR, indeks cenowy PPP, podstawowe parametry katalogu oraz zestawienie median, średnich i indeksów na jednym profilu cenowym."
            ),
            h4("Struktura plików"),
            tags$pre("app.R\ndata/\n  ikea_country_price_statistics_eur.csv\n  ikea_country_price_statistics_ppp.csv\n  ikea_ppp_top_15_countries_diagnostics.csv\n  ppp_conversion_factors_private_consumption.csv\nwww/\n  ikea_interactive_choropleth_price_map_eur.html\n  ikea_price_ranking_plotly_eur.html\n  ikea_static_price_map_ggplot_sf_eur.png\n  ikea_static_price_map_ggplot_sf_ppp.png\nR/\n  project1_country_names_pl.R\n  prepare_dashboard_data.R")
          )
        )
      )
    )
  )
)

# ============================================================
# 3. Server
# ============================================================

server <- function(input, output, session) {
  filtered_countries <- reactive({
    selected <- input$countries
    df <- country_stats

    if (!is.null(selected) && length(selected) > 0) {
      df <- df |>
        dplyr::filter(country_pl %in% selected)
    }

    df
  })

  metric_df <- reactive({
    metric_id <- input$metric
    metric_label <- get_metric_meta(metric_id, "metric_label")

    filtered_countries() |>
      dplyr::mutate(
        metric_value = .data[[metric_id]],
        metric_label = metric_label,
        metric_pretty = format_metric(metric_value, metric_id),
        hover_text = paste0(
          "<b>", country_pl, "</b>",
          "<br>Metryka: ", metric_label,
          "<br>Wartość: ", metric_pretty,
          "<br>Waluta lokalna: ", currencies,
          "<br>Rekordy: ", scales::comma(n_records, big.mark = " "),
          "<br>Produkty: ", scales::comma(n_products, big.mark = " "),
          "<br>Kategorie główne: ", n_main_categories,
          "<br>Podkategorie: ", n_sub_categories,
          "<br>Indeks cenowy: ", format_metric(price_index, "price_index"),
          "<br>Indeks PPP: ", format_metric(price_index_ppp, "price_index_ppp")
        )
      ) |>
      dplyr::filter(!is.na(metric_value))
  })

  output$vbox_countries <- shinydashboard::renderValueBox({
    df <- filtered_countries()
    shinydashboard::valueBox(
      value = scales::comma(nrow(df), big.mark = " "),
      subtitle = "krajów w bieżącym widoku",
      icon = icon("globe"),
      color = "aqua"
    )
  })

  output$vbox_products <- shinydashboard::renderValueBox({
    df <- filtered_countries()
    shinydashboard::valueBox(
      value = scales::comma(sum(df$n_products, na.rm = TRUE), big.mark = " "),
      subtitle = "unikalnych produktów łącznie",
      icon = icon("couch"),
      color = "blue"
    )
  })

  output$vbox_median_eur <- shinydashboard::renderValueBox({
    df <- filtered_countries()
    shinydashboard::valueBox(
      value = paste0(format_plain(stats::median(df$median_price_eur, na.rm = TRUE), 0.1), " EUR"),
      subtitle = "mediana median krajowych EUR",
      icon = icon("euro-sign"),
      color = "green"
    )
  })

  output$vbox_median_ppp <- shinydashboard::renderValueBox({
    df <- filtered_countries()
    shinydashboard::valueBox(
      value = paste0(format_plain(stats::median(df$median_price_ppp, na.rm = TRUE), 0.1), " intl. $"),
      subtitle = "mediana median krajowych PPP",
      icon = icon("balance-scale"),
      color = "yellow"
    )
  })

  output$map_dynamic <- plotly::renderPlotly({
    df <- metric_df()
    req(nrow(df) > 0)

    metric_id <- input$metric
    metric_label <- get_metric_meta(metric_id, "metric_label")

    use_log <- isTRUE(input$log_map) && all(df$metric_value > 0, na.rm = TRUE)
    df <- df |>
      dplyr::mutate(
        map_value = if (use_log) log10(metric_value) else metric_value
      )

    color_title <- if (use_log) paste0("log10(", metric_label, ")") else metric_label

    plotly::plot_ly(
      data = df,
      type = "choropleth",
      locations = ~iso3,
      z = ~map_value,
      text = ~hover_text,
      hoverinfo = "text",
      colorscale = "Viridis",
      marker = list(line = list(color = "white", width = 0.5)),
      colorbar = list(title = color_title)
    ) |>
      plotly::layout(
        title = list(
          text = paste0("Rozkład cen produktów IKEA — ", metric_label),
          x = 0.02,
          xanchor = "left"
        ),
        geo = list(
          projection = list(type = "natural earth"),
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = "gray70",
          landcolor = "gray95",
          bgcolor = "rgba(0,0,0,0)"
        ),
        margin = list(l = 0, r = 10, t = 70, b = 0)
      ) |>
      plotly::config(displayModeBar = TRUE, responsive = TRUE)
  })

  output$rank_plot <- plotly::renderPlotly({
    df <- metric_df()
    req(nrow(df) > 0)

    if (input$rank_order == "desc") {
      df <- df |>
        dplyr::arrange(dplyr::desc(metric_value))
    } else {
      df <- df |>
        dplyr::arrange(metric_value)
    }

    df <- df |>
      dplyr::slice_head(n = input$top_n) |>
      dplyr::arrange(metric_value) |>
      dplyr::mutate(country_pl = factor(country_pl, levels = country_pl))

    metric_label <- get_metric_meta(input$metric, "metric_label")

    plotly::plot_ly(
      data = df,
      x = ~metric_value,
      y = ~country_pl,
      type = "bar",
      orientation = "h",
      text = ~hover_text,
      hoverinfo = "text"
    ) |>
      plotly::layout(
        title = paste0("Ranking: ", metric_label),
        xaxis = list(title = metric_label),
        yaxis = list(title = ""),
        margin = list(l = 120, r = 20, t = 60, b = 50)
      ) |>
      plotly::config(displayModeBar = TRUE, responsive = TRUE)
  })

  output$country_snapshot <- DT::renderDT({
    df <- metric_df() |>
      dplyr::select(
        Kraj = country_pl,
        ISO3 = iso3,
        Metryka = metric_pretty,
        `Mediana EUR` = median_price_eur,
        `Mediana PPP` = median_price_ppp,
        `Indeks EUR` = price_index,
        `Produkty` = n_products,
        Waluty = currencies
      )

    DT::datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 10, scrollX = TRUE)
    ) |>
      DT::formatRound(columns = c("Mediana EUR", "Mediana PPP", "Indeks EUR"), digits = 1)
  })

  selected_country_df <- reactive({
    req(input$country_detail)
    country_stats |>
      dplyr::filter(country_pl == input$country_detail) |>
      dplyr::slice_head(n = 1)
  })

  output$country_note <- renderUI({
    df <- selected_country_df()
    req(nrow(df) == 1)

    HTML(paste0(
      "<div class='note-card'>",
      "<b>", df$country_pl, "</b><br>",
      "ISO3: ", df$iso3, "<br>",
      "Waluta w katalogu: ", df$currencies, "<br>",
      "Rekordy: ", scales::comma(df$n_records, big.mark = " "), "<br>",
      "Produkty: ", scales::comma(df$n_products, big.mark = " "), "<br>",
      "Pozycja EUR: ", df$price_position_eur, "<br>",
      "Pozycja PPP: ", df$price_position_ppp,
      "</div>"
    ))
  })

  render_index_gauge <- function(value, title) {
    value <- ifelse(is.na(value), 0, value)
    axis_max <- max(180, ceiling(value / 10) * 10)

    plotly::plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = value,
      number = list(suffix = " pkt"),
      delta = list(reference = 100,
                   increasing = list(color = "red"),
                   decreasing = list(color = "green")),
      title = list(text = title),
      gauge = list(
        axis = list(range = list(NULL, axis_max)),
        threshold = list(line = list(width = 3), thickness = 0.75, value = 100),
        steps = list(
          list(range = c(0, 90)),
          list(range = c(90, 110)),
          list(range = c(110, axis_max))
        )
      )
    ) |>
      plotly::layout(margin = list(l = 20, r = 20, t = 50, b = 20)) |>
      plotly::config(displayModeBar = FALSE)
  }

  output$gauge_eur <- plotly::renderPlotly({
    df <- selected_country_df()
    req(nrow(df) == 1)
    render_index_gauge(df$price_index, "100 = globalna mediana")
  })

  output$gauge_ppp <- plotly::renderPlotly({
    df <- selected_country_df()
    req(nrow(df) == 1)
    render_index_gauge(df$price_index_ppp, "100 = globalna mediana PPP")
  })

  output$country_profile_plot <- plotly::renderPlotly({
    df <- selected_country_df()
    req(nrow(df) == 1)

    profile <- tibble::tibble(
      metric = c("Mediana EUR", "Średnia EUR", "Mediana PPP", "Średnia PPP", "Indeks EUR", "Indeks PPP"),
      value = c(
        df$median_price_eur,
        df$mean_price_eur,
        df$median_price_ppp,
        df$mean_price_ppp,
        df$price_index,
        df$price_index_ppp
      ),
      group = c("EUR", "EUR", "PPP", "PPP", "Indeks", "Indeks")
    )

    plotly::plot_ly(
      data = profile,
      x = ~metric,
      y = ~value,
      type = "bar",
      text = ~scales::number(value, accuracy = 0.1, big.mark = " "),
      hovertemplate = paste("%{x}: %{y:.2f}<extra></extra>")
    ) |>
      plotly::layout(
        title = paste0("Profil cenowy: ", df$country_pl),
        xaxis = list(title = ""),
        yaxis = list(title = "Wartość"),
        margin = list(l = 70, r = 30, t = 70, b = 80)
      ) |>
      plotly::config(displayModeBar = TRUE, responsive = TRUE)
  })

  output$country_detail_table <- DT::renderDT({
    df <- selected_country_df()
    req(nrow(df) == 1)

    details <- tibble::tribble(
      ~Parametr, ~Wartość,
      "Kraj", as.character(df$country_pl),
      "ISO3", as.character(df$iso3),
      "Waluty", as.character(df$currencies),
      "Liczba rekordów", scales::comma(df$n_records, big.mark = " "),
      "Liczba produktów", scales::comma(df$n_products, big.mark = " "),
      "Kategorie główne", scales::comma(df$n_main_categories, big.mark = " "),
      "Podkategorie", scales::comma(df$n_sub_categories, big.mark = " "),
      "Mediana ceny EUR", format_metric(df$median_price_eur, "median_price_eur"),
      "Średnia cena EUR", format_metric(df$mean_price_eur, "mean_price_eur"),
      "Zakres cen EUR", format_metric(df$price_range_eur, "median_price_eur"),
      "Indeks EUR", format_metric(df$price_index, "price_index"),
      "Mediana ceny PPP", format_metric(df$median_price_ppp, "median_price_ppp"),
      "Średnia cena PPP", format_metric(df$mean_price_ppp, "mean_price_ppp"),
      "Indeks PPP", format_metric(df$price_index_ppp, "price_index_ppp"),
      "Rok PPP", as.character(df$ppp_year),
      "Średnia ocena", format_metric(df$mean_rating, "mean_rating")
    )

    DT::datatable(
      details,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE),
      escape = FALSE
    )
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("ikea_dashboard_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(metric_df(), file)
    }
  )
}

shiny::shinyApp(ui, server)
