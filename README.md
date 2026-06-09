# visualization-in-r — IKEA Price Explorer
Rozwinięcie mniejszego projektu analizy danych

## Szybkie uruchomienie w RStudio

1. Otwórz w RStudio plik:

```text
visualization-in-r.Rproj
```

2. W konsoli RStudio uruchom instalację pakietów:

```r
source("install_packages.R")
```

3. Uruchom aplikację:

```r
shiny::runApp()
```

Alternatywnie:

```r
source("run_app.R")
```

## Co zawiera dashboard
- zawiera grafiki: interaktywną mapę choropletyczną, ranking krajów, wykres punktowy, gauge dla kraju i statyczne mapy,
- zawiera prezentacje liczbowe w formie liczników KPI i wskaźników gauge,
- pozwala sterować widokiem: wybór metryki, krajów, liczby pozycji w rankingu, kierunku sortowania, skali log10 i kraju do analizy szczegółowej,
- uwzględnia mapę wygenerowaną w `R/project1_country_names_pl.R`, osadzoną w zakładce `Mapa z project1`,
- ma osobną zakładkę `Dane` z tabelami źródłowymi i eksportem CSV.

## Struktura projektu

```text
.gitignore
visualization-in-r.Rproj
app.R
install_packages.R
run_app.R
README.md
project1.R
R/
  project1_country_names_pl.R
  prepare_dashboard_data.R
data/
  ikea_country_price_statistics_eur.csv
  ikea_country_price_statistics_ppp.csv
  ikea_ppp_top_15_countries_diagnostics.csv
  ppp_conversion_factors_private_consumption.csv
www/
  ikea_interactive_choropleth_price_map_eur.html
  ikea_price_ranking_plotly_eur.html
  ikea_static_price_map_ggplot_sf_eur.png
  ikea_static_price_map_ggplot_sf_ppp.png
archive/
  original_Rhistory_not_used_by_app.Rhistory.txt
```

## Dane

Dashboard działa od razu na gotowych plikach CSV w katalogu `data/`. Nie wymaga surowego katalogu IKEA do samego uruchomienia.

Jeżeli masz pełny plik `IKEA_product_catalog.csv`, umieść go jako:

```text
data/IKEA_product_catalog.csv
```

Następnie odtwórz pipeline:

```r
source("R/prepare_dashboard_data.R")
```

Skrypt wykona `R/project1_country_names_pl.R` i przeniesie nowe eksporty do `data/` oraz `www/`.

Uwaga: regeneracja wymaga połączenia z Internetem, ponieważ pipeline pobiera kursy walut oraz dane PPP.

## Najważniejsze pliki

- `app.R` — właściwa aplikacja Shiny.
- `project1.R` — kompatybilny punkt wejścia ze starego repozytorium; źródłuje `R/project1_country_names_pl.R`.
- `R/project1_country_names_pl.R` — pełny pipeline czyszczenia danych, przeliczenia EUR/PPP, map i eksportów.
- `R/prepare_dashboard_data.R` — skrypt do regeneracji danych dashboardu.
- `www/ikea_interactive_choropleth_price_map_eur.html` — oryginalna mapa Plotly z projektu.

## Uwagi migracyjne

`.Rhistory` nie jest aktywnym plikiem projektu. Został przeniesiony do `archive/`, ponieważ `.gitignore` wyklucza historię sesji R z repozytorium.
