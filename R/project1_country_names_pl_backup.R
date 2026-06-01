# ============================================================
# IKEA Global Product Catalog 2026
# Rozkład cen produktów IKEA na świecie po przeliczeniu na EUR
# ============================================================


# ============================================================
# 1. Pakiety
# ============================================================
# install.packages(c(
#    "readr", "dplyr", "tidyr", "janitor", "stringr",
#    "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata",
#    "countrycode", "plotly", "htmlwidgets", "scales",
#    "httr2", "jsonlite"
#  ))

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(plotly)
library(htmlwidgets)
library(scales)
library(httr2)
library(jsonlite)


# ============================================================
# 2. Import danych
# ============================================================

IKEA_product_catalog <- read_csv("data/IKEA_product_catalog.csv")

ikea_raw <- IKEA_product_catalog |>
  clean_names()

glimpse(ikea_raw)


# ============================================================
# 3. Czyszczenie danych
# ============================================================

ikea <- ikea_raw |>
  mutate(
    country = str_squish(country),
    currency = str_to_upper(str_squish(currency)),
    price_num = parse_number(as.character(price)),
    
    product_rating_num = case_when(
      is.na(product_rating) ~ NA_real_,
      str_to_lower(as.character(product_rating)) %in% c("none", "nan", "", "null") ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(product_rating))
    ),
    
    product_rating_count_num = case_when(
      is.na(product_rating_count) ~ NA_real_,
      str_to_lower(as.character(product_rating_count)) %in% c("none", "nan", "", "null") ~ NA_real_,
      TRUE ~ suppressWarnings(parse_number(as.character(product_rating_count)))
    ),
    
    main_category = str_squish(main_category),
    sub_category = str_squish(sub_category),
    product_name = str_squish(product_name)
  ) |>
  filter(
    !is.na(country),
    !is.na(currency),
    !is.na(price_num),
    price_num > 0
  )


# ============================================================
# 4. Ręczne poprawki nazw krajów i ISO3
# ============================================================

ikea <- ikea |>
  mutate(
    country_clean = case_when(
      country %in% c("USA", "US", "United States") ~ "United States of America",
      country %in% c("UK", "United Kingdom") ~ "United Kingdom",
      country %in% c("UAE", "United Arab Emirates") ~ "United Arab Emirates",
      country %in% c("South_Korea", "South Korea", "Korea, Republic of") ~ "South Korea",
      country %in% c("Czechia") ~ "Czech Republic",
      country %in% c("Türkiye", "Turkiye") ~ "Turkey",
      TRUE ~ country
    ),
    iso3 = countrycode(
      country_clean,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "South Korea" = "KOR",
        "United States of America" = "USA",
        "United Kingdom" = "GBR",
        "United Arab Emirates" = "ARE"
      )
    ),
    # Polska nazwa kraju do wyświetlania na wykresach i w eksportach.
    # iso3 zostawiamy jako klucz techniczny do map i złączeń.
    country_pl = coalesce(
      tryCatch(
        countrycode(
          iso3,
          origin = "iso3c",
          destination = "cldr.short.pl",
          custom_match = c(
            "ARE" = "Zjednoczone Emiraty Arabskie",
            "CZE" = "Czechy",
            "GBR" = "Wielka Brytania",
            "KOR" = "Korea Południowa",
            "TUR" = "Turcja",
            "USA" = "Stany Zjednoczone"
          )
        ),
        error = function(e) rep(NA_character_, length(iso3))
      ),
      country_clean
    )
  )

# Kontrola niedopasowanych krajów
ikea |>
  filter(is.na(iso3)) |>
  distinct(country, country_clean, country_pl) |>
  arrange(country)


# ============================================================
# 5. Lista walut w danych
# ============================================================

currency_summary <- ikea |>
  count(currency, sort = TRUE)

currency_summary


# ============================================================
# 6. Pobranie kursów walut do EUR + ręczne uzupełnienie braków
# ============================================================
# Frankfurter zwraca kursy jako:
# 1 EUR = X jednostek waluty.
#
# Dla ceny w lokalnej walucie:
# price_eur = price_local / rate_per_eur

exchange_date <- "2026-02-27"

currencies_needed <- ikea |>
  distinct(currency) |>
  pull(currency) |>
  sort()

currencies_to_fetch <- setdiff(currencies_needed, "EUR")

url <- paste0(
  "https://api.frankfurter.dev/v1/",
  exchange_date,
  "?base=EUR&symbols=",
  paste(currencies_to_fetch, collapse = ",")
)

response <- request(url) |>
  req_perform()

rates_json <- response |>
  resp_body_json()

api_rates_tbl <- tibble(
  currency = names(rates_json$rates),
  rate_per_eur = as.numeric(unlist(rates_json$rates))
) |>
  bind_rows(
    tibble(
      currency = "EUR",
      rate_per_eur = 1
    )
  )

api_rates_tbl

# ============================================================
# 7. Ręczne kursy dla walut, których nie zwróciło API
# ============================================================
# WAŻNE:
# rate_per_eur oznacza: 1 EUR = X jednostek danej waluty.
#
# Uzupełnij wartości kursami z tej samej daty co exchange_date.
# Poniżej zostawiam NA_real_, żeby kod jasno wymagał uzupełnienia.

manual_rates_tbl <- tribble(
  ~currency, ~rate_per_eur,
  "AED", 4.29,
  "BHD", 0.44,
  "CLP", 1043.63,
  "COP", 4257.6,
  "EGP", 61.7,
  "JOD", 0.83,
  "KWD", 0.36,
  "MAD", 10.82,
  "OMR", 0.45,
  "QAR", 4.25,
  "SAR", 4.38,
  "RSD", 117.35
)


# ============================================================
# 8. Złączenie kursów i kontrola braków
# ============================================================

rates_tbl <- api_rates_tbl |>
  bind_rows(manual_rates_tbl) |>
  group_by(currency) |>
  summarise(
    rate_per_eur = first(na.omit(rate_per_eur)),
    .groups = "drop"
  )

missing_rates <- ikea |>
  distinct(currency) |>
  anti_join(
    rates_tbl |> filter(!is.na(rate_per_eur)),
    by = "currency"
  )

missing_rates

if (nrow(missing_rates) > 0) {
  stop(
    paste0(
      "Nadal brakuje kursów dla walut: ",
      paste(missing_rates$currency, collapse = ", "),
      "\nUzupełnij manual_rates_tbl."
    )
  )
}

# ============================================================
# ============================================================
# 8A. Przeliczenie cen lokalnych na EUR
# ============================================================
# Wartości z Frankfurter i manual_rates_tbl mają postać:
# 1 EUR = X jednostek lokalnej waluty.
# Dlatego cena w EUR = cena lokalna / rate_per_eur.

ikea_eur <- ikea |>
  left_join(rates_tbl, by = "currency") |>
  mutate(
    price_eur = price_num / rate_per_eur
  ) |>
  filter(
    !is.na(price_eur),
    price_eur > 0
  )

ikea_eur



# 9. Statystyki cen EUR per kraj
# ============================================================

country_prices <- ikea_eur |>
  filter(!is.na(iso3)) |>
  group_by(country_clean, country_pl, iso3) |>
  summarise(
    n_records = n(),
    n_products = n_distinct(product_id),
    n_main_categories = n_distinct(main_category),
    n_sub_categories = n_distinct(sub_category),
    
    median_price_eur = median(price_eur, na.rm = TRUE),
    mean_price_eur = mean(price_eur, na.rm = TRUE),
    min_price_eur = min(price_eur, na.rm = TRUE),
    q1_price_eur = quantile(price_eur, 0.25, na.rm = TRUE),
    q3_price_eur = quantile(price_eur, 0.75, na.rm = TRUE),
    max_price_eur = max(price_eur, na.rm = TRUE),
    iqr_price_eur = IQR(price_eur, na.rm = TRUE),
    
    mean_rating = mean(product_rating_num, na.rm = TRUE),
    median_rating_count = median(product_rating_count_num, na.rm = TRUE),
    
    currencies = paste(sort(unique(currency)), collapse = ", "),
    .groups = "drop"
  ) |>
  mutate(
    price_range_eur = max_price_eur - min_price_eur
  )

country_prices


# ============================================================
# 10. Indeks cenowy dla tych samych produktów
# ============================================================

product_global_prices <- ikea_eur |>
  group_by(product_id) |>
  summarise(
    global_product_median_eur = median(price_eur, na.rm = TRUE),
    countries_available = n_distinct(country_clean),
    .groups = "drop"
  ) |>
  filter(
    countries_available >= 3,
    !is.na(global_product_median_eur),
    global_product_median_eur > 0
  )

country_price_index <- ikea_eur |>
  inner_join(product_global_prices, by = "product_id") |>
  mutate(
    relative_price = price_eur / global_product_median_eur
  ) |>
  group_by(country_clean, country_pl, iso3) |>
  summarise(
    price_index = median(relative_price, na.rm = TRUE) * 100,
    compared_products = n_distinct(product_id),
    .groups = "drop"
  )

country_prices <- country_prices |>
  left_join(
    country_price_index,
    by = c("country_clean", "country_pl", "iso3")
  )

country_prices


# ============================================================
# ============================================================
# 10B. Dane PPP do przyszłego użycia w projekcie
# ============================================================
# PPP = parytet siły nabywczej.
# Używamy wskaźnika World Bank PA.NUS.PRVT.PP:
# PPP conversion factor, private consumption
# local currency unit per international dollar.
#
# Dla produktu konsumenckiego:
# price_ppp_int_dollar = cena lokalna / PPP
#
# Jednostka price_ppp_int_dollar to dolar międzynarodowy PPP,
# a nie zwykły USD ani EUR.

ppp_indicator <- "PA.NUS.PRVT.PP"

ppp_url <- paste0(
  "https://api.worldbank.org/v2/country/all/indicator/",
  ppp_indicator,
  "?format=json&per_page=20000"
)

ppp_response <- request(ppp_url) |>
  req_perform()

ppp_json <- ppp_response |>
  resp_body_json()

ppp_tbl <- bind_rows(lapply(ppp_json[[2]], function(x) {
  tibble(
    iso3 = x$countryiso3code,
    ppp_year = as.integer(x$date),
    ppp_lcu_per_int_dollar = as.numeric(x$value)
  )
})) |>
  filter(
    !is.na(iso3),
    iso3 != "",
    !is.na(ppp_lcu_per_int_dollar),
    ppp_lcu_per_int_dollar > 0
  ) |>
  group_by(iso3) |>
  slice_max(ppp_year, n = 1, with_ties = FALSE) |>
  ungroup()

# Kontrola krajów z danych IKEA, dla których nie znaleziono PPP.
missing_ppp <- ikea |>
  distinct(country, country_clean, country_pl, iso3) |>
  filter(!is.na(iso3)) |>
  anti_join(ppp_tbl, by = "iso3") |>
  arrange(country_clean)

missing_ppp

if (nrow(missing_ppp) > 0) {
  warning(
    paste0(
      "Brakuje PPP dla krajów: ",
      paste(missing_ppp$country_pl, collapse = ", "),
      "\nKraje bez PPP będą pominięte w metrykach PPP."
    )
  )
}

# Dane produktowe rozszerzone o PPP.
# Zostawiamy je jako osobny obiekt, żeby w przyszłości można było
# użyć PPP również w wykresach interaktywnych, rankingach i boxplotach.
ikea_ppp <- ikea |>
  left_join(ppp_tbl, by = "iso3") |>
  mutate(
    price_ppp_int_dollar = price_num / ppp_lcu_per_int_dollar
  )

# Statystyki cen PPP per kraj.
country_prices_ppp <- ikea_ppp |>
  filter(
    !is.na(iso3),
    !is.na(price_ppp_int_dollar),
    price_ppp_int_dollar > 0
  ) |>
  group_by(country_clean, country_pl, iso3) |>
  summarise(
    n_records = n(),
    n_products = n_distinct(product_id),
    n_main_categories = n_distinct(main_category),
    n_sub_categories = n_distinct(sub_category),
    
    median_price_ppp = median(price_ppp_int_dollar, na.rm = TRUE),
    mean_price_ppp = mean(price_ppp_int_dollar, na.rm = TRUE),
    min_price_ppp = min(price_ppp_int_dollar, na.rm = TRUE),
    q1_price_ppp = quantile(price_ppp_int_dollar, 0.25, na.rm = TRUE),
    q3_price_ppp = quantile(price_ppp_int_dollar, 0.75, na.rm = TRUE),
    max_price_ppp = max(price_ppp_int_dollar, na.rm = TRUE),
    iqr_price_ppp = IQR(price_ppp_int_dollar, na.rm = TRUE),
    
    mean_rating = mean(product_rating_num, na.rm = TRUE),
    median_rating_count = median(product_rating_count_num, na.rm = TRUE),
    
    ppp_year = max(ppp_year, na.rm = TRUE),
    currencies = paste(sort(unique(currency)), collapse = ", "),
    .groups = "drop"
  ) |>
  mutate(
    price_range_ppp = max_price_ppp - min_price_ppp
  )

# Indeks PPP dla tych samych produktów, analogiczny do indeksu EUR.
product_global_prices_ppp <- ikea_ppp |>
  group_by(product_id) |>
  summarise(
    global_product_median_ppp = median(price_ppp_int_dollar, na.rm = TRUE),
    countries_available = n_distinct(country_clean),
    .groups = "drop"
  ) |>
  filter(
    countries_available >= 3,
    !is.na(global_product_median_ppp),
    global_product_median_ppp > 0
  )

country_price_index_ppp <- ikea_ppp |>
  inner_join(product_global_prices_ppp, by = "product_id") |>
  mutate(
    relative_price_ppp = price_ppp_int_dollar / global_product_median_ppp
  ) |>
  group_by(country_clean, country_pl, iso3) |>
  summarise(
    price_index_ppp = median(relative_price_ppp, na.rm = TRUE) * 100,
    compared_products_ppp = n_distinct(product_id),
    .groups = "drop"
  )

country_prices_ppp <- country_prices_ppp |>
  left_join(
    country_price_index_ppp,
    by = c("country_clean", "country_pl", "iso3")
  )

country_prices_ppp

# Diagnostyka wartości skrajnych PPP.
# Przy mapach choropletycznych pojedynczy kraj o bardzo wysokiej medianie
# może rozciągnąć skalę kolorów i obniżyć czytelność pozostałych krajów.
ppp_top_countries <- country_prices_ppp |>
  arrange(desc(median_price_ppp)) |>
  select(
    country_clean,
    country_pl,
    iso3,
    median_price_ppp,
    mean_price_ppp,
    n_records,
    n_products,
    ppp_year,
    currencies
  ) |>
  slice_head(n = 15)

ppp_top_countries


# 11. Mapa świata: sf
# ============================================================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world_prices <- world |>
  left_join(
    country_prices,
    by = c("iso_a3" = "iso3")
  )

world_prices_ppp <- world |>
  left_join(
    country_prices_ppp,
    by = c("iso_a3" = "iso3")
  )


# ============================================================
# 12. Statyczna mapa ggplot2 + sf
# ============================================================
# Spełnia wymaganie: mapa wykonana z ggplot2 i sf.

map_ggplot <- ggplot(world_prices) +
  geom_sf(
    aes(fill = median_price_eur),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey90",
    labels = label_number(big.mark = " ", suffix = " €")
  ) +
  labs(
    title = "Mediana cen produktów IKEA według kraju",
    subtitle = "Ceny przeliczone na EUR",
    fill = "Mediana\nceny EUR"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  )

map_ggplot

ggsave(
  filename = "ikea_static_price_map_ggplot_sf_eur.png",
  plot = map_ggplot,
  width = 12,
  height = 7,
  dpi = 300
)


# ============================================================
# 12B. Statyczna mapa ggplot2 + sf — PPP
# ============================================================
# Ta mapa jest odpowiednikiem punktu 12, ale pokazuje medianę cen
# po korekcie parytetem siły nabywczej.
# Jednostka: dolary międzynarodowe PPP.
#
# Korekty względem podstawowej wersji:
# - usunięcie Antarktydy z kompozycji mapy,
# - skala logarytmiczna, żeby pojedyncze wartości skrajne
#   nie dominowały całej palety kolorów,
# - jawne białe tło i czarny tekst dla poprawnego eksportu PNG,
# - doprecyzowana legenda i podtytuł.
#
# Nie stosujemy progu minimalnej liczby produktów — wszystkie kraje
# z dostępną wartością PPP pozostają widoczne.

world_prices_ppp_plot <- world_prices_ppp |>
  filter(continent != "Antarctica")

map_ggplot_ppp <- ggplot(world_prices_ppp_plot) +
  geom_sf(
    aes(fill = median_price_ppp),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    trans = "log10",
    na.value = "grey90",
    labels = label_number(big.mark = " ", suffix = " intl. $")
  ) +
  labs(
    title = "Mediana cen produktów IKEA według kraju",
    subtitle = "Ceny przeliczone z walut lokalnych na dolary międzynarodowe PPP; skala kolorów: log10",
    fill = "Mediana ceny\nw intl. $ PPP"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 16, color = "black"),
    plot.subtitle = element_text(size = 11, color = "black"),
    legend.title = element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.position = "right"
  )

map_ggplot_ppp

ggsave(
  filename = "ikea_static_price_map_ggplot_sf_ppp.png",
  plot = map_ggplot_ppp,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 13. Dane do głównej interaktywnej mapy plotly
# ============================================================

map_long <- country_prices |>
  left_join(
    country_prices_ppp |>
      select(
        country_clean,
        country_pl,
        iso3,
        median_price_ppp
      ),
    by = c("country_clean", "country_pl", "iso3")
  ) |>
  select(
    country_clean,
    country_pl,
    iso3,
    currencies,
    n_records,
    n_products,
    n_main_categories,
    n_sub_categories,
    median_price_eur,
    mean_price_eur,
    median_price_ppp,
    price_range_eur,
    price_index,
    compared_products
  ) |>
  pivot_longer(
    cols = c(
      median_price_eur,
      mean_price_eur,
      median_price_ppp,
      price_range_eur,
      price_index
    ),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric_label = recode(
      metric,
      median_price_eur = "Mediana ceny w EUR",
      mean_price_eur = "Średnia cena w EUR",
      median_price_ppp = "Mediana ceny PPP",
      price_range_eur = "Zakres cen w EUR",
      price_index = "Indeks cenowy tych samych produktów"
    ),
    value_label = case_when(
      metric == "price_index" ~ paste0(round(value, 1), " pkt"),
      metric == "median_price_ppp" ~ paste0(round(value, 2), " intl. $ PPP"),
      TRUE ~ paste0(round(value, 2), " EUR")
    ),
    hover_text = paste0(
      "<b>", country_pl, "</b>",
      "<br>Oryginalna waluta: ", currencies,
      "<br>Liczba rekordów: ", n_records,
      "<br>Liczba unikalnych produktów: ", n_products,
      "<br>Liczba głównych kategorii: ", n_main_categories,
      "<br>Liczba podkategorii: ", n_sub_categories,
      "<br>Produkty porównane w indeksie: ", compared_products,
      "<br>Metryka: ", metric_label,
      "<br>Wartość: ", value_label
    )
  )


# ============================================================
# 14. Główna interaktywna mapa plotly choropleth
# ============================================================

metric_order <- c(
  "Mediana ceny w EUR",
  "Średnia cena w EUR",
  "Mediana ceny PPP",
  "Zakres cen w EUR",
  "Indeks cenowy tych samych produktów"
)

map_long <- map_long |>
  mutate(
    metric_label = factor(metric_label, levels = metric_order)
  )

metrics <- levels(map_long$metric_label)

fig_map <- plot_ly()

for (i in seq_along(metrics)) {
  
  metric_i <- metrics[i]
  
  temp <- map_long |>
    filter(metric_label == metric_i)
  
  fig_map <- fig_map |>
    add_trace(
      data = temp,
      type = "choropleth",
      locations = ~iso3,
      z = ~value,
      text = ~hover_text,
      hoverinfo = "text",
      colorscale = "Viridis",
      reversescale = FALSE,
      visible = i == 1,
      name = metric_i,
      marker = list(
        line = list(
          color = "white",
          width = 0.5
        )
      ),
      colorbar = list(
        title = metric_i
      )
    )
}

buttons <- lapply(seq_along(metrics), function(i) {
  
  visible_vector <- rep(FALSE, length(metrics))
  visible_vector[i] <- TRUE
  
  list(
    method = "update",
    args = list(
      list(visible = visible_vector),
      list(
        title = list(
          text = paste0(
            "Rozkład cen produktów IKEA na świecie<br><sup>",
            metrics[i],
            "</sup>"
          ),
          x = 0.02,
          xanchor = "left",
          y = 0.97,
          yanchor = "top"
        )
      )
    ),
    label = metrics[i]
  )
})

fig_map <- fig_map |>
  layout(
    title = list(
      text = paste0(
        "Rozkład cen produktów IKEA na świecie<br><sup>",
        metrics[1],
        "</sup>"
      ),
      x = 0.02,
      xanchor = "left",
      y = 0.97,
      yanchor = "top"
    ),
    updatemenus = list(
      list(
        type = "dropdown",
        active = 0,
        buttons = buttons,
        direction = "down",
        x = 1,
        y = 1.15,
        xanchor = "right",
        yanchor = "top",
        pad = list(r = 10, t = 0)
      )
    ),
    geo = list(
      projection = list(type = "natural earth"),
      showframe = FALSE,
      showcoastlines = TRUE,
      coastlinecolor = "gray70",
      landcolor = "gray95",
      bgcolor = "rgba(0,0,0,0)"
    ),
    margin = list(
      l = 0,
      r = 10,
      t = 130,
      b = 0
    )
  ) |>
  config(
    displayModeBar = TRUE,
    responsive = TRUE
  )

fig_map

saveWidget(
  fig_map,
  file = "ikea_interactive_choropleth_price_map_eur.html",
  selfcontained = TRUE
)


# ============================================================
# 15. Dodatkowy wykres plotly:
# ranking krajów według mediany ceny w EUR
# ============================================================

ranking_data <- country_prices |>
  arrange(desc(median_price_eur)) |>
  slice_head(n = 15)

fig_ranking <- plot_ly(
  data = ranking_data,
  x = ~median_price_eur,
  y = ~reorder(country_pl, median_price_eur),
  type = "bar",
  orientation = "h",
  text = ~paste0(
    "<b>", country_pl, "</b>",
    "<br>Oryginalna waluta: ", currencies,
    "<br>Mediana ceny: ", round(median_price_eur, 2), " EUR",
    "<br>Średnia cena: ", round(mean_price_eur, 2), " EUR",
    "<br>Liczba produktów: ", n_products
  ),
  hoverinfo = "text"
) |>
  layout(
    title = "Top 15 krajów według mediany ceny produktów IKEA w EUR",
    xaxis = list(title = "Mediana ceny w EUR"),
    yaxis = list(title = ""),
    margin = list(l = 130, r = 30, t = 70, b = 60)
  ) |>
  config(
    displayModeBar = TRUE,
    responsive = TRUE
  )

fig_ranking

saveWidget(
  fig_ranking,
  file = "ikea_price_ranking_plotly_eur.html",
  selfcontained = TRUE
)


# ============================================================
# 16. Dodatkowy wykres plotly:
# boxplot cen EUR dla krajów z największą liczbą produktów
# ============================================================

top_countries_by_products <- country_prices |>
  arrange(desc(n_products)) |>
  slice_head(n = 10) |>
  pull(country_clean)

box_data <- ikea_eur |>
  filter(
    country_clean %in% top_countries_by_products,
    !is.na(price_eur),
    price_eur > 0
  )

fig_boxplot <- plot_ly(
  data = box_data,
  x = ~country_pl,
  y = ~price_eur,
  type = "box",
  boxpoints = "outliers",
  text = ~paste0(
    "<b>", country_pl, "</b>",
    "<br>Produkt: ", product_name,
    "<br>Kategoria główna: ", main_category,
    "<br>Podkategoria: ", sub_category,
    "<br>Cena oryginalna: ", price_num, " ", currency,
    "<br>Cena EUR: ", round(price_eur, 2), " EUR"
  ),
  hoverinfo = "text"
) |>
  layout(
    title = "Rozkład cen produktów IKEA w EUR",
    xaxis = list(title = ""),
    yaxis = list(title = "Cena w EUR"),
    margin = list(l = 70, r = 30, t = 70, b = 120)
  ) |>
  config(
    displayModeBar = TRUE,
    responsive = TRUE
  )

fig_boxplot

saveWidget(
  fig_boxplot,
  file = "ikea_price_boxplot_plotly_eur.html",
  selfcontained = TRUE
)


# ============================================================
# 17. Eksport danych wynikowych
# ============================================================

write_csv(
  country_prices,
  "ikea_country_price_statistics_eur.csv"
)

write_csv(
  ikea_eur,
  "ikea_product_catalog_with_eur_prices.csv"
)

write_csv(
  ppp_tbl,
  "ppp_conversion_factors_private_consumption.csv"
)

write_csv(
  ppp_top_countries,
  "ikea_ppp_top_15_countries_diagnostics.csv"
)

write_csv(
  country_prices_ppp,
  "ikea_country_price_statistics_ppp.csv"
)

write_csv(
  ikea_ppp,
  "ikea_product_catalog_with_ppp_prices.csv"
)

