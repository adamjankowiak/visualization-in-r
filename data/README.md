# data/

Katalog zawiera dane wejściowe dashboardu wygenerowane przez `R/project1_country_names_pl.R`.

Pliki wymagane do uruchomienia aplikacji:

- `ikea_country_price_statistics_eur.csv`
- `ikea_country_price_statistics_ppp.csv`
- `ikea_ppp_top_15_countries_diagnostics.csv`
- `ppp_conversion_factors_private_consumption.csv`

Opcjonalnie można dodać surowy plik `IKEA_product_catalog.csv`. Po jego dodaniu można odtworzyć cały pipeline poleceniem:

```r
source("R/prepare_dashboard_data.R")
```
