# Setup ---------------------------------------------------------------------
source(here::here("02-codes", "utils", "setup.R"))


# Import data ---------------------------------------------------------------
# Import data on hicp
df_hicp <-
  map2_dfr(
    c("Sheet 1", "Sheet 2", "Sheet 3"),
    c("total", "industrial_goods", "energy"),
    \(sheet_name, type_name) {
      df <-
        here(
          path_raw_data_folder,
          "prc_hicp_minr__custom_22953881_spreadsheet.xlsx"
        ) |>
        read_xlsx(
          sheet = sheet_name,
          skip = 8,
          na = c("", ":")
        ) |>
        clean_names() |>
        # Remove descriptive rows
        filter(
          !geo_labels %in% c("TIME", "Special value"),
          !is.na(geo_labels)
        ) |>
        # Convert data into numeric
        mutate(
          across(
            .cols = !geo_labels,
            .fns = as.numeric
          ),
          type = type_name
        ) |>
        rename(time = geo_labels) |>
        # Pivot to have countries in rows
        pivot_longer(
          cols = !c(time, type),
          names_to = "country",
          values_to = "hicp"
        )
    }
  ) |>
  # Pivot to have each variable in column
  pivot_wider(
    names_from = type,
    values_from = hicp,
    names_prefix = "hicp_"
  ) |>
  print()


# Import data for copper & oil prices & Eurostoxx
df_financial <-
  here(
    path_raw_data_folder,
    "refinitiv-data.xlsx"
  ) |>
  read_xlsx(
    skip = 4
  ) |>
  rename(time = Code) |>
  # Remove days indication : no days in other datasets
  mutate(
    time = str_sub(as.character(time), 1, -4)
  ) |>
  print()


# Import data on producer prices
df_producer_prices <-
  map2_dfr(
    c("Sheet 1", "Sheet 2", "Sheet 3"),
    c("mining", "manufacturing", "electricity"),
    \(sheet_name, type_name) {
      df <-
        here(
          path_raw_data_folder,
          "sts_inpp_m__custom_22954108_spreadsheet.xlsx"
        ) |>
        read_xlsx(
          sheet = sheet_name,
          skip = 10,
          na = c("", ":")
        ) |>
        clean_names() |>
        # Remove descriptive rows
        filter(
          !geo_labels %in% c("TIME", "Special value"),
          !is.na(geo_labels)
        ) |>
        mutate(
          across(
            .cols = !geo_labels,
            .fns = as.numeric
          ),
          type = type_name
        ) |>
        rename(time = geo_labels) |>
        # Pivot to have countries in rows
        pivot_longer(
          cols = !c(time, type),
          names_to = "country",
          values_to = "producer_prices"
        )
    }
  ) |>
  pivot_wider(
    names_from = type,
    values_from = producer_prices,
    names_prefix = "producer_prices_"
  ) |>
  print()


# Import data on production in industry
df_production <-
  map2_dfr(
    c("Sheet 1", "Sheet 2", "Sheet 3"),
    c("mining", "manufacturing", "electricity"),
    \(sheet_name, type_name) {
      df <-
        here(
          path_raw_data_folder,
          "sts_inpr_m__custom_22954043_spreadsheet.xlsx"
        ) |>
        read_xlsx(
          sheet = sheet_name,
          skip = 10,
          na = c("", ":")
        ) |>
        clean_names() |>
        # Remove descriptive rows
        filter(
          !geo_labels %in% c("TIME", "Special value"),
          !is.na(geo_labels)
        ) |>
        mutate(
          across(
            .cols = !geo_labels,
            .fns = as.numeric
          ),
          type = type_name
        ) |>
        rename(time = geo_labels) |>
        # Pivot to have countries in rows
        pivot_longer(
          cols = !c(time, type),
          names_to = "country",
          values_to = "production"
        )
    }
  ) |>
  pivot_wider(
    names_from = type,
    values_from = production,
    names_prefix = "production_"
  ) |>
  print()


# Create main database ------------------------------------------------------
df_financial_sa <-
  df_financial |>
  filter(time >= "1996-01") |>
  pivot_longer(
    cols = !time,
    names_to = "variable",
    values_to = "value"
  ) |>
  nest(.by = variable) |>
  mutate(
    # Conversion en objet time series
    ts_data = map(data, ~ ts(.x$value, start = c(1996, 1), frequency = 12)),   
    # Application de x13 sécurisée avec future_map
    modele_x13 = future_map(ts_data, ~ x13(.x, spec = "RSA5c")),
    # Fusion de la série SA avec le dataframe d'origine via la colonne 'time'
    data_sa = map2(data, modele_x13, function(df_sub, mod) {
      sa_ts <- mod$final$series[, "sa"]
      # Récupération de l'index temporel exact sous forme de Date
      df_sa <- tibble(
        time = as.Date(zoo::as.yearmon(time(sa_ts))), 
        value_sa = as.numeric(sa_ts)
      )
      df_sub <-
        df_sub |>
        mutate(
          time = as.Date(paste0(time, "-01"))
        )
      # Joindre sur 'time' conserve le nombre de lignes original de df_sub
      left_join(df_sub, df_sa, by = "time")
    })
  ) |>
  select(variable, data_sa) |>
  unnest(data_sa) |>
  pivot_wider(
    names_from = variable,
    values_from = c(value, value_sa),
    names_glue = "{variable}{ifelse(.value == 'value_sa', '_sa', '')}"
  )

df_main <-
  # Merge hicp, producer prices and production data together
  list(df_hicp, df_producer_prices, df_production) |>
  reduce(full_join, by = c("time", "country")) |>
  pivot_longer(
    cols = -c(time, country),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    .by = c(country, variable),
    percent_na = mean(is.na(value)),
    suppress_country = if_else(percent_na > 0.5, country, NA)
  ) |>
  filter(
    !country %in% unique(suppress_country)
  ) |>
  filter(country != "malta") |> # no variations for producer prices
  select(!c(percent_na, suppress_country)) |>
  nest(.by = c(country, variable)) |>
  mutate(
    # Conversion en objet time series
    ts_data = map(data, ~ ts(.x$value, start = c(1996, 1), frequency = 12)),  
    # Application de x13 sécurisée avec future_map
    modele_x13 = future_map(ts_data, ~ x13(.x, spec = "RSA5c")),
    # Fusion de la série SA avec le dataframe d'origine via la colonne 'time'
    data_sa = map2(data, modele_x13, function(df_sub, mod) {
      sa_ts <- mod$final$series[, "sa"]
      # Récupération de l'index temporel exact sous forme de Date
      df_sa <- tibble(
        time = as.Date(zoo::as.yearmon(time(sa_ts))), 
        value_sa = as.numeric(sa_ts)
      )
      df_sub <-
        df_sub |>
        mutate(
          time = as.Date(paste0(time, "-01"))
        )
      # Joindre sur 'time' conserve le nombre de lignes original de df_sub
      left_join(df_sub, df_sa, by = "time")
    })
  ) |>
  select(country, variable, data_sa) |>
  unnest(data_sa) |>
  pivot_wider(
    names_from = variable,
    values_from = c(value, value_sa),
    names_glue = "{variable}{ifelse(.value == 'value_sa', '_sa', '')}"
  ) |>
  # Add financial data (no country so can't merge before)
  left_join(
    df_financial_sa,
    join_by(time)
  ) |>  
  # Compute log_diff for each variable
  mutate(
    .by = country, 
    across(
      .cols = !c(time),
      .fns = \(variable) {(log(variable) - log(lag(variable))) * 100},
      .names = "{.col}_log_diff"
    ),
    across(
      .cols = c(LCPCASH, LCPCASH_sa, CRUDOIL, CRUDOIL_sa, DJSTOXX, DJSTOXX_sa),
      .fns = \(variable) {log(variable)},
      .names = "{.col}_log"
    )
  ) |>
  print()


# Save database
write_csv(
  df_main,
  path_processed_data_df_main
)





