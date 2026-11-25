# Setup ---------------------------------------------------------------------
source(here::here("02-codes", "utils", "setup.R"))


# Import country data -------------------------------------------------------
# Import data on hicp
df_hicp <-
  map2_dfr(
    paste("Sheet", 1:7),
    c("total", "goods", "industrial_goods", "industrial_goods_ne", "energy", "services", "total_ne"),
    \(sheet_name, type_name) {
      df <-
        here(
          path_raw_data_folder,
          "prc_hicp_minr__custom_24325404_spreadsheet.xlsx"
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
  select(time, country, hicp_total_ne, hicp_industrial_goods_ne) |>
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


# Import global data --------------------------------------------------------
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


# Import data on monthly world copper production
df_copper_production <-
  here(
    path_raw_data_folder,
    "refinitiv-copper-data.xlsx"
  ) |>
  read_xlsx(
    skip = 4,
    sheet = "Copper production"
  ) |>
  rename(time = Code) |>
  # Remove days indication : no days in other datasets
  mutate(
    time = str_sub(as.character(time), 1, -4)
  ) |>
  select(time, CUIWMPM) |>
  rename(copper_production = CUIWMPM) |>
  print()


# Import data on copper LME and COMEX stocks
df_copper_stocks <-
  here(
    path_raw_data_folder,
    "refinitiv-copper-data.xlsx"
  ) |>
  read_xlsx(
    skip = 4,
    sheet = "Copper stocks"
  ) |>
  rename(time = Code) |>
  # Remove days indication : no days in other datasets
  mutate(
    time = str_sub(as.character(time), 1, -4),
    HGCMXST = HGCMXST * 0.9071874,
    copper_stocks_total = LCPWARE + HGCMXST
  ) |>
  select(time, LCPWARE) |>
  print()


# Import data on world industrial production
df_world_industrial_production <-
  here(
    path_raw_data_folder,
    "OECD_plus6_industrial_production.xlsx"
  ) |>
  read_xlsx() |>
  clean_names() |>
  rename(
    time = x1,
    world_industrial_production = oecd_6nme_industrial_production
  ) |>
  select(time, world_industrial_production) |>
  mutate(time = str_sub(as.character(time), 1, -4)) |>
  print()


# Import data on oil supply shocks
df_oil_supply_shock <-
  here(
    path_raw_data_folder,
    "BH2_supply_shocks.xlsx"
  ) |>
  read_xlsx(skip = 1) |>
  clean_names() |>
  rename(
    time = x1
  ) |>
  select(time, oil_supply_shocks) |>
  mutate(time = str_sub(as.character(time), 1, -4)) |>
  print()


# Import data on oil demand shocks
df_oil_demand_shock <-
  here(
    path_raw_data_folder,
    "BH2_demand_shocks.xlsx"
  ) |>
  read_xlsx(skip = 1) |>
  clean_names() |>
  rename(
    time = x1
  ) |>
  select(time, oil_consumption_demand_shocks) |>
  mutate(time = str_sub(as.character(time), 1, -4)) |>
  print()


# Create main database ------------------------------------------------------
# Create tibble for global data
df_global_data <-
  # Take df with global data -> but not for shocks -> na sa for shocks
  list(df_financial, df_copper_production, df_copper_stocks,
       df_world_industrial_production) |>
  reduce(full_join, by = "time") |>
  # Convert time in a Date type (need for ts and lag)
  mutate(time = as.Date(paste0(time, "-01"))) |>
  arrange(time) |>
  pivot_longer(
    cols = !time,
    names_to = "variable",
    values_to = "value"
  ) |>
  # Nest by variable so that we can sa each variable
  nest(.by = variable) |>
  mutate(
    # Convert each variable in a TS object with a dynamic start date
    ts_data = map(
      data,
      \(df_sub) {
        # Keep only values between the first and last non-NA 
        idx_valides <- which(!is.na(df_sub$value))
        df_clean <- df_sub[min(idx_valides):max(idx_valides), ]
        # Extract the month and year date of the start
        date_debut <- min(df_clean$time)
        start_vec <-
          c(
            as.integer(format(date_debut, "%Y")),
            as.integer(format(date_debut, "%m"))
          )
        # Create the TS object
        ts(df_clean$value, start = start_vec, frequency = 12)
      }),
    # Use Eurostat x13 method to sa each variable
    modele_x13 = future_map(ts_data, ~ x13(.x, spec = "RSA5c")),
    # Merge sa series with original series thanks to the time column
    data_sa =
      map2(
        data,
        modele_x13,
        function(df_sub, mod) {
          # Keep the seasonally adjusted series
          sa_ts <- mod$final$series[, "sa"]
          # Create a dfwith the temporal index and the sa series 
          df_sa <-
            tibble(
              time = as.Date(zoo::as.yearmon(time(sa_ts))),
              value_sa = as.numeric(sa_ts)
            )
          # Join with the original series
          left_join(df_sub, df_sa, by = "time")
        }
      )
  ) |>
  select(variable, data_sa) |>
  unnest(data_sa) |>
  # Pivot wider to have each variable in column. Add "_sa" for sa series
  pivot_wider(
    names_from = variable,
    values_from = c(value, value_sa),
    names_glue = "{variable}{ifelse(.value == 'value_sa', '_sa', '')}"
  ) |>
  arrange(time) |>
  # Compute the log for all variables except world_industrial_production
  # Already index data
  mutate(
    across(
      .cols = !c(time, starts_with("world_industrial_production")),
      .fns = \(x) {log(x)},
      .names = "{.col}_log"
    )
  ) |>
  # Compute log_diff for all variables except variables already in log
  mutate(
    across(
      .cols = !c(time, ends_with("_log")),
      .fns = \(x) {(log(x) - log(lag(x))) * 100},
      .names = "{.col}_log_diff"
    )
  ) |>
  # Compute the Delta of stocks as in Baumeister2019 (for BHSBVAR)
  mutate(
    LCPWARE_delta = ((LCPWARE - lag(LCPWARE)) / lag(copper_production)) * 100,
    LCPWARE_delta_sa = ((LCPWARE_sa - lag(LCPWARE_sa)) / lag(copper_production_sa)) * 100
  ) |>
  # Add oil supply and demand shocks
  full_join(
    df_oil_demand_shock |> mutate(time = as.Date(as.yearmon(time))),
    join_by(time)
  ) |>
  full_join(
    df_oil_supply_shock |> mutate(time = as.Date(as.yearmon(time))),
    join_by(time)
  ) |>
  print()










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
  print()


# Save database
write_csv(
  df_main,
  path_processed_data_df_main
)





