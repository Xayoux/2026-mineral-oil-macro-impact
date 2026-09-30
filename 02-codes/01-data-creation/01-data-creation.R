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
df_main <-
  # Merge hicp, producer prices and production data together
  list(df_hicp, df_producer_prices, df_production) |>
  reduce(full_join, by = c("time", "country")) |>
  # Add financial data (no country so can't merge before)
  full_join(
    df_financial,
    join_by(time)
  ) |>
  # Compute log_diff for each variable
  mutate(
    .by = country,
    across(
      .cols = !c(time),
      .fns = \(variable) {log(variable) - log(lag(variable)) * 100},
      .names = "{.col}_log_diff"
    )
  ) |>
  print()


# Save database
write.csv(
  df_main,
  path_processed_data_df_main
)





