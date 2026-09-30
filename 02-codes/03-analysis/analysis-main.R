# Setup --------------------------------------------------------------------
source(here::here("02-codes", "utils", "setup.R"))

# Data analysis ------------------------------------------------------------
vector_country <-
  df_main |>
  drop_na() |>
  pull(country) |>
  unique()


# Graphs for base and log_diff variables for each country
walk(
  df_main |>pull(country) |>unique(),
  \(country_name) {
    # Graph for base variables
    graph_base <-
      df_main |>
      filter(
        country == country_name
      ) |>
      select(time, country, !ends_with("log_diff")) |>
      pivot_longer(
        cols = !c(time, country),
        names_to = "variable",
        values_to = "value"
      ) |>
      mutate(
        time = paste0(time, "-01"),
        time = as.Date(time)
      ) |>
      ggplot(aes(x = time, y = value, color = variable)) +
      geom_line(linewidth = 1.2, show.legend = FALSE) +
      facet_wrap(variable~., scales = "free") +
      labs(x = country_name)

    ggsave(
      here(
        path_output_graphs_description_data_base_folder,
        glue("{country_name}-description-graph-base.png")
      ),
      graph_base,
      width = 30,
      height = 20
    )

    # Graph for log_diff variables
    graph_log_diff <-
      df_main |>
      filter(
        country == country_name
      ) |>
      select(time, country, ends_with("log_diff")) |>
      pivot_longer(
        cols = !c(time, country),
        names_to = "variable",
        values_to = "value"
      ) |>
      mutate(
        time = paste0(time, "-01"),
        time = as.Date(time)
      ) |>
      ggplot(aes(x = time, y = value, color = variable)) +
      geom_line(linewidth = 1.2, show.legend = FALSE) +
      facet_wrap(variable~., scales = "free")+
      labs(x = country_name)

    ggsave(
      here(
        path_output_graphs_description_data_log_diff_folder,
        glue("{country_name}-description-graph-log-diff.png")
      ),
      graph_log_diff,
      width = 30,
      height = 20
    ) 
  }
)


df_test <-
  df_main |>
  filter(country == "germany") |>
  drop_na()


df_endog <-
  df_test |>
  select(!c(time, country)) |>
  select(
    ends_with("log_diff")
  ) |>
  print()

res <-
  lp_nl_iv(
    endog_data = df_endog,
    shock = df_test |>select(LCPCASH_log_diff),
    cumul_mult = FALSE,
    lags_endog_nl = 4,
    confint = 1.96,
    hor = 12,
    trend = 1,
    switching = df_test |>select(CRUDOIL_log_diff) |>pull(),
    use_logistic = TRUE,
    use_hp = FALSE,
    gamma = 4
  )

plot(res)

summary(res)[7] 
