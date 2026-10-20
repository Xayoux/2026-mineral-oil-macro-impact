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


df_endog_level <-
  df_test |>
  select(!c(time, country)) |>
  select(
    !ends_with("log_diff")
  ) |>
  print()

df <-
  df_endog_level |>
  select(
    hicp_industrial_goods, producer_prices_manufacturing, production_manufacturing, LCPCASH, CRUDOIL, DJSTOXX
  ) |>
  mutate(log_LCPCASH = log(LCPCASH), log_CRUDOIL = log(CRUDOIL)) |>
  select(!c(LCPCASH, CRUDOIL))

res <-
  lp_lin_iv(
    endog_data = df,
    shock = df |> select(log_LCPCASH),
    cumul_mult = TRUE,
    lags_endog = 24,
    confint = 1.96,
    hor = 24,
    trend = 1,
    lags_criterion = NaN
  )



a <- plot_lin(res)


a[[1]]

names(res)

res$specs

res$irf_lin_mean

summary(res)

## library(patchwork)

nb <- 5
a$gg_s1[[nb]] + a$gg_s2[[nb]]



df_main |>
  pull(country) |>
  unique()


summary(res)[7] 




df_panel <-
  df_main |>
  relocate(country, time) |>
  mutate(.by = country,
    hicp_total_lag = lag(hicp_total)
  )

res_panel <-
  lp_lin_panel(
    data_set = df_panel,
    endog_data = "hicp_total",
    cumul_mult = TRUE,
    shock = "LCPCASH",
    diff_shock = FALSE,
    panel_model = "within",
    panel_effect = "twoways",
    confint = 1.96,
    hor = 12,
    lags_exog_data = 12
  )

a <- plot_lin(res_panel)

a

res_panel$reg_outputs
