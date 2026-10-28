# Setup --------------------------------------------------------------------
source(here::here("02-codes", "utils", "setup.R"))

# Data analysis ------------------------------------------------------------
# Graphs for base and log_diff variables for each country
walk(
  df_main |>pull(country) |>unique(),
  \(country_name) {
    # Df with base variables in level
    df_base_series <-
      df_main |>
      filter(
        country == country_name
      ) |>
      select(
        time, country,
        !c(ends_with("log_diff"), ends_with("sa"), starts_with("CRUDOIL"), starts_with("LCPCASH"), starts_with("DJSTOXX"))
      ) |>
      pivot_longer(
        cols = !c(time, country),
        names_to = "variable",
        values_to = "value"
      ) |>
      mutate(
        variable = paste0(variable, "_sa")
      )

      # Df with seasonal adjustment variables in level
      df_sa_series <-
        df_main |>
        filter(
          country == country_name
        ) |>
        select(time, country, ends_with("sa")) |>
        select(!c(starts_with("CRUDOIL"), starts_with("LCPCASH"), starts_with("DJSTOXX"))) |>
        pivot_longer(
          cols = !c(time, country),
          names_to = "variable",
          values_to = "value"
        )

        # Graph in level
        graph_level <-
          ggplot() +
          geom_line(
            data = df_sa_series,
            aes(x = time, y = value, color = variable),
            linewidth = 1,
            ) +
          geom_line(
            data = df_base_series,
            aes(x = time, y = value),
            color = "grey50",
            linewidth = 1, alpha = 0.5
          ) +
          facet_wrap(variable~., scales = "free") +
          labs(color = NULL) +
          theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 30, color = "black"),
            axis.text = element_text(size = 22, color = "black"),
            axis.title =  element_text(size = 28, color = "black"),
            legend.position = "none",
            strip.text = element_text(size = 28, color = "black"),
            strip.background = element_rect(fill = "grey90")
          ) +
          labs(
            x = country_name,
            y = NULL
          )

        # Save graph in levels
        ggsave(
          here(
            path_output_graphs_description_data_level_folder,
            glue("{country_name}-description-graph-level.png")
          ),
          graph_level,
          width = 30,
          height = 20
        )

        # Graph for log_diff variables (seasonal adjustment)
        graph_log_diff <-
          df_main |>
          filter(
            country == country_name
          ) |>
          select(time, country, ends_with("sa_log_diff")) |>
          select(!c(starts_with("CRUDOIL"), starts_with("LCPCASH"), starts_with("DJSTOXX"))) |>
          pivot_longer(
            cols = !c(time, country),
            names_to = "variable",
            values_to = "value"
          ) |>
          ggplot(aes(x = time, y = value, color = variable)) +
          geom_line(linewidth = 1.2, show.legend = FALSE) +
          facet_wrap(variable~., scales = "free")+
          labs(x = country_name) +
          theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 30, color = "black"),
            axis.text = element_text(size = 22, color = "black"),
            axis.title =  element_text(size = 28, color = "black"),
            legend.position = "none",
            strip.text = element_text(size = 28, color = "black"),
            strip.background = element_rect(fill = "grey90")
          )

          # Save graph in log-diff
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


# Linear LP ----------------------------------------------------------------
# Create and save graphs for Linear LP IRFs for each specification and country
walk(
  df_main |> pull(country) |> unique(),
  \(country_name) {
    function_lp_lin_graph_country(
      df = df_main,
      country_name = country_name,
      list_endog = list_endog,
      list_exog_copper = list_exog_copper,
      list_exog_oil = list_exog_oil,
      shock_copper = "LCPCASH_log_diff",
      shock_oil = "CRUDOIL_log_diff"
    )

    ggsave(
      here(path_output_graphs_lp_lin_country_folder, glue("lp-lin-{country_name}.png")),
      width = 30,
      height = 20
    )
  }
)


# Non-linear LP ------------------------------------------------------------
# Test function for non-linear LP IFRs
a <- function_lp_nl_graph_country(
  df = df_main,
  country_name = "france",
  list_endog = list_endog,
  list_exog_copper = list_exog_copper,
  list_exog_oil = list_exog_oil,
  shock_copper = "LCPCASH_log_diff",
  shock_oil = "CRUDOIL_log_diff",
  switching_var = "CRUDOIL_log_diff"
)

