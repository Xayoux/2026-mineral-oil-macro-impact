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


# Test LP --------------------------------------------------------------


list_endog <-
  list(
    "hicp_total_sa",
    "hicp_industrial_goods_sa",
    "hicp_energy_sa",
    "producer_prices_manufacturing_sa",
    "producer_prices_electricity_sa",
    "producer_prices_mining_sa",
    "production_manufacturing_sa",
    "production_electricity_sa",
    "production_mining_sa"
  )

list_exog_copper <-
  list(
    c("producer_prices_manufacturing_sa", "production_manufacturing_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("producer_prices_manufacturing_sa", "production_manufacturing_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("producer_prices_electricity_sa", "production_electricity_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_industrial_goods_sa", "production_manufacturing_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "production_electricity_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "production_mining_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_industrial_goods_sa", "producer_prices_manufacturing_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "producer_prices_electricity_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "producer_prices_mining_sa", "CRUDOIL_log", "DJSTOXX_log")
  )

list_exog_oil <-
  list(
    c("producer_prices_manufacturing_sa", "production_manufacturing_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("producer_prices_manufacturing_sa", "production_manufacturing_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("producer_prices_electricity_sa", "production_electricity_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_industrial_goods_sa", "production_manufacturing_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "production_electricity_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "production_mining_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_industrial_goods_sa", "producer_prices_manufacturing_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "producer_prices_electricity_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "producer_prices_mining_sa", "LCPCASH_log", "DJSTOXX_log")
  )



function_lp_lin_graph_country <- function(df, country_name, list_endog, list_exog_copper, list_exog_oil) {
  lp_lin_graph_indiv <-
    pmap(
      list(list_endog, list_exog_copper, list_exog_oil),
      \(var_endog, var_exog_copper, var_exog_oil) {
        df_endog <-
          df |>
          filter(country == country_name) |>
          select(all_of(var_endog))

        df_exog_copper <-
          df |>
          filter(country == country_name) |>
          select(all_of(var_exog_copper))

        df_exog_oil <-
          df |>
          filter(country == country_name) |>
          select(all_of(var_exog_oil)) 

        df_shock_copper <-
          df |>
          filter(country == country_name) |>
          select(LCPCASH_log_diff)

        df_shock_oil <-
          df |>
          filter(country == country_name) |>
          select(CRUDOIL_log_diff) 

        res_copper <-
          lp_lin_iv(
            endog_data = df_endog,
            shock = df_shock_copper,
            cumul_mult = TRUE,
            lags_endog = 24,
            confint = 1.96,
            hor = 24,
            trend = 1,
            lags_criterion = NaN,
            exog_data = df_exog_copper,
            lags_exog = 24
          )

        res_oil <-
          lp_lin_iv(
            endog_data = df_endog,
            shock = df_shock_oil,
            cumul_mult = TRUE,
            lags_endog = 24,
            confint = 1.96,
            hor = 24,
            trend = 1,
            lags_criterion = NaN,
            exog_data = df_exog_oil,
            lags_exog = 24
          )

        tibble(
          res_copper_mean = res_copper$irf_lin_mean |> t() |> as.numeric(),
          res_copper_low = res_copper$irf_lin_low |> t() |> as.numeric(),
          res_copper_up = res_copper$irf_lin_up |> t() |> as.numeric(),
          res_oil_mean = res_oil$irf_lin_mean |> t() |> as.numeric(),
          res_oil_low = res_oil$irf_lin_low |> t() |> as.numeric(),
          res_oil_up = res_oil$irf_lin_up |> t() |> as.numeric()
        ) |>
          mutate(
            h = row_number()
          ) |>
          ggplot(aes(x = h)) +
          geom_ribbon(aes(ymin = res_copper_low, ymax = res_copper_up), fill = "red", alpha = 0.2) +
          geom_ribbon(aes(ymin = res_oil_low, ymax = res_oil_up), fill = "blue", alpha = 0.2) +
          geom_line(aes(y = res_copper_mean), color = "red", linewidth = 1) +
          geom_line(aes(y = res_oil_mean), color = "blue", linewidth = 1) +
          geom_hline(yintercept =  0, color = "black") +
          labs(
            y = NULL,
            x = NULL,
            title = var_endog
          ) +
          theme(
            panel.grid = element_blank()
          )
      }
    )

  wrap_plots(lp_lin_graph_indiv, ncol = 3) +
    plot_annotation(title = country_name)
}


walk(
  df_main |> pull(country) |> unique(),
  \(country_name) {
    function_lp_lin_graph_country(
      df = df_main,
      country_name = country_name,
      list_endog = list_endog,
      list_exog_copper = list_exog_copper,
      list_exog_oil = list_exog_oil
    )

    ggsave(
      here(path_output_graphs_lp_lin_country_folder, glue("lp-lin-{country_name}.png")),
      width = 30,
      height = 20
    )
  }
)


df_test <-
  df_main |>
  filter(country == "france")

endog <-
  df_test |>
  select(hicp_total_sa)

exog_copper <-
  df_test |>
  select(production_manufacturing_sa, producer_prices_manufacturing_sa, CRUDOIL_log, DJSTOXX_log)

exog_oil <-
  df_test |>
  select(production_manufacturing_sa, producer_prices_manufacturing_sa, LCPCASH_log, DJSTOXX_log)

shock_copper <-
  df_test |>
  select(LCPCASH_log_diff)

shock_oil <-
  df_test |>
  select(CRUDOIL_log_diff)

switching <-
  df_test |>
  select(CRUDOIL_log_diff)


res_copper <-
  lp_lin_iv(
    endog_data = endog,
    shock = shock_copper,
    cumul_mult = TRUE,
    lags_endog = 24,
    confint = 1.96,
    hor = 24,
    trend = 1,
    lags_criterion = NaN,
    exog_data = exog_copper,
    lags_exog = 24
  )

res_oil <-
  lp_lin_iv(
    endog_data = endog,
    shock = shock_oil,
    cumul_mult = TRUE,
    lags_endog = 24,
    confint = 1.96,
    hor = 24,
    trend = 1,
    lags_criterion = NaN,
    exog_data = exog_oil,
    lags_exog = 24
  )



tibble(
  res_copper_mean = res_copper$irf_lin_mean |> t() |> as.numeric(),
  res_copper_low = res_copper$irf_lin_low |> t() |> as.numeric(),
  res_copper_up = res_copper$irf_lin_up |> t() |> as.numeric(),
  res_oil_mean = res_oil$irf_lin_mean |> t() |> as.numeric(),
  res_oil_low = res_oil$irf_lin_low |> t() |> as.numeric(),
  res_oil_up = res_oil$irf_lin_up |> t() |> as.numeric()
) |>
  mutate(
    h = row_number()
  ) |>
  ggplot(aes(x = h)) +
  # Aires d'intervalle de confiance (en premier pour rester en arrière-plan)
  geom_ribbon(aes(ymin = res_copper_low, ymax = res_copper_up), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = res_oil_low, ymax = res_oil_up), fill = "blue", alpha = 0.2) +
  # Lignes de moyenne
  geom_line(aes(y = res_copper_mean), color = "red", linewidth = 1) +
  geom_line(aes(y = res_oil_mean), color = "blue", linewidth = 1) +
  geom_hline(yintercept =  0, color = "black") +
  theme(
    panel.grid = element_blank()
  )


plot_lin(res_oil)


dim(res_copper$irf_lin_mean)










a <- plot_lin(res)

a

res_nl <-
  lp_nl_iv(
    endog_data = endog,
    shock = shock,
    cumul_mult = TRUE,
    lags_endog = 24,
    confint = 1.96,
    hor = 24,
    trend = 1,
    lags_criterion = NaN,
    exog_data = exog,
    lags_exog = 24,
    switching = switching,
    use_logistic = TRUE,
    gamma = 10,
    use_hp = FALSE
  )

b <- plot_nl(res_nl)

b$gg_s1[[1]] + b$gg_s2[[1]]


df_endog_level <-
  df_test |>
  select(!c(time, country)) |>
  select(
    !ends_with("log_diff")
  ) |>
  print()

df <-
  df_test |>
  select(
    hicp_industrial_goods_sa, producer_prices_manufacturing_sa, production_manufacturing_sa, LCPCASH, CRUDOIL, DJSTOXX,
    LCPCASH_log_diff
  ) |>
  mutate(log_LCPCASH = log(LCPCASH), log_CRUDOIL = log(CRUDOIL)) |>
  select(!c(LCPCASH, CRUDOIL)) |>
  drop_na()

df_cumul <-
  df |>
  mutate(
    hicp_industrial_goods_cumul = cumsum(hicp_industrial_goods),
    LCPCASH_log_diff_cumul = cumsum(LCPCASH_log_diff)
  ) |>
  relocate(hicp_industrial_goods_cumul, LCPCASH_log_diff_cumul) |>
  select(!c(hicp_industrial_goods, LCPCASH_log_diff))

res <-
  lp_lin_iv(
    endog_data = df |> select(hicp_industrial_goods_sa),
    shock = df |> select(LCPCASH_log_diff),
    cumul_mult = TRUE,
    lags_endog = 24,
    confint = 1.96,
    hor = 24,
    trend = 1,
    lags_criterion = NaN,
    exog_data = df |>select(!c(hicp_industrial_goods_sa, log_LCPCASH)),
    lags_exog = 24
  )

res_cumul <-
  lp_lin_iv(
    endog_data = df_cumul |> select(hicp_industrial_goods_cumul),
    shock = df_cumul |> select(LCPCASH_log_diff_cumul),
    cumul_mult = FALSE,
    lags_endog = 24,
    confint = 1.96,
    hor = 24,
    trend = 1,
    lags_criterion = NaN,
    exog_data = df_cumul |>select(!c(hicp_industrial_goods_cumul, log_LCPCASH, LCPCASH_log_diff_cumul)),
    lags_exog = 24
  )



a <- plot_lin(res)
b <- plot_lin(res_cumul)


a[[1]] + b[[1]]

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


# TEst delta method ------------------------------------------------------
library(msm)

## Simple linear regression, E(y) = alpha + beta x 
x <- 1:100
y <- rnorm(100, 4*x, 5)
toy.lm <- lm(y ~ x)
estmean <- coef(toy.lm)
estvar <- summary(toy.lm)$cov.unscaled * summary(toy.lm)$sigma^2

## Estimate of (1 / (alphahat + betahat))
1 / (estmean[1] + estmean[2])
## Approximate standard error
deltamethod (~ 1 / (x1 + x2), estmean, estvar) 

## We have a variable z we would like to use within the formula.
z <- 1
## deltamethod (~ z / (x1 + x2), estmean, estvar) will not work.
## Instead, build up the formula as a string, and convert to a formula.
form <- sprintf("~ %f / (x1 + x2)", z)
form
deltamethod(as.formula(form), estmean, estvar)

(estmean[1] / estmean[2])
## Approximate standard error
deltamethod (~ (x1 / x2), estmean, estvar)

# Test désaisonaliser ----------------------------
library(RJDemetra)

# Exemple avec une série temporelle mensuelle (AirPassengers)
data("AirPassengers")

# Traitement avec la spécification standard Eurostat (RSA5c)
res_x13 <- x13(AirPassengers, spec = "RSA5c")

# Inspection du modèle (modèle ARIMA, outliers détectés, régresseurs)
summary(res_x13)

# Extraction de la série corrigée des variations saisonnières (CVS / SA)
serie_sa <- res_x13$final$series[, "sa"]

# Visualisation native de la décomposition
plot(res_x13)

# Superposition de la série brute et de la série ajustée
plot(AirPassengers, col = "grey50", lty = 2, ylab = "Valeur", main = "Désaisonnalisation (Norme Eurostat)")
lines(serie_sa, col = "blue", lwd = 2)
legend("topleft", legend = c("Brute", "Désaisonnalisée (SA)"), 
       col = c("grey50", "blue"), lty = c(2, 1), lwd = c(1, 2))

plot(AirPassengers)

res_x13 <- df_main |>
  filter(country == "france") |>
  select(hicp_total) |>
  ts(frequency = 12, start = c(1996,1)) |>
  x13()


serie_sa <- res_x13$final$series[, "sa"]

serie_y <- res_x13$final$series[, "y"]


plot(serie_y, col = "grey50", lty = 2, ylab = "Valeur", main = "Désaisonnalisation (Norme Eurostat)")
lines(serie_sa, col = "blue", lwd = 2)
legend("topleft", legend = c("Brute", "Désaisonnalisée (SA)"), 
       col = c("grey50", "blue"), lty = c(2, 1), lwd = c(1, 2))

forecast::ggseasonplot(df_main |>
                         filter(country == "malta") |>
                         select(hicp_total) |>
                         ts(frequency = 12, start = c(1996,1)))
