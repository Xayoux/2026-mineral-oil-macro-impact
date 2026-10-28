#' Compute linear LP results and return a df for 2 different shocks for a specific
#' country
#'
#' @param df Dataframe containing all variables needed
#' @param country_name Character specifying the name of the country in the
#' `country` variable of `df`
#' @param var_endog Character of length one, specifying the name of the variable
#' to be used as the endogenous variable
#' @param var_exog_copper Character vector, specifying the name of the variables
#' to be used as the exogenous variables for the LP with the copper shock
#' @param var_exog_oil Character vector, specifying the name of the variables
#' to be used as the exogenous variables for the LP with the oil shock
#' @param shock_copper Character of length one, specifying the name of the variable
#' to be used as the copper shock variable
#' @param shock_oil Character of length one, specifying the name of the variable
#' to be used as the oil shock variable
#' @return A dataframe with mean and IC for linear LP
sub_function_lp_lin_res_country <- function(df, country_name, var_endog,
                                            var_exog_copper, var_exog_oil,
                                            shock_copper, shock_oil) {
  # Df with endogenous variable
  df_endog <-
    df |>
    filter(country == country_name) |>
    select(all_of(var_endog))

  # Df with exogenous variables for copper shock
  df_exog_copper <-
    df |>
    filter(country == country_name) |>
    select(all_of(var_exog_copper))

  # Df with exogenous variables for oil shocks
  df_exog_oil <-
    df |>
    filter(country == country_name) |>
    select(all_of(var_exog_oil)) 

  # Df with the copper shock variable
  df_shock_copper <-
    df |>
    filter(country == country_name) |>
    select(all_of(shock_copper))

  # Df with the oil shock variable
  df_shock_oil <-
    df |>
    filter(country == country_name) |>
    select(all_of(shock_oil)) 

  # LP res for the copper shock 
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

  # LP res for the oil shock
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

  # Transform LP res for copper into a Dataframe
  df_res_lp_copper <-
    res_copper |>
    keep_at(\(name) str_starts(name, "^irf")) |>
    map(t) |>
    map(as.numeric) |>
    as_tibble() |>
    rename_with(\(x) str_replace(x, "irf_lin", "res_copper"))

  # Transform LP res for oil into a Dataframe
  df_res_lp_oil <-
    res_oil |>
    keep_at(\(name) str_starts(name, "^irf")) |>
    map(t) |>
    map(as.numeric) |>
    as_tibble() |>
    rename_with(\(x) str_replace(x, "irf_lin", "res_oil"))

  # Bind res for copper and oil in a single dataframe
  df_res_lp <-
    df_res_lp_copper |>
    bind_cols(df_res_lp_oil) |>
    mutate(
      h = row_number(),
      var_endog = var_endog # Specify the endogenous variable for loops
    )

  return(df_res_lp)
}


#' Create graph for linear LP IRF for copper and oil shocks for different
#' specifications
#'
#' @param df Dataframe containing all variables needed
#' @param country_name Character specifying the name of the country in the
#' `country` variable of `df`
#' @param list_endog List of characteer, specifying the name of the variable
#' to be used as the endogenous variable for each specification
#' @param list_exog_copper List of character vectors, specifying the name of
#' the variables to be used as the exogenous variables for the LP with
#' the copper shock for each specification (1 vector per specification)
#' @param list_exog_oil List of character vectors, specifying the name of
#' the variables to be used as the exogenous variables for the LP with
#' the oil shock for each specification (1 vector per specification)
#' @param shock_copper Character of length one, specifying the name of the variable
#' to be used as the copper shock variable
#' @param shock_oil Character of length one, specifying the name of the variable
#' to be used as the oil shock variable
#' @return A graph with IRFs for all specifications
function_lp_lin_graph_country <- function(df, country_name, list_endog,
                                          list_exog_copper, list_exog_oil,
                                          shock_copper, shock_oil) {
  # Df with results for LP with copper and oil shocks
  # Do 1 LP for each specification. Each specifications are join in rows with
  # the endogenous indicated in the `var_endog` variable of the Df
  lp_lin_indiv <-
    pmap_dfr(
      list(list_endog, list_exog_copper, list_exog_oil),
      \(var_endog, var_exog_copper, var_exog_oil) {
        sub_function_lp_lin_res_country(
          df = df,
          country_name = country_name,
          var_endog = var_endog,
          var_exog_copper = var_exog_copper,
          var_exog_oil = var_exog_oil,
          shock_copper = shock_copper,
          shock_oil = shock_oil
        )
      }
    )

  # Graph fwith IRF for copper and oil shocks for all specifications
  graph <-
    lp_lin_indiv |>
    ggplot(aes(x = h)) +
    # Area for CI for copper shock
    geom_ribbon(aes(ymin = res_copper_low, ymax = res_copper_up),
                fill = "red", alpha = 0.2) +
    # Area for CI for oil shock
    geom_ribbon(aes(ymin = res_oil_low, ymax = res_oil_up),
                fill = "blue", alpha = 0.2) +
    # Mean IRF for copper shock
    geom_line(aes(y = res_copper_mean), color = "red", linewidth = 1) +
    # Mean IRF for oil shock
    geom_line(aes(y = res_oil_mean), color = "blue", linewidth = 1) +
    geom_hline(yintercept =  0, color = "black") +
    labs(
      y = NULL,
      x = NULL,
      title = country_name
    ) +
    theme(
      panel.grid = element_blank()
    ) +
    facet_wrap(facets = vars(var_endog), ncol = 3, scales = "free")

  return(graph)
}
