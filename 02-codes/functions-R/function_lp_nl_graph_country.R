#' Compute non-linear LP results and return a df for 2 different shocks for a
#' specific country
#'
#' Non-linearity is compute with a logistic transition function
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
#' @param switching_var Character of length one specifying the name of the
#' variable to be used as the transition variable in the transition function
#' @return A dataframe with mean and IC for non-linear LP
sub_function_lp_nl_res_country <- function(df, country_name, var_endog,
                                           var_exog_copper, var_exog_oil,
                                           shock_copper, shock_oil,
                                           switching_var) {
  # Df with endogenous variable
  df_country <-
    df |>
    filter(country == country_name)

  # Df with endogenous variable
  df_endog <-
    df_country |>
    select(all_of(var_endog))

  # Df with exogenous variables for copper shocks
  df_exog_copper <-
    df_country |>
    select(all_of(var_exog_copper))

  # Df with exogenous variables for oil shocks
  df_exog_oil <-
    df_country |>
    select(all_of(var_exog_oil)) 

  # Df with the copper shock variable
  df_shock_copper <-
    df_country |>
    select(all_of(shock_copper))

  # Df with the oil shock variable
  df_shock_oil <-
    df_country |>
    select(all_of(shock_oil))

  # Df with the switching (transition) variable
  df_switching <-
    df_country |>
    select(all_of(switching_var))

  # Non-linear LP res for the copper shock
  res_copper <-
    lp_nl_iv(
      endog_data = df_endog,
      shock = df_shock_copper,
      cumul_mult = TRUE,
      lags_endog = 24,
      confint = 1.96,
      hor = 24,
      trend = 1,
      lags_criterion = NaN,
      exog_data = df_exog_copper,
      lags_exog = 24,
      switching = df_switching,
      gamma = 1,
      use_hp = FALSE
    )

  # Non-linear LP res for the oil shock
  res_oil <-
    lp_nl_iv(
      endog_data = df_endog,
      shock = df_shock_oil,
      cumul_mult = TRUE,
      lags_endog = 24,
      confint = 1.96,
      hor = 24,
      trend = 1,
      lags_criterion = NaN,
      exog_data = df_exog_oil,
      lags_exog = 24,
      switching = df_switching,
      gamma = 1,
      use_hp = FALSE
    )

  # Transform LP res for copper into a Dataframe
  df_res_lp_copper <-
    res_copper |>
    keep_at(\(name) str_starts(name, "^irf")) |>
    map(t) |>
    map(as.numeric) |>
    as_tibble() |>
    rename_with(\(x) str_replace(x, "irf", "res_copper"))

  # Transform LP res for oil into a Dataframe
  df_res_lp_oil <-
    res_oil |>
    keep_at(\(name) str_starts(name, "^irf")) |>
    map(t) |>
    map(as.numeric) |>
    as_tibble() |>
    rename_with(\(x) str_replace(x, "irf", "res_oil"))

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



#' Create graph for non-linear LP IRF for copper and oil shocks for different
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
#' @param switching_var Character of length one specifying the name of the
#' variable to be used as the transition variable in the transition function
#' @return A graph with IRFs for all specifications (left panel = Regime 1 and
#' right panel = Regime 2)
function_lp_nl_graph_country <- function(df, country_name, list_endog,
                                          list_exog_copper, list_exog_oil,
                                          shock_copper, shock_oil, switching_var) {
  # Df with results for LP with copper and oil shocks
  # Do 1 LP for each specification. Each specifications are join in rows with
  # the endogenous indicated in the `var_endog` variable of the Df
  lp_nl_indiv <-
    pmap_dfr(
      list(list_endog, list_exog_copper, list_exog_oil),
      \(var_endog, var_exog_copper, var_exog_oil) {
        sub_function_lp_nl_res_country(
          df = df,
          country_name = country_name,
          var_endog = var_endog,
          var_exog_copper= var_exog_copper,
          var_exog_oil = var_exog_oil,
          shock_copper = shock_copper,
          shock_oil = shock_oil,
          switching_var = switching_var
        )
      }
    )

  # NEED TO MODIFY ->  NOT SCALABLE
  # Need to separate variables in 3 groups to have lisible graphs
  # Group with hicp as endogenous variables
  hicp_group <-
    list_endog[1:3]

  # Group with prodcer prices as endogenous variables
  producer_prices_group <-
    list_endog[4:6]

  # group with production as endogenous vaables
  production_group <-
    list_endog[7:9]

  # List containing all the groups
  list_group <- list(hicp_group, producer_prices_group, production_group)

  # Empty list to store graphs for each groups
  list_graph <- list(NA, NA, NA)

  num <- 1

  # Create a graph for each group
  for (group in list_group){
    # Graph for the left panel : regime 1
    graph_s1 <-
      lp_nl_indiv |>
      # Keep only variables for the wanted group
      filter(var_endog %in% group) |>
      ggplot(aes(x = h)) +
       # Area for CI for copper shock
      geom_ribbon(aes(ymin = res_copper_s1_low, ymax = res_copper_s1_up),
                  fill = "red", alpha = 0.2) +
      # Area for CI for oil shock
      geom_ribbon(aes(ymin = res_oil_s1_low, ymax = res_oil_s1_up),
                  fill = "blue", alpha = 0.2) +
      # Mean IRF for copper shock
      geom_line(aes(y = res_copper_s1_mean), color = "red", linewidth = 1) +
      # Mean IRF for oil shock
      geom_line(aes(y = res_oil_s1_mean), color = "blue", linewidth = 1) +
      geom_hline(yintercept =  0, color = "black") +
      labs(
        y = NULL,
        x = NULL,
        title = country_name
      ) +
      theme(
        panel.grid = element_blank()
      ) +
      facet_wrap(facets = vars(var_endog), ncol = 1, scales = "free")

    # Graph for the right panel : regime2
    graph_s2 <-
      lp_nl_indiv |>
      # Keep only variables for the wanted group
      filter(var_endog %in% group) |>
      ggplot(aes(x = h)) +
      # Area for CI for copper shock
      geom_ribbon(aes(ymin = res_copper_s2_low, ymax = res_copper_s2_up),
                  fill = "red", alpha = 0.2) +
      # Area for CI for oil shock
      geom_ribbon(aes(ymin = res_oil_s2_low, ymax = res_oil_s2_up),
                  fill = "blue", alpha = 0.2) +
      # Mean IRF for copper shock
      geom_line(aes(y = res_copper_s2_mean), color = "red", linewidth = 1) +
      # Mean IRF for oil shock
      geom_line(aes(y = res_oil_s2_mean), color = "blue", linewidth = 1) +
      geom_hline(yintercept =  0, color = "black") +
      labs(
        y = NULL,
        x = NULL,
        title = country_name
      ) +
      theme(
        panel.grid = element_blank()
      ) +
      facet_wrap(facets = vars(var_endog), ncol = 1, scales = "free")

    # Store the combine graph in the list
    list_graph[[num]] <- graph_s1 + graph_s2

    num <- num + 1
  }
  
  
  return(
    list(
      df_lp_nl = lp_nl_indiv,
      graph_lp_nl = list_graph
    )
  )
}
