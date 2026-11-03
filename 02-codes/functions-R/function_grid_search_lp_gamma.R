#' Objective function to obtain the gamma parameter which maximize de R² of
#' the first horizon LP
#'
#' @param g Value of gamma (slope parameter) to be tester
#' @param df_endog Tibble containing the endogeneous variable (only one variable)
#' @param df_shock Tibble containing the shock variable (only one variable)
#' @param df_exog Tibble containing exogenous variables
#' @param df_switching Tibble containing the switching variable (only one variable)
#' @return The R² value of the first horizon for the NL-LP
obj_func <- function(g, df_endog, df_shock, df_exog,
                            df_switching) {
  tryCatch({
    # NL-LP with 24 lags for 1 horizon
    res <- lp_nl_iv(
      endog_data     = df_endog,
      shock          = df_shock,
      cumul_mult     = TRUE,
      lags_endog     = 24,
      confint        = 1.96,
      hor            = 1,
      trend          = 1,
      lags_criterion = NaN,
      exog_data      = df_exog,
      lags_exog      = 24,
      switching      = df_switching,
      gamma          = g,
      use_hp         = FALSE
    )

    # Value of the R² for the first equation
    val <- res$diagnostic_list[[1]][1]
    
    # Gestion of infinite, NA or NULL values
    if (is.null(val) || is.na(val) || is.infinite(val)) return(-Inf)
    return(val)
  }, error = function(e) {
    return(-Inf)
  })
}

#' Optimize to find the gamma parameter which maximise the R² for the first
#' horizon for a spcific country and specification
#'
#' Gamma is found through a reasearch grid
#'
#' @param df Tibble containing all relevant data
#' @param country_name Character specifying the name of the country
#' @param var_endog Character specifying the name of he endogenous variable
#' @param var_exog_copper Vector of character specifying the names of the exogenous
#' variables to be used with copper shock
#' @param var_exog_oil Vector of character specifying the names of the exogenous
#' variables to be used with oil shock
#' @param shock_copper Character specifying the name of the copper shock variable
#' @param shock_oil Character specifying the name of the oil shock variable
#' @param switching_var Character specifying the name of the switching variable
#' @return A tibble containing best gamma for oil and copper shock
function_grid_search_lp_gamma_country_specific <- function(df, country_name, var_endog,
                                           var_exog_copper, var_exog_oil,
                                           shock_copper, shock_oil,
                                          switching_var) {
  # Df with all variables
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

  # Define the grid search
  gamma_grid <- seq(0.1, 10, by = 0.1)

  # Evaluate the objective function trough the grid search for copper shock
  grid_results_copper <-
    sapply(
      gamma_grid,
      \(gamma) obj_func(
        g = gamma,
        df_endog = df_endog,
        df_shock = df_shock_copper,
        df_exog = df_exog_copper,
        df_switching = df_switching
      )
    )

  # Extract gamma which maximise the R²
  best_idx_copper   <- which.max(grid_results_copper)
  best_gamma_copper <- gamma_grid[best_idx_copper]

  # Evaluate the objective function trough the grid search for oil shock
  grid_results_oil <-
    sapply(
      gamma_grid,
      \(gamma) obj_func(
        g = gamma,
        df_endog = df_endog,
        df_shock = df_shock_oil,
        df_exog = df_exog_oil,
        df_switching = df_switching
      )
    )

  # Extract gamma which maximise the R²
  best_idx_oil   <- which.max(grid_results_oil)
  best_gamma_oil <- gamma_grid[best_idx_oil]

  # Create the result dataframe
  df_res_gamma_country_specific <-
    tibble(
      gamma_copper = best_gamma_copper,
      gamma_oil = best_gamma_oil,
      country = country_name,
      specification = var_endog
    )

  return(df_res_gamma_country_specific)
}

#' Find the gamma that maximise the R² for the first horizon for a specific
#' country and for all specifications
#'
#' @param df Tibble containing all variables
#' @param country_name Character specifying the country name
#' @param list_endog List containing character indicating the names of
#' the endogenous variables (one per specification)
#' @param list_exog_copper List containing vector of character indicating the
#' names of the exogenous variables to be used for each specification for copper
#' shocks
#' @param list_exog_oil List containing vector of character indicating the
#' names of the exogenous variables to be used for each specification for oil
#' shocks
#' @param shock_copper Character indicating the copper shock variable
#' @param shock_oil Character indicating the oil shock variable
#' @param switching_var Character indicating the switching variable
#' @return A tibble with the best gamma for all specification
function_grid_search_lp_gamma_country <- function(df, country_name, list_endog,
                                                         list_exog_copper, list_exog_oil,
                                                         shock_copper, shock_oil,
                                                         switching_var) {
  # Combine all specifications into one list
  list_param <-
    list(
      list_endog,
      list_exog_copper,
      list_exog_oil
    )

  # Find the best gamma for all specification for a specific country
  df_res_gamma_country <-
    future_pmap_dfr(
      list_param,
      \(var_endog, var_exog_copper, var_exog_oil)
      function_grid_search_lp_gamma_country_specific(
        df = df_main,
        country_name = country_name,
        var_endog = var_endog,
        var_exog_copper = var_exog_copper,
        var_exog_oil = var_exog_oil,
        shock_copper = shock_copper,
        shock_oil = shock_oil,
        switching_var = switching_var
      )
    )

  return(df_res_gamma_country)
}
