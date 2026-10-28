# Libraries -----------------------------------------------------------------
# Load libraries
library(here)
library(tidyverse)
library(readxl)
library(conflicted)
library(furrr)
library(janitor)
library(glue)
library(lpirfs)
library(zoo)
library(patchwork)

# Avoid error if Java is not installed
tryCatch({
  library(RJDemetrax)
}, error = function(e) {
  message("RJDemetra error : install Java if needed")
})


# Remove scientific writing
options(scipen = 999)


# Manage package functions conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::select)


# Import paths --------------------------------------------------------------
source(here("02-codes", "utils", "paths.R"))

# Create folders ------------------------------------------------------------
walk(
  ls(pattern = "^path_.*_folder$") |> mget(),
  \(path) dir.create(path, showWarnings = FALSE, recursive = TRUE)
)


# Import R functions --------------------------------------------------------
walk(
  list.files(path_codes_functions_R_folder,  full.names = TRUE),
  source
)
 

# Setup parallel workers ----------------------------------------------------
plan(multisession, workers = parallelly::availableCores() - 1)


# Set ggplot2 theme ---------------------------------------------------------
theme_set(
  theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 30, color = "black"),
      axis.title = element_text(size = 28, color = "black"),
      axis.text = element_text(size = 22, color = "black"),
      legend.title = element_text(size = 28, color = "black"),
      legend.text = element_text(size = 22, color = "black"),
      legend.key.size = unit(1, "cm"),
      strip.text = element_text(size = 28, color = "black"),
      strip.background = element_rect(fill = "grey90")
    )
)

# Import data ---------------------------------------------------------------
df_main <-
  path_processed_data_df_main |>
  read_csv(show_col_types = FALSE)


# Setup all the specifications for LPs --------------------------------------
# Endogenous variables for each specifications
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

# Exogenous variables for each specification with a copper shock
list_exog_copper <-
  list(
    c("producer_prices_manufacturing_sa", "production_manufacturing_sa",
      "CRUDOIL_log", "DJSTOXX_log"),
    c("producer_prices_manufacturing_sa", "production_manufacturing_sa",
      "CRUDOIL_log", "DJSTOXX_log"),
    c("producer_prices_electricity_sa", "production_electricity_sa",
      "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_industrial_goods_sa", "production_manufacturing_sa", "CRUDOIL_log",
      "DJSTOXX_log"),
    c("hicp_energy_sa", "production_electricity_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "production_mining_sa", "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_industrial_goods_sa", "producer_prices_manufacturing_sa",
      "CRUDOIL_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "producer_prices_electricity_sa", "CRUDOIL_log",
      "DJSTOXX_log"),
    c("hicp_energy_sa", "producer_prices_mining_sa", "CRUDOIL_log", "DJSTOXX_log")
  )

# Exogenous variables for each specification with a oil shock
list_exog_oil <-
  list(
    c("producer_prices_manufacturing_sa", "production_manufacturing_sa",
      "LCPCASH_log", "DJSTOXX_log"),
    c("producer_prices_manufacturing_sa", "production_manufacturing_sa",
      "LCPCASH_log", "DJSTOXX_log"),
    c("producer_prices_electricity_sa", "production_electricity_sa",
      "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_industrial_goods_sa", "production_manufacturing_sa", "LCPCASH_log",
      "DJSTOXX_log"),
    c("hicp_energy_sa", "production_electricity_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "production_mining_sa", "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_industrial_goods_sa", "producer_prices_manufacturing_sa",
      "LCPCASH_log", "DJSTOXX_log"),
    c("hicp_energy_sa", "producer_prices_electricity_sa", "LCPCASH_log",
      "DJSTOXX_log"),
    c("hicp_energy_sa", "producer_prices_mining_sa", "LCPCASH_log", "DJSTOXX_log")
  )
