# Libraries -----------------------------------------------------------------
# Load libraries
library(here)
library(tidyverse)
library(readxl)
library(conflicted)
library(furrr)
library(janitor)


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
