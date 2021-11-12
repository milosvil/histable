library(shiny)
library(dplyr)
library(purrr)
library(DT)
library(shinyjs)

source("functions.R")

# Constants ----

# table page length
pg_len <- 30

# number of rows
n_rows <- 1000

# color palette

# pal[1] | #8d99ae
# - hist background color
# - cell text color

# pal[2] | #edf2f4
# - background
# - hist cell text color

# pal[3] | #2b2d42
# - selected column
# - table header
# - x-axis
# - y-axis
pal <- c("#8d99ae", "#edf2f4", "#2b2d42")

# Data
# 1. values_data  - Main table
# 2. freq_data    - Hist frequencies
# 3. target_data (empty_data) - Values used for styling main table
# 4. t_freq_data  - Values used for styling freq column
# 5. x_axis_values - Hist values row

values_data <- generate_data(n_rows)

freq_data <- tibble(
  FREQ = rep(0, n_rows)
)

t_freq_data <- tibble(
  T_FREQ = req(101, n_rows)
)

empty_data <- 
  get_hist_table(mm_norm_counts = 1:ncol(values_data),
                 n_rows_pg = pg_len,
                 n_rows_da = n_rows,
                 empty = TRUE)

data <- 
  bind_cols(freq_data,
            values_data,
            t_freq_data,
            empty_data)

x_axis_values <- 
  get_x_axis_values(
    data, 
    empty_data, 
    values = c(NA_real_, 
               rep(0, ncol(values_data)), 
               rep(101, 1+ncol(empty_data)))
  )

x_axis_ids <- get_x_axis_row_ids(data, pg_len)

data <- insert_x_axis_values(data, x_axis_values, x_axis_ids)
