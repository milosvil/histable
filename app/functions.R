#' Generate table with random values
#' 
#' @param n_rows Number of rows.
#'
generate_data <- function(r_rows) {
  tibble(
    V01 = rbeta(r_rows, shape1 = 1.5, shape2 = 0.9),
    V02 = rbeta(r_rows, shape1 = 0.9, shape2 = 1.5),
    V03 = rbeta(r_rows, shape1 = 1, shape2 = 2),
    V04 = rnorm(r_rows),
    V05 = c(rnorm(r_rows/2), rnorm(r_rows/2, 5, 0.5)),
    V06 = rgamma(r_rows, shape = 3),
    V07 = rgamma(r_rows, shape = 6),
    V08 = rgamma(r_rows, shape = 9),
    V09 = rgamma(r_rows, shape = 12),
    V10 = c(rnorm(r_rows/2), rnorm(r_rows/2, 5)),
    V11 = rweibull(r_rows, 4),
    V12 = rweibull(r_rows, 6),
    V13 = rweibull(r_rows, 8),
    V14 = rweibull(r_rows, 10),
    V15 = rchisq(r_rows, 3),
    V16 = rchisq(r_rows, 4),
    V17 = rchisq(r_rows, 5),
    V18 = rchisq(r_rows, 6),
    V19 = rchisq(r_rows, 7),
    V20 = rf(r_rows, 100, 100)
  ) %>%
    round(2)
}


#' Get `hist()` output
#' 
#' @param x Values.
#' @param bins Number of bins.  
#' 
get_hist <- function(x, bins = 21) {
  h <- hist(x, breaks = seq(min(x), max(x), length.out = bins), plot = FALSE)
  h
}


#' Min-max normalization
#'
#' @param x Values.
#' @param min Min value.
#' @param max Max value.
#' 
min_max_normalization <- function(x, min = 1, max = 30) {
  min_max_norm <- (x - min(x)) / (max(x) - min(x))
  round((max-min) * min_max_norm + min)
}


#' Create histogram table
#' 
#' @param mm_norm_counts Min-max normalized counts.
#' @param n_rows_pg Number of rows of a page.
#' @param n_rows_da Number of rows of a table.
#' @param empty Logical. If TRUE all values are zero.
#' @param col_prefix Column name prefix.
#' 
get_hist_table <- function(mm_norm_counts, 
                           n_rows_pg,
                           n_rows_da, 
                           empty = FALSE, 
                           col_prefix = "T") {
  da <- 
    map(mm_norm_counts, ~{
      v <- rep(0, n_rows_pg)
      if (!empty) {
        v[1:.x] <- 1
      }
      rev(v)
    }) %>% 
    set_names(paste0(col_prefix, seq_along(mm_norm_counts))) %>%
    as_tibble()
  
  n_iter <- ceiling(n_rows_da / n_rows_pg)
  
  l_da <- map(1:n_iter, ~{da})
  da <- do.call(rbind, l_da) 
  
  da[1:n_rows_da, ]
}


#' Get y-axis frequencies values
#' 
#' @param n_rows Number of rows of a table.
#' @param h `hist()` output.
#' @param pg_len Page length.
#' 
get_y_axis_freq_values <- function(n_rows, h, pg_len) {
  h_counts <- round(seq(min(h$counts), max(h$counts), length.out = pg_len))
  h_counts_tmp <- rep(NA_real_, length(h_counts))
  h_counts_tmp[seq(1, length(h_counts), 3)] <- h_counts[seq(1, length(h_counts), 3)]
  h_counts <- rev(h_counts_tmp)
  h_counts <- rep(h_counts, ceiling(n_rows / length(h$mids)))[1:n_rows]
  
  h_counts
}


#' Get x-axis values
#' 
#' @param data Data.
#' @param empty_data Data with zeros.
#' @param values Vector of values.
#' 
get_x_axis_values <- function(data, empty_data, values) {
  x_axis_data <- data[1, ]
  n_cols <- ncol(data)
  x_axis_data[1, ] <- matrix(values, ncol = n_cols)
  x_axis_data
}


#' Get x-axis row indices
#' 
#' @param data Data.
#' @param pg_len Page length.
#' 
get_x_axis_row_ids <- function(data, pg_len) {
  ids <- seq(1, nrow(data), pg_len)
  ids_len <- length(ids) - 1
  ids <- ids + lag(0:ids_len)
  ids[-1]
}


#' Insert x-axis values into data
#' 
#' @param data Main table.
#' @param x_axis_values Data returned from `get_x_axis_values()` function.
#' @param x_axis_ids x-axis values indices.
#' 
insert_x_axis_values <- function(data, x_axis_values, x_axis_ids) {
  for (i in x_axis_ids) {
    data <- data %>% add_row(x_axis_values, .before = i)
  }
  
  data
}


#' Set target data values
#' 
#' @param target_data Target data.
#' @param selected_col Index of selected column.
#' 
set_target_values <- function(target_data, selected_col) {
  sel_col_vals <- target_data[[selected_col]]
  target_data[sel_col_vals == 0, selected_col] <- -1
  target_data[sel_col_vals != 0, selected_col] <- 2
  target_data[, -selected_col] <- target_data[, -selected_col] * 10 + 11
  
  target_data
}


#' Create DT table
#' 
#' @param values_data Data with values.
#' @param empty_data Data with zeros.
#' @param t_freq_data Target freq data.
#' @param pal Color pallet.
#'
#' @details 
#' color palette
#' 
#' pal[1] | #8d99ae
#' - hist background color
#' - cell text color
#' 
#' pal[2] | #edf2f4
#' - background
#' - hist cell text color
#' 
#' pal[3] | #2b2d42
#' - selected column
#' - table header
#' - x-axis
#' - y-axis
create_dt_table <- function(data, values_data, empty_data, t_freq_data, pal) {
  brks_1 <- c(0, 1, 10, 20, 100, 200)
  brks_2 <- c(-1, 0, 1, 10, 20, 100, 200)

  bg_clrs_1 <- c(pal[2], pal[1], pal[1], pal[2], pal[1], pal[2], pal[2])
  clrs <- c(pal[3], pal[1], pal[2], pal[3], pal[1], pal[2], pal[2], pal[3])
  fnt_w <- c("bold", "normal", "normal", "bold", "normal", "normal", "normal", "bold")
  fnt_s <- c("12px", "10px", "10px", "12px", "10px", "10px", "10px", "12px")
  brd_lr <- c(stringr::str_glue("2px solid {pal[3]}"), "none", "none",
              stringr::str_glue("2px solid {pal[3]}"), "none", "none", "none", "none")
  
  tar_start <- 1 + ncol(values_data)
  tar_end <- tar_start + ncol(empty_data)
  
  data %>%
    DT::datatable(
      style = "bootstrap4",
      class = "hover",
      rownames = FALSE,
      colnames = c("  ", colnames(values_data), colnames(t_freq_data), colnames(empty_data)),
      selection = list(target = 'column', mode = 'single'),
      callback = DT::JS(stringr::str_glue(
        "$('table.dataTable.no-footer').css('border', '3px solid {pal[2]}');"
      )),
      options = list(
        headerCallback = DT::JS(stringr::str_glue(
          "function(thead) {{",
          "  $(thead).css('font-size', '14px');",
          "  $(thead).closest('thead').find('th').css('border-bottom', '2px solid {pal[3]}');",
          "  $(thead).closest('thead').find('th').css('background', '{pal[2]}');",
          "  $(thead).closest('thead').find('th').css('color', '{pal[3]}');",
          "  $(thead).closest('thead').find('th').css('text-align', 'center');",
          "  $(thead).closest('thead').find('th').css('padding', '3px');",
          "}}"
        )),
        initComplete = JS(
          "function(settings, json) {",
          "$('body').css({'font-family': 'Courier'});",
          "}"
        ),
        pagingType = "simple",
        columnDefs = list(list(targets = tar_start:tar_end, visible = FALSE)),
        pageLength = pg_len + 1,
        ordering = FALSE,
        dom = "tp"
      )) %>%
    formatStyle(columns = 1:ncol(data),
                padding = "1px",
                `font-size` = "10px",
                `line-height` = "16px",
                `vertical-align` = "middle",
                `border-top` = "0px",
                `text-align` = "center") %>%
    formatStyle(columns = 1:(ncol(values_data)+1),
                valueColumns = c(names(t_freq_data), names(empty_data)),
                color = styleInterval(brks_2, clrs),
                backgroundColor = styleInterval(brks_1, bg_clrs_1),
                fontSize = styleInterval(brks_2, fnt_s),
                fontWeight = styleInterval(brks_2, fnt_w),
                borderLeft = styleInterval(brks_2, brd_lr),
                borderRight = styleInterval(brks_2, brd_lr)
    ) 
}

