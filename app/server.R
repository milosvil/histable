function(input, output, session) {
  
  # RV ----
  rv <- reactiveValues(data = data,
                       counter = 1,
                       trigger = 0,
                       selected_col = 99999)
  
  # DT | table ----
  output$table <- DT::renderDataTable({
    
    create_dt_table(data, values_data, empty_data, t_freq_data, pal)
  })
  
  # DT | proxy ----
  proxy <- dataTableProxy('table')
  
  # oE | selected column ----
  observeEvent(input$table_columns_selected, {
    inp_sel_col <- isolate(input$table_columns_selected)
    
    # deselect column
    if (rv$selected_col == inp_sel_col | inp_sel_col == 0) {
      target_data <- empty_data
      
      t_freq_data <<- tibble(
        T_FREQ = 101
      )
      
      x_axis_values <<- 
        get_x_axis_values(
          data, 
          empty_data, 
          values = c(NA_real_, 
                     rep(NA_real_, ncol(empty_data)), 
                     rep(101, ncol(empty_data) + 1))
        )
      
      rv$selected_col <- 99999
      
    # select column
    } else {
      sel_col <- data[[inp_sel_col+1]][-x_axis_ids]
      
      h <- get_hist(sel_col, bins = ncol(values_data) + 1)
      min_max_norm_counts <- min_max_normalization(x = h$counts, 
                                                   min = 1, 
                                                   max = pg_len)
      
      y_axis_freq_vals <- 
        get_y_axis_freq_values(n_rows, h, pg_len)
      
      freq_data <<- tibble(
        FREQ = y_axis_freq_vals
      )
      
      t_freq_data <<- tibble(
        T_FREQ = 201
      )
      
      h_mids <- h$mids
      h_mids[seq(1, ncol(values_data), 2)] <- NA_real_
      
      x_axis_values <<- 
        get_x_axis_values(
          data, 
          empty_data, 
          values = c(NA_real_, 
                     round(h_mids,2), rep(201, ncol(empty_data)+1))
        )
      
      target_data <- 
        get_hist_table(mm_norm_counts = min_max_norm_counts,
                       n_rows_pg = pg_len,
                       n_rows_da = nrow(values_data)) %>%
        set_target_values(inp_sel_col)
      
      rv$selected_col <- inp_sel_col
    }
    
    final_data <- 
      bind_cols(freq_data, 
                values_data, 
                t_freq_data, 
                target_data) %>%
      insert_x_axis_values(x_axis_values, 
                           x_axis_ids)
    
    rv$data <- final_data
    rv$trigger <- rv$trigger + 1
    rv$counter <- 1
  })
  
  # o | animation ----
  observe({
    rv$trigger
    max_iter <- ncol(values_data)
    
    isolate({
      if (rv$trigger > 0) {
        data <<- rv$data
        
        anim_type <- input$anim_type
        tar_cols <- colnames(empty_data)
        val_cols <- colnames(values_data)
        
        tmp <- 
          bind_cols(freq_data, 
                    values_data, 
                    t_freq_data, 
                    empty_data) %>%
          insert_x_axis_values(x_axis_values, 
                               x_axis_ids)
        
        if (anim_type == 1) {
          # no animation
          tmp <- data
          rv$counter <- max_iter
        } else if (anim_type == 2) {
          # slide | left to right
          tmp[, tar_cols[1:rv$counter]] <- data[, tar_cols[(max_iter-rv$counter+1):max_iter]]
        } else if (anim_type == 3) {
          # slide | right to left
          tmp[, tar_cols[(max_iter-rv$counter+1):max_iter]] <- data[, tar_cols[1:rv$counter]]
        } else if (anim_type == 4) {
          # steps | left to right
          tmp[, tar_cols[1:rv$counter]] <- data[, tar_cols[1:rv$counter]]
        } else if (anim_type == 5) {
          # steps | right to left
          tmp[, tar_cols[(max_iter-rv$counter+1):max_iter]] <- data[, tar_cols[(max_iter-rv$counter+1):max_iter]]
        }
        
        data <<- tmp
        
        replaceData(proxy, 
                    data, 
                    resetPaging = FALSE, 
                    rownames = FALSE)
      }
      rv$counter <- rv$counter + 1
    })
    
    if (isolate(rv$counter) <= max_iter){
      invalidateLater(100, session)
    }
  })
  
}
