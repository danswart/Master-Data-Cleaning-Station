#####  CHUNK 4 and 5  #####

  # Create reactive plot for run chart with IMPROVED data processing
  run_plot <- reactive({
    data <- filtered_data()
    req(data)

    # IMPROVED: Better data validation
    if(!"value" %in% names(data)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No 'value' column found in data", size = 6) + theme_void())
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # IMPROVED: Safe numeric conversion and validation
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if(all(is.na(data$value))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid numeric values in 'value' column", size = 6) + theme_void())
    }

    title_text <- if(is.null(input$run_title) || input$run_title == "") "Run Chart" else input$run_title
    subtitle_text <- if(is.null(input$run_subtitle)) "" else input$run_subtitle
    caption_text <- if(is.null(input$run_caption)) "" else input$run_caption

    # IMPROVED: Safe statistical calculations
    median_value <- safe_median(data$value)

    if(is.na(median_value)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cannot calculate median - insufficient data", size = 6) + theme_void())
    }

    # Calculate date range for proper text positioning
    date_range <- max(data$date) - min(data$date)
    start_text_pos <- min(data$date) - date_range * 0.05  # 5% before start
    end_text_pos <- max(data$date) + date_range * 0.05    # 5% after end

    # Smart date formatting
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE))
      if (date_range_days > 365*5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create run chart with fixed formatting standards
    ggplot(data, aes(x = date, y = value)) +

      # Lines: darkgray, linewidth 1.2
      geom_line(color = "darkgray", linewidth = 1.2) +

      # Points: blue, size 2.5
      geom_point(color = "blue", size = 2.5) +

      # Median reference line
      geom_hline(yintercept = median_value, color = "red", linewidth = 1, linetype = "solid", alpha = 0.8) +

      # Median label above the line
      geom_text(aes(x = min(date), y = median_value, label = "Median"),
                color = "red", vjust = -0.5, hjust = 0, size = 8, fontface = "bold") +

      # Y-axis formatting: conditional based on user selection
      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.10, 0.10))
      ) +

      # X-axis formatting
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(add = 30)
      ) +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "Date", y = "Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })





  ##### CHUNK 5 #####

  # Render run chart
  output$run_chart <- renderPlot({
    run_plot()
  })

  # Create reactive plot for line chart with IMPROVED data processing
  line_plot <- reactive({
    data <- filtered_data()
    req(data)

    # IMPROVED: Better data validation
    if(!"value" %in% names(data)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No 'value' column found in data", size = 6) + theme_void())
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # Use selected grouping variable
    if(is.null(input$grouping_var) || input$grouping_var == "" || !input$grouping_var %in% names(data)) {
      return(NULL)
    }

    group_var <- input$grouping_var

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if(all(is.na(data$value))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid numeric values in 'value' column", size = 6) + theme_void())
    }

    title_text <- if(is.null(input$line_title) || input$line_title == "") "Line Chart" else input$line_title
    subtitle_text <- if(is.null(input$line_subtitle)) "" else input$line_subtitle
    caption_text <- if(is.null(input$line_caption)) "" else input$line_caption

    # Smart date formatting
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE))
      if (date_range_days > 365*5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create data for end-of-line labels
    label_data <- data %>%
      group_by(!!sym(group_var)) %>%
      filter(date == max(date)) %>%
      ungroup()

    # Create line chart
    ggplot(data, aes(x = date, y = value, color = !!sym(group_var))) +

      geom_line(linewidth = 1.2) +

      scale_color_manual(values = chart_colors) +

      # End-of-line labels
      geom_text(data = label_data,
                aes(x = date, y = value, label = !!sym(group_var), color = !!sym(group_var)),
                hjust = -0.1, vjust = 0.5, size = 6, fontface = "bold") +

      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.10, 0.10))
      ) +

      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.05, 0.15))
      ) +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "Date", y = "Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  # Render line chart
  output$line_chart <- renderPlot({
    line_plot()
  })


  # FIXED: Create reactive plot for expectation chart with IMPROVED runs analysis and data processing
  # UPDATED: Added auto-correlation adjustment option with horizontal annotation boxes
  control_plot <- reactive({
    data <- filtered_data()
    req(data)

    # IMPROVED: Better data validation
    if(!"value" %in% names(data)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No 'value' column found in data", size = 6) + theme_void())
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if(all(is.na(data$value))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid numeric values in 'value' column", size = 6) + theme_void())
    }

    title_text <- if(is.null(input$control_title) || input$control_title == "") "Untrended Expectation Chart" else input$control_title
    subtitle_text <- if(is.null(input$control_subtitle)) "" else input$control_subtitle
    caption_text <- if(is.null(input$control_caption)) "" else input$control_caption

    # Better recalculation logic with date range checking
    recalc_enabled <- !is.null(input$enable_recalc) && input$enable_recalc &&
                     !is.null(input$recalc_date) &&
                     input$recalc_date >= min(data$date, na.rm = TRUE) &&
                     input$recalc_date <= max(data$date, na.rm = TRUE)

    # Check if auto-correlation adjustment is enabled
    use_autocorr <- !is.null(input$use_autocorr_modifier) && input$use_autocorr_modifier

    # Get correlation values if needed
    corr_vals <- if(use_autocorr) correlation_values() else NULL
    r_lag1 <- if(!is.null(corr_vals) && !is.na(corr_vals$r_lag1)) corr_vals$r_lag1 else NA

    if(recalc_enabled) {
      # RECALCULATION MODE: Split data and calculate separate expectation limits
      recalc_date <- input$recalc_date

      # Split data into two segments
      data_before <- data[data$date < recalc_date, ]
      data_after <- data[data$date >= recalc_date, ]

      # IMPROVED: Calculate original expectation chart statistics with safe functions
      emp_cl_orig <- safe_mean(data$value)

      # Calculate sigma based on whether auto-correlation adjustment is enabled
      if(use_autocorr && !is.na(r_lag1)) {
        # Calculate moving ranges for the entire dataset
        avg_mr_orig <- calculate_moving_ranges(data$value)
        if(!is.na(avg_mr_orig) && abs(r_lag1) < 0.999) {  # Avoid division by zero
          # Apply auto-correlation adjustment formula: σ = R-bar / (d2 * √(1 - r²))
          sigma_orig <- avg_mr_orig / (1.128 * sqrt(1 - r_lag1^2))
        } else {
          # Fallback to standard deviation if moving range fails or r is too close to 1
          sigma_orig <- safe_sd(data$value)
        }
      } else {
        # Standard calculation using moving range method for consistency
        avg_mr_orig <- calculate_moving_ranges(data$value)
        if(!is.na(avg_mr_orig)) {
          # Standard moving range sigma calculation
          sigma_orig <- avg_mr_orig / 1.128
        } else {
          # Fallback to standard deviation if moving range fails
          sigma_orig <- safe_sd(data$value)
        }
      }

      emp_ucl_orig <- emp_cl_orig + 3 * sigma_orig
      emp_lcl_orig <- emp_cl_orig - 3 * sigma_orig

      # Calculate recalculated expectation chart statistics
      if(nrow(data_after) >= 3) {
        emp_cl_recalc <- safe_mean(data_after$value)

        # Calculate sigma for recalculated segment
        if(use_autocorr && !is.na(r_lag1)) {
          # Calculate moving ranges for the after-recalc segment
          avg_mr_recalc <- calculate_moving_ranges(data_after$value)
          if(!is.na(avg_mr_recalc) && abs(r_lag1) < 0.999) {  # Avoid division by zero
            # Apply auto-correlation adjustment formula: σ = R-bar / (d2 * √(1 - r²))
            sigma_recalc <- avg_mr_recalc / (1.128 * sqrt(1 - r_lag1^2))
          } else {
            # Fallback to standard deviation if moving range fails or r is too close to 1
            sigma_recalc <- safe_sd(data_after$value)
          }
        } else {
          # Standard calculation using moving range method for consistency
          avg_mr_recalc <- calculate_moving_ranges(data_after$value)
          if(!is.na(avg_mr_recalc)) {
            # Standard moving range sigma calculation
            sigma_recalc <- avg_mr_recalc / 1.128
          } else {
            # Fallback to standard deviation if moving range fails
            sigma_recalc <- safe_sd(data_after$value)
          }
        }

        emp_ucl_recalc <- emp_cl_recalc + 3 * sigma_recalc
        emp_lcl_recalc <- emp_cl_recalc - 3 * sigma_recalc
      } else {
        emp_cl_recalc <- emp_cl_orig
        sigma_recalc <- sigma_orig
        emp_ucl_recalc <- emp_ucl_orig
        emp_lcl_recalc <- emp_lcl_orig
      }

      # Add expectation chart columns to data
      data$emp_cl_orig <- emp_cl_orig
      data$emp_ucl_orig <- emp_ucl_orig
      data$emp_lcl_orig <- emp_lcl_orig
      data$emp_cl_recalc <- emp_cl_recalc
      data$emp_ucl_recalc <- emp_ucl_recalc
      data$emp_lcl_recalc <- emp_lcl_recalc
      data$recalc_date <- recalc_date

      # Calculate sigma signals using appropriate limits for each segment
      data$sigma_signals <- ifelse(data$date < recalc_date,
                                  data$value > emp_ucl_orig | data$value < emp_lcl_orig,
                                  data$value > emp_ucl_recalc | data$value < emp_lcl_recalc)

      # Use improved runs analysis for recalculation mode
      data$runs_signal <- detect_runs_signals_recalc(data$value, emp_cl_orig, emp_cl_recalc, data$date, recalc_date)

      # Create annotation data for original limits - HORIZONTAL FORMAT
      annotation_text_orig <- paste0("Original: UCL = ", round(emp_ucl_orig, 2),
                                    " | CL = ", round(emp_cl_orig, 2),
                                    " | LCL = ", round(emp_lcl_orig, 2))
      if(use_autocorr && !is.na(r_lag1)) {
        annotation_text_orig <- paste0(annotation_text_orig,
                                      " | σ = ", round(sigma_orig, 3),
                                      " (autocorr-adjusted, r = ", round(r_lag1, 3), ")")
      }

      annotation_data_orig <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
        y_pos = emp_ucl_orig + (emp_ucl_orig - emp_lcl_orig) * 0.15,  # Moved higher
        label = annotation_text_orig
      )

      # Move recalculated annotation box to the right - HORIZONTAL FORMAT
      annotation_text_recalc <- paste0("Recalculated (from ", format(recalc_date, "%Y-%m-%d"), "): UCL = ", round(emp_ucl_recalc, 2),
                                      " | CL = ", round(emp_cl_recalc, 2),
                                      " | LCL = ", round(emp_lcl_recalc, 2))
      if(use_autocorr && !is.na(r_lag1)) {
        annotation_text_recalc <- paste0(annotation_text_recalc,
                                        " | σ = ", round(sigma_recalc, 3),
                                        " (autocorr-adjusted)")
      }

      annotation_data_recalc <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.55,
        y_pos = emp_lcl_orig - (emp_ucl_orig - emp_lcl_orig) * 0.08,
        label = annotation_text_recalc
      )

      # Create annotation data for runs analysis
      annotation_data_runs <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
        y_pos = emp_lcl_orig - (emp_ucl_orig - emp_lcl_orig) * 0.15,
        label = ifelse(any(data$runs_signal, na.rm = TRUE), "Runs Signal Detected", "No Runs Signal")
      )

    } else {
      # STANDARD MODE: Original untrended expectation chart
      # IMPROVED: Safe statistical calculations
      emp_cl <- safe_mean(data$value)

      # Calculate sigma based on whether auto-correlation adjustment is enabled
      if(use_autocorr && !is.na(r_lag1)) {
        # Calculate average moving range
        avg_mr <- calculate_moving_ranges(data$value)
        if(!is.na(avg_mr) && abs(r_lag1) < 0.999) {  # Avoid division by zero
          # Apply auto-correlation adjustment formula: σ = R-bar / (d2 * √(1 - r²))
          # where d2 = 1.128 for moving range of 2 consecutive points
          sigma <- avg_mr / (1.128 * sqrt(1 - r_lag1^2))
        } else {
          # Fallback to standard deviation if moving range calculation fails or r is too close to 1
          sigma <- safe_sd(data$value)
        }
      } else {
        # Standard calculation using moving range method for consistency
        avg_mr <- calculate_moving_ranges(data$value)
        if(!is.na(avg_mr)) {
          # Standard moving range sigma calculation: σ = R-bar / d2
          sigma <- avg_mr / 1.128
        } else {
          # Fallback to standard deviation if moving range calculation fails
          sigma <- safe_sd(data$value)
        }
      }

      emp_ucl <- emp_cl + 3 * sigma
      emp_lcl <- emp_cl - 3 * sigma

      # Add expectation chart columns to data
      data$emp_cl <- emp_cl
      data$emp_ucl <- emp_ucl
      data$emp_lcl <- emp_lcl

      # Calculate sigma signals
      data$sigma_signals <- data$value > emp_ucl | data$value < emp_lcl

      # Use improved runs analysis
      data$runs_signal <- detect_runs_signals(data$value, rep(emp_cl, nrow(data)), data$date)

      # Create annotation data for limits - HORIZONTAL FORMAT
      annotation_text <- paste0("UCL = ", round(emp_ucl, 2), " | CL = ", round(emp_cl, 2), " | LCL = ", round(emp_lcl, 2))
      if(use_autocorr && !is.na(r_lag1)) {
        # Add auto-correlation information to annotation
        avg_mr_display <- if(exists("avg_mr") && !is.na(avg_mr)) round(avg_mr, 3) else "N/A"
        base_sigma <- if(exists("avg_mr") && !is.na(avg_mr)) round(avg_mr / 1.128, 3) else "N/A"
        annotation_text <- paste0(annotation_text,
                                 " | σ = ", round(sigma, 3),
                                 " (autocorr-adjusted) | Base σ = ", base_sigma,
                                 " | r = ", round(r_lag1, 3),
                                 " | avg MR = ", avg_mr_display)
      } else {
        # Show moving range info for standard calculation too
        avg_mr_display <- if(exists("avg_mr") && !is.na(avg_mr)) round(avg_mr, 3) else "N/A"
        annotation_text <- paste0(annotation_text,
                                 " | σ = ", round(sigma, 3),
                                 " (moving range method) | avg MR = ", avg_mr_display)
      }

      annotation_data_limits <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
        y_pos = emp_ucl + (emp_ucl - emp_lcl) * 0.15,  # Moved higher
        label = annotation_text
      )

      # Create annotation data for runs analysis
      annotation_data_runs <- data.frame(
        x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
        y_pos = emp_lcl - (emp_ucl - emp_lcl) * 0.05,
        label = ifelse(any(data$runs_signal, na.rm = TRUE), "Runs Signal Detected", "No Runs Signal")
      )
    }

    # Smart date formatting
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE))
      if (date_range_days > 365*5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create expectation chart plot
    p <- ggplot(data, aes(x = date, y = value)) +

      # Connect the dots with lines
      geom_line(color = "darkgray", linewidth = 1.2) +

      # Points colored by sigma signals
      geom_point(aes(color = sigma_signals), size = 2.5) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"))

    if(recalc_enabled) {
      # RECALCULATION MODE: Show both sets of expectation limits

      # Determine if any runs signal exists for line style
      any_runs_signal <- any(data$runs_signal, na.rm = TRUE)
      centerline_style <- if(any_runs_signal) "dashed" else "solid"

      # Original centerline (before recalc date)
      p <- p + geom_line(aes(y = ifelse(date < recalc_date, emp_cl_orig, NA)),
                        color = "blue", linewidth = 1, linetype = centerline_style) +

      # Recalculated centerline (after recalc date)
      geom_line(aes(y = ifelse(date >= recalc_date, emp_cl_recalc, NA)),
               color = "green", linewidth = 1, linetype = centerline_style) +

      # Original expectation limits (before recalc date)
      geom_line(aes(y = ifelse(date < recalc_date, emp_ucl_orig, NA)), color = "red", linetype = "solid", linewidth = 1) +
      geom_line(aes(y = ifelse(date < recalc_date, emp_lcl_orig, NA)), color = "red", linetype = "solid", linewidth = 1) +

      # Recalculated expectation limits (after recalc date)
      geom_line(aes(y = ifelse(date >= recalc_date, emp_ucl_recalc, NA)), color = "red", linetype = "dashed", linewidth = 1) +
      geom_line(aes(y = ifelse(date >= recalc_date, emp_lcl_recalc, NA)), color = "red", linetype = "dashed", linewidth = 1) +

      # Vertical line at recalculation point
      geom_vline(xintercept = recalc_date, color = "purple", linetype = "dotted", linewidth = 1.5, alpha = 0.7) +

      # Rich text annotations for both sets of limits - HORIZONTAL FORMAT
      ggtext::geom_richtext(
        data = annotation_data_orig,
        aes(x = x_pos, y = y_pos, label = label),
        size = 4, color = "black", hjust = 0, vjust = 1,
        fill = "lightblue", label.color = "black",
        label.padding = grid::unit(c(0.2, 0.5, 0.2, 0.5), "lines")
      ) +

      ggtext::geom_richtext(
        data = annotation_data_recalc,
        aes(x = x_pos, y = y_pos, label = label),
        size = 4, color = "black", hjust = 0, vjust = 1,
        fill = "lightgreen", label.color = "black",
        label.padding = grid::unit(c(0.2, 0.5, 0.2, 0.5), "lines")
      ) +

      ggtext::geom_richtext(
        data = annotation_data_runs,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5, color = "black", hjust = 0, vjust = 0,
        fill = "lightyellow", label.color = "black"
      ) +

      # Labels for original limits
      geom_text(aes(x = dplyr::first(date), y = dplyr::first(emp_cl_orig), label = "Original Expectation"),
                color = "blue", vjust = -1, hjust = 1.1, size = 5) +

      # Labels for recalculated limits
      geom_text(aes(x = recalc_date, y = emp_cl_recalc, label = "Recalc Expectation"),
                color = "green", vjust = -1, hjust = -0.1, size = 5) +

      # Recalculation point label
      geom_text(aes(x = recalc_date, y = max(c(emp_ucl_orig, emp_ucl_recalc), na.rm = TRUE), label = "Recalc Point"),
                color = "purple", vjust = -0.5, hjust = 0.5, size = 5, fontface = "bold")

    } else {
      # STANDARD MODE: Original expectation chart display

      # Determine if any runs signal exists for line style
      any_runs_signal <- any(data$runs_signal, na.rm = TRUE)
      centerline_style <- if(any_runs_signal) "dashed" else "solid"

      p <- p +
      # Center line with linetype based on runs signals
      geom_line(aes(y = emp_cl), color = "blue", linewidth = 1, linetype = centerline_style) +

      # Upper and lower expectation limits
      geom_line(aes(y = emp_ucl), color = "red", linetype = "solid", linewidth = 1) +
      geom_line(aes(y = emp_lcl), color = "red", linetype = "solid", linewidth = 1) +

      # Rich text annotations - HORIZONTAL FORMAT
      ggtext::geom_richtext(
        data = annotation_data_limits,
        aes(x = x_pos, y = y_pos, label = label),
        size = 4, color = "black", hjust = 0, vjust = 1,
        fill = "lightblue", label.color = "black",
        label.padding = grid::unit(c(0.2, 0.5, 0.2, 0.5), "lines")
      ) +

      ggtext::geom_richtext(
        data = annotation_data_runs,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5, color = "black", hjust = 0, vjust = 0,
        fill = "lightyellow", label.color = "black"
      ) +

      # Standard labels
      geom_text(aes(x = dplyr::first(date), y = dplyr::first(emp_cl), label = "Avg Expectation"),
                color = "blue", vjust = -1, hjust = 1.1, size = 5) +

      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_cl),
                    label = format(round(dplyr::last(emp_cl), 2), nsmall = 2)),
                color = "blue", vjust = 0, hjust = -0.5, size = 5) +

      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(emp_ucl), label = "Upper Expectation"),
                color = "red", vjust = -1, hjust = 1.1, size = 5) +

      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_ucl),
                    label = format(round(dplyr::last(emp_ucl), 2), nsmall = 2)),
                color = "red", vjust = -1, hjust = -0.5, size = 5) +

      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(emp_lcl), label = "Lower Expectation"),
                color = "red", vjust = 1.75, hjust = 1.1, size = 5) +

      geom_text(aes(x = dplyr::last(date), y = dplyr::last(emp_lcl),
                    label = format(round(dplyr::last(emp_lcl), 2), nsmall = 2)),
                color = "red", vjust = 1.5, hjust = -0.5, size = 5)
    }

    # Complete the plot with formatting
    p <- p +
      # Y-axis formatting with generous space for labels
      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.20, 0.20))
      ) +

      # X-axis formatting
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.1, 0.1))
      ) +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "Date", y = "Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )

    return(p)
  })



  # Create reactive plot for bar chart with IMPROVED data processing
  bar_plot <- reactive({
    data <- filtered_data()
    req(data)

    # IMPROVED: Better data validation
    if(!"value" %in% names(data)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No 'value' column found in data", size = 6) + theme_void())
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # Use selected grouping variable
    if(is.null(input$grouping_var) || input$grouping_var == "" || !input$grouping_var %in% names(data)) {
      return(NULL)
    }

    group_var <- input$grouping_var

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if(all(is.na(data$value))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid numeric values in 'value' column", size = 6) + theme_void())
    }

    # Calculate averages across all user-selected dates for each group using safe functions
    bar_data <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(value = safe_mean(value), .groups = 'drop')

    title_text <- if(is.null(input$bar_title) || input$bar_title == "") "Bar Chart" else input$bar_title
    subtitle_text <- if(is.null(input$bar_subtitle)) "" else input$bar_subtitle
    caption_text <- if(is.null(input$bar_caption)) "" else input$bar_caption

    # Create bar chart
    ggplot(bar_data, aes(x = reorder(!!sym(group_var), value), y = value, fill = !!sym(group_var))) +
      geom_col(width = 0.7, alpha = 0.8) +

      # Add value labels at the end of each bar
      geom_text(aes(label = if(input$format_as_percentage) {
        scales::percent(value, accuracy = 0.1)
      } else {
        format(round(value, 2), big.mark = ",")
      }),
      hjust = -0.1, vjust = 0.5, size = 6, color = "black", fontface = "bold") +

      scale_fill_manual(values = chart_colors) +
      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.10, 0.15))  # Increased right margin for labels
      ) +
      scale_x_discrete(expand = expansion(add = 0.6)) +
      # Flip coordinates so values are horizontal
      coord_flip() +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text,
           x = tools::toTitleCase(group_var), y = "Average Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5),  # No angle needed for horizontal values
        axis.text.y = element_text(angle = 0, hjust = 1.0),  # Groups on y-axis
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  # Render bar chart
  output$bar_chart <- renderPlot({
    bar_plot()
  })

  
  
  # Create reactive plot for trended expectation chart with IMPROVED data processing
  trended_plot <- reactive({
    data <- filtered_data()
    req(data)

    # IMPROVED: Better data validation
    if(!"value" %in% names(data)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No 'value' column found in data", size = 6) + theme_void())
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if(all(is.na(data$value))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid numeric values in 'value' column", size = 6) + theme_void())
    }

    title_text <- if(is.null(input$trended_title) || input$trended_title == "") "Trended Expectation Chart" else input$trended_title
    subtitle_text <- if(is.null(input$trended_subtitle)) "" else input$trended_subtitle
    caption_text <- if(is.null(input$trended_caption)) "" else input$trended_caption

    # Convert dates to numeric for linear modeling (days since first date)
    data$date_numeric <- as.numeric(data$date - min(data$date))

    # IMPROVED: Fit linear model with robust error handling
    trend_model <- NULL
    bias_corrected_sd <- 0

    tryCatch({
      trend_model <- lm(value ~ date_numeric, data = data)

      # Calculate residuals standard deviation with bias correction
      residuals_sd <- safe_sd(residuals(trend_model))
      bias_corrected_sd <- residuals_sd / 1.128

      # Calculate trended centerline and expectation limits
      data$trended_cl <- as.numeric(predict(trend_model, newdata = data))
      data$trended_ucl <- data$trended_cl + 3 * bias_corrected_sd
      data$trended_lcl <- data$trended_cl - 3 * bias_corrected_sd
    }, error = function(e) {
      # Fallback to simple mean if linear model fails
      emp_cl <- safe_mean(data$value)
      data$trended_cl <<- rep(emp_cl, nrow(data))
      data$trended_ucl <<- rep(emp_cl + 3 * safe_sd(data$value), nrow(data))
      data$trended_lcl <<- rep(emp_cl - 3 * safe_sd(data$value), nrow(data))
      bias_corrected_sd <<- safe_sd(data$value)
    })

    # Calculate sigma signals (points outside expectation limits)
    data$sigma_signals <- data$value > data$trended_ucl | data$value < data$trended_lcl

    # Use improved runs analysis for trended chart
    data$runs_signal <- detect_runs_signals(data$value, data$trended_cl, data$date)

    # Create annotation data for limits
    annotation_data_limits <- data.frame(
      x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
      y_pos = max(data$trended_ucl) + (max(data$trended_ucl) - min(data$trended_lcl)) * 0.02,
      label = if(!is.null(trend_model)) {
        paste0("Trend Model: y = ", round(coef(trend_model)[1], 2), " + ",
               round(coef(trend_model)[2], 4), " × days<br>",
               "Bias-corrected σ = ", round(bias_corrected_sd, 2))
      } else {
        paste0("Fallback Model (No Trend)<br>",
               "σ = ", round(bias_corrected_sd, 2))
      }
    )

    # Create annotation data for runs analysis
    annotation_data_runs <- data.frame(
      x_pos = min(data$date) + as.numeric(diff(range(data$date))) * 0.02,
      y_pos = min(data$trended_lcl) - (max(data$trended_ucl) - min(data$trended_lcl)) * 0.05,
      label = ifelse(any(data$runs_signal, na.rm = TRUE), "Runs Signal Detected", "No Runs Signal")
    )

    # Smart date formatting
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)

    if (all_jan_first) {
      date_format <- "%Y"
      date_breaks_interval <- "1 year"
    } else {
      date_format <- "%Y-%m-%d"
      date_range_days <- as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE))
      if (date_range_days > 365*5) {
        date_breaks_interval <- "1 year"
      } else if (date_range_days > 365) {
        date_breaks_interval <- "6 months"
      } else if (date_range_days > 90) {
        date_breaks_interval <- "1 month"
      } else {
        date_breaks_interval <- "1 week"
      }
    }

    # Create trended expectation chart

    # Determine if any runs signal exists for line style
    any_runs_signal <- any(data$runs_signal, na.rm = TRUE)
    centerline_style <- if(any_runs_signal) "dashed" else "solid"

    ggplot(data, aes(x = date, y = value)) +

      # Connect the dots with lines
      geom_line(color = "darkgray", linewidth = 1.2) +

      # Points colored by sigma signals (red = outside limits, blue = within)
      geom_point(aes(color = sigma_signals), size = 2.5) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +

      # Trended centerline with linetype based on runs signals
      geom_line(aes(y = trended_cl), color = "blue", linewidth = 1, linetype = centerline_style) +

      # Upper and lower expectation limits (red dotted lines for trended)
      geom_line(aes(y = trended_ucl), color = "red", linetype = "dotted", linewidth = 1) +
      geom_line(aes(y = trended_lcl), color = "red", linetype = "dotted", linewidth = 1) +

      # Rich text annotation for trend model info
      ggtext::geom_richtext(
        data = annotation_data_limits,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5,
        color = "black",
        hjust = 0,
        vjust = 1,
        fill = "lightblue",
        label.color = "black"
      ) +

      # Rich text annotation for runs analysis
      ggtext::geom_richtext(
        data = annotation_data_runs,
        aes(x = x_pos, y = y_pos, label = label),
        size = 5,
        color = "black",
        hjust = 0,
        vjust = 0,
        fill = "lightyellow",
        label.color = "black"
      ) +

      # Label for trended centerline at start
      geom_text(aes(x = dplyr::first(date), y = dplyr::first(trended_cl), label = "Trended Expectation"),
                color = "blue", vjust = -1, hjust = 1.1, size = 5) +

      # Value for trended centerline at end
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(trended_cl),
                    label = format(round(dplyr::last(trended_cl), 2), nsmall = 2)),
                color = "blue", vjust = 0, hjust = -0.5, size = 5) +

      # Label for upper limit at start
      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(trended_ucl), label = "Upper Trend Limit"),
                color = "red", vjust = -1, hjust = 1.1, size = 5) +

      # Value for upper limit at end
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(trended_ucl),
                    label = format(round(dplyr::last(trended_ucl), 2), nsmall = 2)),
                color = "red", vjust = -1, hjust = -0.5, size = 5) +

      # Label for lower limit at start
      geom_text(aes(x = dplyr::first(date) + 0.5, y = dplyr::first(trended_lcl), label = "Lower Trend Limit"),
                color = "red", vjust = 1.75, hjust = 1.1, size = 5) +

      # Value for lower limit at end
      geom_text(aes(x = dplyr::last(date), y = dplyr::last(trended_lcl),
                    label = format(round(dplyr::last(trended_lcl), 2), nsmall = 2)),
                color = "red", vjust = 1.5, hjust = -0.5, size = 5) +

      # Y-axis formatting with generous space for labels
      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.20, 0.20))
      ) +

      # X-axis formatting
      scale_x_date(
        date_labels = date_format,
        date_breaks = date_breaks_interval,
        expand = expansion(mult = c(0.1, 0.1))
      ) +

      labs(title = title_text, subtitle = subtitle_text, caption = caption_text, x = "Date", y = "Value") +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.00),
        axis.title.x = element_text(margin = margin(t = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  
  
  # Create reactive plot for educational cohort tracking with IMPROVED data processing
  cohort_plot <- reactive({
    data <- filtered_data()
    req(data)

    # IMPROVED: Better data validation
    if(!"value" %in% names(data)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No 'value' column found in data", size = 6) + theme_void())
    }

    # Remove rows with NA values to prevent TRUE/FALSE errors
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data after removing NA values", size = 6) + theme_void())
    }

    # Check if required inputs are available
    if(is.null(input$cohort_grade_var) || input$cohort_grade_var == "" ||
       !input$cohort_grade_var %in% names(data) ||
       is.null(input$cohort_start_grade) || is.null(input$cohort_end_grade) ||
       is.null(input$cohort_start_year) || is.null(input$cohort_end_year)) {
      return(NULL)
    }

    grade_var <- input$cohort_grade_var
    start_grade <- input$cohort_start_grade
    end_grade <- input$cohort_end_grade
    start_year <- input$cohort_start_year
    end_year <- input$cohort_end_year

    # IMPROVED: Safe numeric conversion
    data$value <- safe_numeric(data$value)

    # Check if we have any valid numeric values
    if(all(is.na(data$value))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid numeric values in 'value' column", size = 6) + theme_void())
    }

    title_text <- if(is.null(input$cohort_title) || input$cohort_title == "") "Educational Cohort Analysis" else input$cohort_title
    subtitle_text <- if(is.null(input$cohort_subtitle)) "" else input$cohort_subtitle
    caption_text <- if(is.null(input$cohort_caption)) "" else input$cohort_caption

    # Validate inputs
    if(end_grade < start_grade || end_year < start_year) {
      return(NULL)
    }

    # Create cohort progression data
    cohort_data <- data.frame()

    current_year <- start_year
    current_grade <- start_grade

    while(current_year <= end_year && current_grade <= end_grade) {
      # Check if data has 'year' column, otherwise extract from 'date'
      year_matches <- if("year" %in% names(data)) {
        # Extract year from date objects if needed
        if(inherits(data$year, c("Date", "POSIXct", "POSIXlt"))) {
          year(data$year) == current_year
        } else {
          data$year == current_year
        }
      } else if("date" %in% names(data)) {
        year(data$date) == current_year
      } else {
        rep(TRUE, nrow(data))  # If no year info, include all
      }

      # Use user-selected grade column
      grade_matches <- if(is.numeric(data[[grade_var]])) {
        # If numeric grade column (like grade_level_code: 3,4,5,6,7,8)
        data[[grade_var]] == current_grade
      } else {
        # If text grade column (like grade_level_name: "Grade 3", "Grade 4", etc.)
        # Try to extract number from text or match text patterns
        grade_text_patterns <- paste0("\\b", current_grade, "\\b")  # Word boundary match
        grepl(grade_text_patterns, data[[grade_var]], ignore.case = TRUE)
      }

      # Get data for this year-grade combination
      year_grade_data <- data[year_matches & grade_matches, ]

      if(nrow(year_grade_data) > 0) {
        # Extract the single value directly (no averaging needed)
        cohort_value <- safe_numeric(year_grade_data$value[1])  # Take first/only value

        # Add to cohort progression
        cohort_data <- rbind(cohort_data, data.frame(
          year = current_year,
          grade = current_grade,
          cohort_year = current_year - start_year + 1,
          value = cohort_value
        ))
      }

      current_year <- current_year + 1
      current_grade <- current_grade + 1
    }

    # If no valid data found, return NULL
    if(nrow(cohort_data) == 0) {
      return(NULL)
    }

    # Create the cohort progression line chart
    ggplot(cohort_data, aes(x = grade, y = value)) +

      # Connect dots with line
      geom_line(color = "#2E86AB", linewidth = 1.5) +

      # Points for each grade
      geom_point(color = "#A23B72", size = 3.5) +

      # Add value labels above points (with better precision)
      geom_text(aes(label = format(round(value, 3), nsmall = 3)),
                color = "black", vjust = -0.8, size = 5, fontface = "bold") +

      # Add year labels below points (larger and more distinct)
      geom_text(aes(label = paste0("(", year, ")")),
                color = "darkgreen", vjust = 2.5, size = 7, fontface = "bold") +

      scale_x_continuous(
        breaks = cohort_data$grade,
        labels = paste("Grade", cohort_data$grade),
        expand = expansion(mult = c(0.1, 0.1))
      ) +

      scale_y_continuous(
        labels = if(input$format_as_percentage) {
          scales::percent_format(accuracy = 0.1)
        } else {
          function(y) format(y, scientific = FALSE, big.mark = ",")
        },
        expand = expansion(mult = c(0.15, 0.15))
      ) +

      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = "Grade Level",
        y = "Performance Value"
      ) +

      theme_minimal(base_size = 26) +

      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1.0),
        axis.text.y = element_text(angle = 0, hjust = 1.0),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        text = element_text(color = "royalblue"),
        plot.title.position = "panel",
        plot.title = element_markdown(color = "darkgreen", size = 26, face = "bold", lineheight = 1.1, margin = margin(2, 0, 0, 0, "lines")),
        plot.subtitle = element_markdown(color = "darkgreen", size = 24, face = "bold", lineheight = 1.0, margin = margin(0, 0, 0, 0, "lines")),
        plot.caption = element_text(size = 22, hjust = 0, vjust = 2, face = "italic", color = "darkblue"),
        axis.text = element_text(color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
  })

  # Cohort data reactive function (for compatibility with existing chart render code)
  cohort_data <- reactive({
    # Check if required inputs are available
    if(is.null(input$cohort_grade_var) || input$cohort_grade_var == "" ||
       is.null(input$cohort_start_grade) || is.null(input$cohort_end_grade) ||
       is.null(input$cohort_start_year) || is.null(input$cohort_end_year)) {
      return(data.frame())
    }

    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      return(data.frame())
    }

    # Remove rows with NA values
    data <- data[!is.na(data$date) & !is.na(data$value), ]

    if(nrow(data) == 0) {
      return(data.frame())
    }

    grade_var <- input$cohort_grade_var
    start_grade <- as.numeric(input$cohort_start_grade)
    end_grade <- as.numeric(input$cohort_end_grade)
    start_year <- as.numeric(input$cohort_start_year)
    end_year <- as.numeric(input$cohort_end_year)

    # Calculate years to track
    usable_years <- seq(from = start_year, to = end_year)
    years_to_track <- length(usable_years)
    grades <- seq(from = start_grade, by = 1, length.out = years_to_track)

    # Build cohort progression data
    final_result <- data.frame()

    for (i in 1:length(usable_years)) {
      current_year <- usable_years[i]
      current_grade <- grades[i]

      # Handle year matching - extract year from date objects
      year_matches <- if("year" %in% names(data)) {
        # Extract year from date objects if needed
        if(inherits(data$year, c("Date", "POSIXct", "POSIXlt"))) {
          year(data$year) == current_year
        } else {
          data$year == current_year
        }
      } else if("date" %in% names(data)) {
        year(data$date) == current_year
      } else {
        rep(TRUE, nrow(data))
      }

      # Handle grade matching using selected column
      grade_matches <- if(is.numeric(data[[grade_var]])) {
        as.numeric(data[[grade_var]]) == current_grade
      } else {
        grade_text_patterns <- paste0("\\b", current_grade, "\\b")
        grepl(grade_text_patterns, data[[grade_var]], ignore.case = TRUE)
      }

      # Filter for this year-grade combination
      year_data <- data[year_matches & grade_matches, ]

      if(nrow(year_data) > 0) {
        final_result <- rbind(final_result, year_data)
      }
    }

    if(nrow(final_result) == 0) {
      return(data.frame())
    }

    # Add progression column for x-axis
    final_result$progression <- paste0("Year ", final_result$year, " (Grade ", final_result[[grade_var]], ")")

    # Sort by year to ensure correct order
    final_result <- final_result %>% arrange(if("year" %in% names(final_result)) year else date)

    return(final_result)
  })

  
  # Render expectation chart
  output$control_chart <- renderPlot({
    control_plot()
  })

  # Render trended expectation chart
  output$trended_chart <- renderPlot({
    trended_plot()
  })

  # Render cohort chart
  output$cohort_chart <- renderPlot({
    cohort_plot()
  })

  # Download handlers for run chart
  output$download_run_png <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_run_svg <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_run_pdf <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = run_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = run_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(run_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for line chart
  output$download_line_png <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_line_svg <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_line_pdf <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = line_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = line_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(line_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for expectation chart
  output$download_control_png <- downloadHandler(
    filename = function() paste0("expectation_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = control_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_control_svg <- downloadHandler(
    filename = function() paste0("expectation_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = control_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_control_pdf <- downloadHandler(
    filename = function() paste0("expectation_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = control_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = control_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(control_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for bar chart
  output$download_bar_png <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = bar_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_bar_svg <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = bar_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_bar_pdf <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = bar_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = bar_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(bar_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for trended expectation chart
  output$download_trended_png <- downloadHandler(
    filename = function() paste0("trended_expectation_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = trended_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_trended_svg <- downloadHandler(
    filename = function() paste0("trended_expectation_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = trended_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_trended_pdf <- downloadHandler(
    filename = function() paste0("trended_expectation_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = trended_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = trended_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(trended_plot())
          dev.off()
        })
      })
    }
  )

  # Download handlers for cohort chart
  output$download_cohort_png <- downloadHandler(
    filename = function() paste0("cohort_chart_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = cohort_plot(), device = "png", width = 16, height = 8, dpi = 300, bg = "white")
    }
  )

  output$download_cohort_svg <- downloadHandler(
    filename = function() paste0("cohort_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = cohort_plot(), device = "svg", width = 16, height = 8, bg = "white")
    }
  )

  output$download_cohort_pdf <- downloadHandler(
    filename = function() paste0("cohort_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = cohort_plot(), device = cairo_pdf, width = 16, height = 8, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = cohort_plot(), device = "pdf", width = 16, height = 8, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 16, height = 8)
          print(cohort_plot())
          dev.off()
        })
      })
    }
  )

  # Observe uploaded data and create filtering controls
  observeEvent(raw_data(), {
    data <- raw_data()
    req(data)

    # Clear existing filter controls
    removeUI(selector = "#filter_controls > *", multiple = TRUE)

    # Get column names excluding value columns
    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    # NEW: Also exclude conversion-related columns from filters
    exclude_cols <- c("is_original", "conversion_note", "original_value", "original_units", "unit_category")
    filter_columns <- names(data)[!grepl(value_pattern, names(data), ignore.case = TRUE) & 
                                  !names(data) %in% exclude_cols]

    # Create dynamic filter controls
    filter_controls <- map(filter_columns, function(col) {

      if(inherits(data[[col]], "Date") || lubridate::is.Date(data[[col]])) {
        # Date columns get date sliders
        date_values <- data[[col]][!is.na(data[[col]])]
        min_date <- min(date_values, na.rm = TRUE)
        max_date <- max(date_values, na.rm = TRUE)

        column(4,
          sliderInput(
            inputId = paste0("filter_", make.names(col)),
            label = col,
            min = min_date,
            max = max_date,
            value = c(min_date, max_date),
            timeFormat = "%Y-%m-%d",
            step = 1
          )
        )

      } else if(is.numeric(data[[col]])) {
        # Numeric columns get range sliders
        column(2,
          sliderInput(
            inputId = paste0("filter_", make.names(col)),
            label = col,
            min = min(data[[col]], na.rm = TRUE),
            max = max(data[[col]], na.rm = TRUE),
            value = c(min(data[[col]], na.rm = TRUE), max(data[[col]], na.rm = TRUE)),
            step = (max(data[[col]], na.rm = TRUE) - min(data[[col]], na.rm = TRUE)) / 100,
            sep = ""
          )
        )

      } else {
        # Categorical columns get multi-select
        unique_values <- unique(data[[col]][!is.na(data[[col]])])
        unique_values <- sort(as.character(unique_values))

        choices_list <- c("All" = "ALL_VALUES", "None" = "NO_VALUES", setNames(unique_values, unique_values))

        column(2,
          selectInput(
            inputId = paste0("filter_", make.names(col)),
            label = col,
            choices = choices_list,
            selected = "ALL_VALUES",
            multiple = TRUE,
            selectize = TRUE
          )
        )
      }
    })

    # Insert filter controls
    insertUI(
      selector = "#filter_controls",
      where = "beforeEnd",
      ui = fluidRow(filter_controls)
    )
  })

  # Observer for "All" and "None" selection logic
  observe({
    data <- raw_data()
    req(data)

    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    # NEW: Also exclude conversion-related columns
    exclude_cols <- c("is_original", "conversion_note", "original_value", "original_units", "unit_category")
    filter_columns <- names(data)[!grepl(value_pattern, names(data), ignore.case = TRUE) & 
                                  !names(data) %in% exclude_cols]

    for(col in filter_columns) {
      if(!is.numeric(data[[col]]) && !inherits(data[[col]], "Date") && !lubridate::is.Date(data[[col]])) {
        filter_input_name <- paste0("filter_", make.names(col))

        observeEvent(input[[filter_input_name]], {
          current_selection <- input[[filter_input_name]]

          if(!is.null(current_selection)) {
            if("ALL_VALUES" %in% current_selection && length(current_selection) > 1) {
              updateSelectInput(session, filter_input_name, selected = "ALL_VALUES")
            }
            else if("NO_VALUES" %in% current_selection && length(current_selection) > 1) {
              updateSelectInput(session, filter_input_name, selected = "NO_VALUES")
            }
          }
        }, ignoreInit = TRUE)
      }
    }
  })

  # MODIFIED: Reactive filtered data with conversion toggle support
  filtered_data <- reactive({
    data <- raw_data()
    req(data)
    
    # NEW: Filter by conversion toggle if conversions exist
    if(!is.null(input$show_converted) && "is_original" %in% names(data)) {
      if(input$show_converted) {
        # Show converted values (is_original = FALSE)
        data <- data[!data$is_original, ]
      } else {
        # Show original values (is_original = TRUE)
        data <- data[data$is_original, ]
      }
    }

    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    # NEW: Also exclude conversion-related columns
    exclude_cols <- c("is_original", "conversion_note", "original_value", "original_units", "unit_category")
    filter_columns <- names(data)[!grepl(value_pattern, names(data), ignore.case = TRUE) & 
                                  !names(data) %in% exclude_cols]

    filtered <- data

    for(col in filter_columns) {
      filter_input_name <- paste0("filter_", make.names(col))
      filter_value <- input[[filter_input_name]]

      if(!is.null(filter_value)) {

        # Date filtering
        if(inherits(data[[col]], "Date") || lubridate::is.Date(data[[col]])) {
          start_date <- filter_value[1]
          end_date <- filter_value[2]
          filtered <- filtered %>%
            filter(!!sym(col) >= start_date & !!sym(col) <= end_date)

        } else if(is.numeric(data[[col]])) {
          # Numeric filtering
          filtered <- filtered %>%
            filter(!!sym(col) >= filter_value[1] & !!sym(col) <= filter_value[2])

        } else {
          # Categorical filtering
          if("ALL_VALUES" %in% filter_value) {
            next
          } else if("NO_VALUES" %in% filter_value && length(filter_value) == 1) {
            filtered <- filtered %>% filter(FALSE)
          } else {
            actual_values <- filter_value[!filter_value %in% c("ALL_VALUES", "NO_VALUES")]
            if(length(actual_values) > 0) {
              filtered <- filtered %>%
                filter(as.character(!!sym(col)) %in% actual_values)
            } else {
              filtered <- filtered %>% filter(FALSE)
            }
          }
        }
      }
    }

    return(filtered)
  })

  # Render data table
  output$data_table <- DT::renderDataTable({
    data <- filtered_data()
    req(data)
    
    # MODIFIED: Hide conversion-related columns from display
    display_cols <- names(data)[!names(data) %in% c("is_original", "conversion_note", "original_value", "original_units")]

    DT::datatable(
      data[, display_cols],
      options = list(
        pageLength = as.numeric(input$rows_display),
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)