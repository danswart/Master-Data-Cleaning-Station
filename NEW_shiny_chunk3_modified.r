# Server
server <- function(input, output, session) {

  # Function to detect and convert numeric dates
  detect_and_convert_dates <- function(data) {
    for(col in names(data)) {
      if(is.numeric(data[[col]])) {
        values <- data[[col]][!is.na(data[[col]])]

        if(length(values) > 0) {
          converted <- FALSE

          # Check for years (like 1995, 1996, 2023)
          if(all(values >= 1900 & values <= 2100) & all(values == floor(values))) {
            try({
              data[[col]] <- as.Date(paste0(data[[col]], "-01-01"))
              converted <- TRUE
            }, silent = FALSE)
          }
          # Check for Excel serial dates
          else if(all(values >= 15000 & values <= 60000)) {
            try({
              data[[col]] <- as.Date(data[[col]], origin = "1899-12-30")
            }, silent = FALSE)
          }
          # Check for YYYYMMDD format
          else if(all(values >= 19000101 & values <= 21001231) &
                  all(nchar(as.character(values)) == 8)) {
            try({
              data[[col]] <- as.Date(as.character(data[[col]]), format = "%Y%m%d")
              converted <- TRUE
            }, silent = FALSE)
          }
        }
      }
    }
    return(data)
  }

  # Reactive value to store uploaded data
  raw_data <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$datapath)

    data <- if(ext == "csv") {
      read_csv(input$file$datapath, col_names = input$header, show_col_types = FALSE)
    } else if(ext %in% c("xlsx", "xls")) {
      read_excel(input$file$datapath, col_names = input$header)
    } else if(ext == "rds") {  # NEW: Handle RDS files
      readRDS(input$file$datapath)
    } else {
      return(NULL)
    }

    # Convert numeric date columns
    if(!is.null(data)) {
      data <- detect_and_convert_dates(data)
    }

    return(data)
  })

  # Output to control conditional panel visibility
  output$data_uploaded <- reactive({
    return(!is.null(raw_data()))
  })
  outputOptions(output, 'data_uploaded', suspendWhenHidden = FALSE)
  
  # NEW: Check if data has conversions
  output$has_conversions <- reactive({
    data <- raw_data()
    if(is.null(data)) return(FALSE)
    return("is_original" %in% names(data))
  })
  outputOptions(output, 'has_conversions', suspendWhenHidden = FALSE)

  # Reactive for available grouping columns
  grouping_choices <- reactive({
    data <- raw_data()
    if(is.null(data)) return(NULL)

    # Exclude date and value columns
    value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
    value_pattern <- paste(value_patterns, collapse = "|")
    # NEW: Also exclude conversion-related columns
    exclude_cols <- c("date", "is_original", "conversion_note", "original_value", "original_units", "unit_category")
    potential_groups <- names(data)[!names(data) %in% exclude_cols &
                                   !grepl(value_pattern, names(data), ignore.case = TRUE)]

    return(potential_groups)
  })

  # Reactive for cohort grade column choices
  cohort_grade_choices <- reactive({
    data <- raw_data()
    if(is.null(data)) return(NULL)

    # Look for grade-like columns
    grade_patterns <- c("grade", "level", "year")
    potential_grades <- names(data)

    # Prioritize columns with grade-like names
    grade_like <- potential_grades[grepl(paste(grade_patterns, collapse = "|"), potential_grades, ignore.case = TRUE)]

    # If no grade-like columns found, show all non-date/value columns
    if(length(grade_like) == 0) {
      value_patterns <- c("^value$", "^pct$", "^percent$", "^amount$", "^count$", "^measure$")
      value_pattern <- paste(value_patterns, collapse = "|")
      # NEW: Also exclude conversion-related columns
      exclude_cols <- c("date", "is_original", "conversion_note", "original_value", "original_units", "unit_category")
      grade_like <- names(data)[!names(data) %in% exclude_cols &
                               !grepl(value_pattern, names(data), ignore.case = TRUE)]
    }

    return(grade_like)
  })

  # ENHANCED: Reactive for auto-correlation analysis with BOTH ACF and Sample Correlation
  autocorr_data <- reactive({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0 || !"value" %in% names(data)) {
      return(data.frame(
        Lag = c("Lag-1", "Lag-2", "Lag-3"),
        Autocorrelation_ACF = c(NA, NA, NA),
        Sample_Correlation_r = c(NA, NA, NA),
        ACF_Interpretation = c("No data", "No data", "No data"),
        r_Interpretation = c("No data", "No data", "No data"),
        stringsAsFactors = FALSE
      ))
    }

    # Remove rows with NA values and sort by date
    data <- data[!is.na(data$date) & !is.na(data$value), ]
    data <- data[order(data$date), ]

    if(nrow(data) < 4) {
      return(data.frame(
        Lag = c("Lag-1", "Lag-2", "Lag-3"),
        Autocorrelation_ACF = c(NA, NA, NA),
        Sample_Correlation_r = c(NA, NA, NA),
        ACF_Interpretation = c("Insufficient data", "Insufficient data", "Insufficient data"),
        r_Interpretation = c("Insufficient data", "Insufficient data", "Insufficient data"),
        stringsAsFactors = FALSE
      ))
    }

    # Calculate BOTH coefficients for each lag
    lag1_results <- safe_autocorr(data$value, lag = 1)
    lag2_results <- safe_autocorr(data$value, lag = 2)
    lag3_results <- safe_autocorr(data$value, lag = 3)

    # Create interpretation based on absolute value of correlation
    interpret_corr <- function(coeff) {
      if(is.na(coeff)) return("Cannot calculate")
      abs_coeff <- abs(coeff)
      if(abs_coeff < 0.1) return("Very weak correlation")
      if(abs_coeff < 0.3) return("Weak correlation")
      if(abs_coeff < 0.5) return("Moderate correlation")
      if(abs_coeff < 0.7) return("Strong correlation")
      return("Very strong correlation")
    }

    autocorr_result <- data.frame(
      Lag = c("Lag-1", "Lag-2", "Lag-3"),
      Autocorrelation_ACF = round(c(lag1_results$acf, lag2_results$acf, lag3_results$acf), 4),
      Sample_Correlation_r = round(c(lag1_results$r, lag2_results$r, lag3_results$r), 4),
      ACF_Interpretation = c(interpret_corr(lag1_results$acf), interpret_corr(lag2_results$acf), interpret_corr(lag3_results$acf)),
      r_Interpretation = c(interpret_corr(lag1_results$r), interpret_corr(lag2_results$r), interpret_corr(lag3_results$r)),
      stringsAsFactors = FALSE
    )

    return(autocorr_result)
  })

  # NEW: Reactive values to store correlation coefficients for use in control limit calculations
  correlation_values <- reactive({
    autocorr_data_result <- autocorr_data()

    # Extract the Sample Correlation Coefficients for future use
    list(
      r_lag1 = if(nrow(autocorr_data_result) >= 1) autocorr_data_result$Sample_Correlation_r[1] else NA,
      r_lag2 = if(nrow(autocorr_data_result) >= 2) autocorr_data_result$Sample_Correlation_r[2] else NA,
      r_lag3 = if(nrow(autocorr_data_result) >= 3) autocorr_data_result$Sample_Correlation_r[3] else NA,
      acf_lag1 = if(nrow(autocorr_data_result) >= 1) autocorr_data_result$Autocorrelation_ACF[1] else NA,
      acf_lag2 = if(nrow(autocorr_data_result) >= 2) autocorr_data_result$Autocorrelation_ACF[2] else NA,
      acf_lag3 = if(nrow(autocorr_data_result) >= 3) autocorr_data_result$Autocorrelation_ACF[3] else NA
    )
  })

  # Update recalculation date input range when data changes
  observeEvent(raw_data(), {
    data <- raw_data()
    if(!is.null(data) && "date" %in% names(data)) {
      date_range <- range(data$date, na.rm = TRUE)
      # Set default to middle of date range, with available range
      default_date <- date_range[1] + as.numeric(diff(date_range)) * 0.6
      updateDateInput(session, "recalc_date",
                     value = default_date,
                     min = date_range[1],
                     max = date_range[2])
    }
  })

  # Update grouping variable choices when data changes
  observeEvent(grouping_choices(), {
    choices <- grouping_choices()
    if(!is.null(choices) && length(choices) > 0) {
      updateSelectInput(session, "grouping_var",
                       choices = setNames(choices, choices),
                       selected = choices[1])
    } else {
      updateSelectInput(session, "grouping_var",
                       choices = c("No grouping columns available" = ""),
                       selected = "")
    }
  })

  # Update cohort grade variable choices when data changes
  observeEvent(cohort_grade_choices(), {
    choices <- cohort_grade_choices()
    if(!is.null(choices) && length(choices) > 0) {
      # Try to find grade_level_code first, then grade_level_name, then first available
      preferred_selection <- if("grade_level_code" %in% choices) {
        "grade_level_code"
      } else if("grade_level_name" %in% choices) {
        "grade_level_name"
      } else {
        choices[1]
      }

      updateSelectInput(session, "cohort_grade_var",
                       choices = setNames(choices, choices),
                       selected = preferred_selection)
    } else {
      updateSelectInput(session, "cohort_grade_var",
                       choices = c("No grade columns available" = ""),
                       selected = "")
    }
  })
  
  # NEW: Unit warning output
  output$unit_warning <- renderUI({
    data <- filtered_data()
    if(is.null(data) || !"unit_category" %in% names(data)) return(NULL)
    
    # Get unique units in current filtered data
    units <- unique(data$unit_category)
    units <- units[!is.na(units)]
    
    if(length(units) > 1) {
      div(
        style = "background-color: #ffe6e6; padding: 10px; border-radius: 5px; border: 1px solid #ff9999;",
        HTML(paste0(
          "<strong>⚠️ Warning:</strong> Your filtered data contains mixed units: ",
          paste(units, collapse = ", "),
          ". Charts may not be meaningful when combining different unit types. Consider filtering to a single unit type."
        ))
      )
    } else {
      NULL
    }
  })

  # Display date range information (Run Chart tab)
  output$date_info <- renderUI({
    data <- filtered_data()  # MODIFIED: Use filtered_data instead of raw_data
    if (is.null(data) || !"date" %in% names(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)
    all_jan_first <- all(format(data$date, "%m-%d") == "01-01", na.rm = TRUE)
    format_used <- if (all_jan_first) "Years only" else "Full dates (YYYY-MM-DD)"
    
    # NEW: Add unit information if available
    unit_info <- if("unit_category" %in% names(data)) {
      units <- unique(data$unit_category)
      units <- units[!is.na(units)]
      paste0(" | Units: ", paste(units, collapse = ", "))
    } else {
      ""
    }

    div(
      style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; border: 1px solid #4682b4;",
      HTML(paste0(
        "<strong>Date Range:</strong> ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(", nrow(data), " rows, ", format_used, " on x-axis", unit_info, ")</em>"
      ))
    )
  })

  # Display grouping variable information (Line Chart tab)
  output$grouping_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    if(is.null(input$grouping_var) || input$grouping_var == "" || !input$grouping_var %in% names(data)) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML("<strong>Note:</strong> No valid grouping variable selected. Please select a column from the 'Line/Bar Chart Grouping' dropdown above to create multiple lines.")
      )
    } else {
      group_var <- input$grouping_var
      unique_groups <- unique(data[[group_var]])

      div(
        style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; border: 1px solid #81c784;",
        HTML(paste0(
          "<strong>Grouping Variable:</strong> '", group_var, "' with ", length(unique_groups), " categories: ",
          paste(head(unique_groups, 5), collapse = ", "),
          if(length(unique_groups) > 5) "..." else "",
          " <em>(Labels will appear at end of each line)</em>"
        ))
      )
    }
  })

  # Display bar chart information (Bar Chart tab)
  output$bar_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    if(is.null(input$grouping_var) || input$grouping_var == "" || !input$grouping_var %in% names(data)) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML("<strong>Note:</strong> No valid grouping variable selected. Please select a column from the 'Line/Bar Chart Grouping' dropdown above to create bars.")
      )
    } else {
      group_var <- input$grouping_var
      unique_groups <- unique(data[[group_var]])
      date_range <- range(data$date, na.rm = TRUE)

      div(
        style = "background-color: #e7f3ff; padding: 10px; border-radius: 5px; border: 1px solid #7fb3d3;",
        HTML(paste0(
          "<strong>Bar Chart Data:</strong> Showing averages across all filtered dates (",
          format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
          ") with ", length(unique_groups), " categories: ",
          paste(head(unique_groups, 5), collapse = ", "),
          if(length(unique_groups) > 5) "..." else ""
        ))
      )
    }
  })

  # Display trended expectation chart information (Trended Expectation Chart tab)
  output$trended_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)

    div(
      style = "background-color: #e8f5e8; padding: 10px; border-radius: 5px; border: 1px solid #4caf50;",
      HTML(paste0(
        "<strong>Trended Expectation Chart:</strong> Linear trend-adjusted expectation chart showing process capability from ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(Points colored by sigma signals, centerline shows trend, includes runs analysis)</em>"
      ))
    )
  })

  # Display expectation chart information (Expectation Chart tab)
  output$control_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    date_range <- range(data$date, na.rm = TRUE)

    div(
      style = "background-color: #fff8e1; padding: 10px; border-radius: 5px; border: 1px solid #ffa726;",
      HTML(paste0(
        "<strong>Expectation Chart:</strong> Untrended expectation chart showing process capability from ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(Points colored by sigma signals, center line shows runs signals)</em>"
      ))
    )
  })

  output$cohort_info <- renderUI({
    data <- raw_data()
    if (is.null(data)) return(NULL)

    if(is.null(input$cohort_grade_var) || input$cohort_grade_var == "" ||
       !input$cohort_grade_var %in% names(data) ||
       is.null(input$cohort_start_grade) || is.null(input$cohort_end_grade) ||
       is.null(input$cohort_start_year) || is.null(input$cohort_end_year)) {
      div(
        style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffeaa7;",
        HTML("<strong>Note:</strong> Please select a grade column and specify grade range and year range for cohort tracking.")
      )
    } else {
      grade_var <- input$cohort_grade_var
      start_grade <- input$cohort_start_grade
      end_grade <- input$cohort_end_grade
      start_year <- input$cohort_start_year
      end_year <- input$cohort_end_year

      years_span <- end_year - start_year + 1
      grades_span <- end_grade - start_grade + 1

      div(
        style = "background-color: #f3e5f5; padding: 10px; border-radius: 5px; border: 1px solid #ba68c8;",
        HTML(paste0(
          "<strong>Educational Cohort Tracking:</strong> Using '", grade_var, "' column, following cohort progression from Grade ", start_grade, " to Grade ", end_grade,
          " over ", years_span, " years (", start_year, "-", end_year, "). ",
          "<em>Use filters above to select your demographic cohort (ethnicity, subject, etc.)</em>"
        ))
      )
    }
  })

  # ENHANCED: Display auto-correlation information with BOTH correlation types
  output$autocorr_info <- renderUI({
    data <- filtered_data()
    if (is.null(data) || !"value" %in% names(data)) return(NULL)

    # Remove rows with NA values and get valid count
    data_clean <- data[!is.na(data$date) & !is.na(data$value), ]
    data_points <- nrow(data_clean)
    date_range <- range(data_clean$date, na.rm = TRUE)

    # Get correlation values for display
    corr_vals <- correlation_values()

    div(
      style = "background-color: #e8f4fd; padding: 10px; border-radius: 5px; border: 1px solid #3498db;",
      HTML(paste0(
        "<strong>Auto-correlation Analysis:</strong> Analyzing temporal correlation patterns in your data from ",
        format(date_range[1], "%B %d, %Y"), " to ", format(date_range[2], "%B %d, %Y"),
        " <em>(", data_points, " data points after applying filters)</em><br>",
        "<strong>Stored ACF Values:</strong> ACF₁ = ", if(!is.na(corr_vals$acf_lag1)) round(corr_vals$acf_lag1, 4) else "NA",
        ", ACF₂ = ", if(!is.na(corr_vals$acf_lag2)) round(corr_vals$acf_lag2, 4) else "NA",
        ", ACF₃ = ", if(!is.na(corr_vals$acf_lag3)) round(corr_vals$acf_lag3, 4) else "NA",
        "<br>",
        "<strong>Stored r Values:</strong> r₁ = ", if(!is.na(corr_vals$r_lag1)) round(corr_vals$r_lag1, 4) else "NA",
        ", r₂ = ", if(!is.na(corr_vals$r_lag2)) round(corr_vals$r_lag2, 4) else "NA",
        ", r₃ = ", if(!is.na(corr_vals$r_lag3)) round(corr_vals$r_lag3, 4) else "NA",
        " <em>(Available for control limit calculations)</em>"
      ))
    )
  })

  # NEW: Runs debug information
  output$runs_debug_info <- renderText({
    tryCatch({
      data <- filtered_data()
      if(is.null(data) || nrow(data) == 0) {
        return("No data available for runs analysis.")
      }

      # Remove NA values
      data <- data[!is.na(data$date) & !is.na(data$value), ]
      if(nrow(data) == 0) {
        return("No valid data after removing NA values.")
      }

      # Determine if recalculation is enabled
      recalc_enabled <- !is.null(input$enable_recalc) && input$enable_recalc &&
                       !is.null(input$recalc_date) &&
                       input$recalc_date >= min(data$date, na.rm = TRUE) &&
                       input$recalc_date <= max(data$date, na.rm = TRUE)

      if(recalc_enabled) {
        # Recalculation mode analysis
        recalc_date <- input$recalc_date
        data_before <- data[data$date < recalc_date, ]
        data_after <- data[data$date >= recalc_date, ]

        emp_cl_orig <- safe_mean(data$value)
        emp_cl_recalc <- if(nrow(data_after) >= 3) safe_mean(data_after$value) else emp_cl_orig

        # Analyze runs for each segment
        before_above <- if(nrow(data_before) > 0) safe_numeric(data_before$value) > emp_cl_orig else c()
        after_above <- if(nrow(data_after) > 0) safe_numeric(data_after$value) > emp_cl_recalc else c()

        # Find consecutive runs in each segment
        before_runs <- if(length(before_above) > 0) rle(before_above[!is.na(before_above)])$lengths else c()
        after_runs <- if(length(after_above) > 0) rle(after_above[!is.na(after_above)])$lengths else c()

        before_long_runs <- before_runs[before_runs >= 8]
        after_long_runs <- after_runs[after_runs >= 8]

        paste(
          "=== FIXED RUNS ANALYSIS (RECALCULATION MODE) ===",
          paste("Total data points:", nrow(data)),
          paste("Recalculation date:", format(recalc_date, "%Y-%m-%d")),
          paste("Points before recalc:", nrow(data_before)),
          paste("Points after recalc:", nrow(data_after)),
          "",
          "=== BEFORE RECALC SEGMENT ===",
          paste("Centerline (original):", round(emp_cl_orig, 3)),
          paste("All run lengths:", paste(before_runs, collapse = ", ")),
          paste("Runs of 8+ points:", paste(before_long_runs, collapse = ", ")),
          paste("Runs signals detected:", length(before_long_runs) > 0),
          "",
          "=== AFTER RECALC SEGMENT ===",
          paste("Centerline (recalculated):", round(emp_cl_recalc, 3)),
          paste("All run lengths:", paste(after_runs, collapse = ", ")),
          paste("Runs of 8+ points:", paste(after_long_runs, collapse = ", ")),
          paste("Runs signals detected:", length(after_long_runs) > 0),
          "",
          "=== IMPROVEMENT NOTES ===",
          "• Each segment analyzed separately with appropriate centerline",
          "• No artificial breaks at recalculation boundary",
          "• Proper run length encoding (rle) used for detection",
          "• Only 8th+ points in each run marked as signals",
          sep = "\n"
        )
      } else {
        # Standard mode analysis
        emp_cl <- safe_mean(data$value)
        above_cl <- safe_numeric(data$value) > emp_cl
        above_cl_clean <- above_cl[!is.na(above_cl)]

        runs <- rle(above_cl_clean)$lengths
        long_runs <- runs[runs >= 8]

        # Find actual runs details
        run_details <- rle(above_cl_clean)
        above_runs <- run_details$lengths[run_details$values == TRUE]
        below_runs <- run_details$lengths[run_details$values == FALSE]

        paste(
          "=== FIXED RUNS ANALYSIS (STANDARD MODE) ===",
          paste("Total data points:", nrow(data)),
          paste("Centerline:", round(emp_cl, 3)),
          "",
          "=== ALL CONSECUTIVE RUNS ===",
          paste("All run lengths:", paste(runs, collapse = ", ")),
          paste("Runs above centerline:", paste(above_runs, collapse = ", ")),
          paste("Runs below centerline:", paste(below_runs, collapse = ", ")),
          "",
          "=== RUNS SIGNALS (8+ CONSECUTIVE) ===",
          paste("Runs of 8+ points:", paste(long_runs, collapse = ", ")),
          paste("Total long runs detected:", length(long_runs)),
          paste("Runs signal triggered:", length(long_runs) > 0),
          "",
          "=== IMPROVEMENT NOTES ===",
          "• Proper run length encoding (rle) detects ALL consecutive runs",
          "• No boundary condition bugs with rolling windows",
          "• Handles runs of any length (not just exactly 8)",
          "• Only marks 8th+ points in each run as signals",
          sep = "\n"
        )
      }
    }, error = function(e) {
      paste("ERROR in runs analysis:", e$message)
    })
  })

  # NEW: Runs debug data table
  output$runs_debug_table <- DT::renderDataTable({
    tryCatch({
      data <- filtered_data()
      if(is.null(data) || nrow(data) == 0) {
        return(data.frame(Message = "No data available"))
      }

      # Remove NA values
      data <- data[!is.na(data$date) & !is.na(data$value), ]
      if(nrow(data) == 0) {
        return(data.frame(Message = "No valid data after removing NA values"))
      }

      # Calculate runs analysis details
      recalc_enabled <- !is.null(input$enable_recalc) && input$enable_recalc &&
                       !is.null(input$recalc_date) &&
                       input$recalc_date >= min(data$date, na.rm = TRUE) &&
                       input$recalc_date <= max(data$date, na.rm = TRUE)

      if(recalc_enabled) {
        recalc_date <- input$recalc_date
        emp_cl_orig <- safe_mean(data$value)
        data_after <- data[data$date >= recalc_date, ]
        emp_cl_recalc <- if(nrow(data_after) >= 3) safe_mean(data_after$value) else emp_cl_orig

        # Use improved runs detection
        runs_signals <- detect_runs_signals_recalc(safe_numeric(data$value), emp_cl_orig, emp_cl_recalc, data$date, recalc_date)

        centerline_used <- ifelse(data$date < recalc_date, emp_cl_orig, emp_cl_recalc)

        debug_data <- data.frame(
          Row = 1:nrow(data),
          Date = data$date,
          Value = round(safe_numeric(data$value), 3),
          Centerline_Used = round(centerline_used, 3),
          Above_Centerline = safe_numeric(data$value) > centerline_used,
          Runs_Signal = runs_signals,
          Segment = ifelse(data$date < recalc_date, "Before", "After")
        )
      } else {
        emp_cl <- safe_mean(data$value)

        # Use improved runs detection
        runs_signals <- detect_runs_signals(safe_numeric(data$value), rep(emp_cl, nrow(data)), data$date)

        debug_data <- data.frame(
          Row = 1:nrow(data),
          Date = data$date,
          Value = round(safe_numeric(data$value), 3),
          Centerline = round(emp_cl, 3),
          Above_Centerline = safe_numeric(data$value) > emp_cl,
          Runs_Signal = runs_signals
        )
      }

      return(debug_data)

    }, error = function(e) {
      return(data.frame(Error = paste("Error in runs debug table:", e$message)))
    })
  }, options = list(pageLength = 15, scrollX = TRUE, scrollY = "400px"))

  # ENHANCED: Render auto-correlation table with BOTH ACF and Sample Correlation
  output$autocorr_table <- DT::renderDataTable({
    autocorr_data()
  }, options = list(
    pageLength = 10,
    scrollX = TRUE,
    searching = FALSE,
    paging = FALSE,
    info = FALSE,
    columnDefs = list(
      list(targets = c(1, 2), className = "dt-right"),  # Right-align numeric columns
      list(targets = c(3, 4), className = "dt-left")    # Left-align interpretation columns
    )
  ), rownames = FALSE)