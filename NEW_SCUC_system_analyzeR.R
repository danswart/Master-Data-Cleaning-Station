# System Analysis Dashboard - ENHANCED VERSION WITH CONVERSIONS
# Supports converted values display and unit-aware filtering
# Enhanced with BOTH Autocorrelation Coefficients AND Sample Correlation Coefficients
# NEW: Added auto-correlation adjustment for control limits
# UPDATED: Horizontal annotation boxes positioned above UCL
# NEW: Support for RDS files and converted values toggle

library(shiny)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(ggtext)
library(purrr)
library(lubridate)
library(scales)
library(tidyr)  # NEW: For data manipulation with conversions

# IMPROVED: Safe numeric conversion function
safe_numeric <- function(x) {
  if(is.numeric(x)) return(x)

  # Convert to numeric and handle warnings gracefully
  result <- suppressWarnings(as.numeric(as.character(x)))

  # If all values are NA after conversion, return original
  if(all(is.na(result)) && !all(is.na(x))) {
    warning(paste("Could not convert to numeric:", class(x)[1]))
    return(as.numeric(rep(NA, length(x))))
  }

  return(result)
}

# IMPROVED: Safe statistical calculations
safe_mean <- function(x, na.rm = TRUE) {
  x_clean <- safe_numeric(x)
  if(length(x_clean[!is.na(x_clean)]) == 0) return(NA)
  return(mean(x_clean, na.rm = na.rm))
}

safe_median <- function(x, na.rm = TRUE) {
  x_clean <- safe_numeric(x)
  if(length(x_clean[!is.na(x_clean)]) == 0) return(NA)
  return(median(x_clean, na.rm = na.rm))
}

safe_sd <- function(x, na.rm = TRUE) {
  x_clean <- safe_numeric(x)
  if(length(x_clean[!is.na(x_clean)]) <= 1) return(0)
  return(sd(x_clean, na.rm = na.rm))
}

# ENHANCED: Safe auto-correlation calculation function (returns BOTH autocorrelation and sample correlation)
safe_autocorr <- function(x, lag = 1, na.rm = TRUE) {
  x_clean <- safe_numeric(x)

  if(length(x_clean[!is.na(x_clean)]) <= lag + 1) return(list(acf = NA, r = NA))

  # Remove NA values for autocorrelation calculation
  if(na.rm) {
    x_clean <- x_clean[!is.na(x_clean)]
  }

  if(length(x_clean) <= lag + 1) return(list(acf = NA, r = NA))

  n <- length(x_clean)
  if(n <= lag) return(list(acf = NA, r = NA))

  # Calculate mean
  x_mean <- mean(x_clean)

  # Calculate autocorrelation coefficient (normalized by variance)
  numerator <- 0
  denominator <- sum((x_clean - x_mean)^2)

  for(i in 1:(n-lag)) {
    numerator <- numerator + (x_clean[i] - x_mean) * (x_clean[i+lag] - x_mean)
  }

  # Autocorrelation coefficient
  acf_value <- numerator / denominator

  # Sample correlation coefficient (Pearson correlation)
  x_lag <- x_clean[1:(n-lag)]
  x_lead <- x_clean[(lag+1):n]
  r_value <- cor(x_lag, x_lead, use = "complete.obs")

  return(list(acf = acf_value, r = r_value))
}

# NEW: Calculate moving ranges for auto-correlation adjustment
calculate_moving_ranges <- function(values) {
  values <- safe_numeric(values)
  values <- values[!is.na(values)]

  if(length(values) < 2) return(NA)

  # Calculate moving ranges (absolute difference between consecutive points)
  moving_ranges <- abs(diff(values))

  # Return average moving range
  avg_mr <- mean(moving_ranges, na.rm = TRUE)

  return(avg_mr)
}

# IMPROVED: Better runs analysis function with robust error handling
detect_runs_signals <- function(values, centerline, dates = NULL, min_run_length = 8) {
  # Robust numeric conversion
  values <- safe_numeric(values)
  centerline <- safe_numeric(centerline)

  valid_indices <- which(!is.na(values) & !is.na(centerline) & is.finite(values) & is.finite(centerline))
  if(length(valid_indices) < min_run_length) {
    return(rep(FALSE, length(values)))
  }

  # Calculate which side of centerline each point is on
  above_cl <- rep(NA, length(values))
  above_cl[valid_indices] <- values[valid_indices] > centerline[valid_indices]

  # Use run length encoding to find consecutive runs
  valid_above <- above_cl[valid_indices]
  runs <- rle(valid_above)

  # Find runs of min_run_length+ consecutive points
  long_runs <- runs$lengths >= min_run_length

  # Initialize signal vector
  signals <- rep(FALSE, length(values))

  if(any(long_runs)) {
    # Calculate positions in the valid subset
    valid_end_positions <- cumsum(runs$lengths)
    valid_start_positions <- c(1, valid_end_positions[-length(valid_end_positions)] + 1)

    for(i in which(long_runs)) {
      valid_run_start <- valid_start_positions[i]
      valid_run_end <- valid_end_positions[i]

      # Convert back to original indices
      run_start_idx <- valid_indices[valid_run_start]
      run_end_idx <- valid_indices[valid_run_end]

      # Mark from the 8th point onward in this run
      signal_start_in_valid <- valid_run_start + min_run_length - 1
      if(signal_start_in_valid <= length(valid_indices)) {
        signal_start_idx <- valid_indices[signal_start_in_valid]
        signals[signal_start_idx:run_end_idx] <- TRUE
      }
    }
  }

  return(signals)
}

# IMPROVED: Runs analysis for recalculated charts with robust error handling
detect_runs_signals_recalc <- function(values, centerline_orig, centerline_recalc, dates, recalc_date, min_run_length = 8) {
  # Robust numeric conversion
  values <- safe_numeric(values)
  centerline_orig <- safe_numeric(centerline_orig)
  centerline_recalc <- safe_numeric(centerline_recalc)

  signals <- rep(FALSE, length(values))

  # Split data at recalculation point
  before_recalc <- which(!is.na(dates) & dates < recalc_date)
  after_recalc <- which(!is.na(dates) & dates >= recalc_date)

  # Analyze runs separately for each segment
  if(length(before_recalc) >= min_run_length) {
    before_centerline <- rep(centerline_orig[1], length(before_recalc))
    before_signals <- detect_runs_signals(values[before_recalc], before_centerline, dates[before_recalc], min_run_length)
    signals[before_recalc] <- before_signals
  }

  if(length(after_recalc) >= min_run_length) {
    after_centerline <- rep(centerline_recalc[1], length(after_recalc))
    after_signals <- detect_runs_signals(values[after_recalc], after_centerline, dates[after_recalc], min_run_length)
    signals[after_recalc] <- after_signals
  }

  return(signals)
}

# NEW: Function to check unit compatibility
check_unit_compatibility <- function(data, selected_cols) {
  if(!"unit_category" %in% names(data)) return(list(compatible = TRUE, units = NULL))

  units <- unique(data$unit_category[data$col_name %in% selected_cols])
  units <- units[!is.na(units)]

  return(list(
    compatible = length(units) <= 1,
    units = units
  ))
}

# Custom 20-color palette for charts - ensures we never run out of colors
chart_colors <- c(
  "#2E86AB",  # Ocean blue
  "#A23B72",  # Berry purple
  "#F18F01",  # Orange
  "#C73E1D",  # Red
  "#8E5572",  # Mauve
  "#007F5F",  # Forest green
  "#7209B7",  # Purple
  "#AA6C39",  # Brown
  "#FF6B6B",  # Coral
  "#4ECDC4",  # Teal
  "#45B7D1",  # Sky blue
  "#96CEB4",  # Mint green
  "#FFEAA7",  # Light yellow
  "#DDA0DD",  # Plum
  "#F39C12",  # Golden orange
  "#E74C3C",  # Crimson
  "#3498DB",  # Bright blue
  "#2ECC71",  # Emerald
  "#9B59B6",  # Amethyst
  "#F1C40F"   # Sunshine yellow
)

#####  END OF CODE CHUNK #1  #####




##### BEGIN CODE CHUNK #2  #####


# UI
ui <- fluidPage(
  # Remove default bootstrap container constraints
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "swart-20250709.css")
  ),

  titlePanel("Upload Standardized Datasets Only"),

  # Row 1: File Upload and Basic Controls
  fluidRow(
    column(3,
           fileInput("file", "Upload CSV, Excel, or RDS File",
                     accept = c(".csv", ".xlsx", ".xls", ".rds"))  # MODIFIED: Added .rds
    ),
    column(2,
           checkboxInput("header", "Header in first row", value = TRUE)
    ),
    column(2,
           selectInput("rows_display", "Rows to display:",
                       choices = c(30, 40, 50),
                       selected = 30)
    ),
    column(2,
           checkboxInput("format_as_percentage", "Y-axis: Show as Percentage (85.2%)", value = FALSE),
           tags$small("Check if values are rates/percentages, uncheck for scores/counts", style = "color: #6c757d;")
    ),
    column(3,
           conditionalPanel(
             condition = "output.data_uploaded",
             selectInput("grouping_var", "Line/Bar Chart Grouping:",
                         choices = NULL,
                         selected = NULL),
             tags$small("Column to group/label lines and bars", style = "color: #6c757d;")
           )
    )
  ),

  # NEW Row: Conversion toggle and unit warnings
  fluidRow(
    column(4,
           conditionalPanel(
             condition = "output.has_conversions",  # NEW: Only show if conversions exist
             checkboxInput("show_converted", "Show converted values instead of percentages/rates", value = FALSE),
             tags$small("When enabled, displays count/dollar equivalents for percentage/rate data", style = "color: #6c757d;")
           )
    ),
    column(8,
           # NEW: Unit compatibility warning area
           uiOutput("unit_warning")
    )
  ),

  # Row 2: Filtering Controls
  fluidRow(
    column(12,
           div(id = "filter_controls",
               style = "background-color: #f8f9fa; padding: 10px; margin: 10px 0px; border-radius: 5px;")
    )
  ),

  # Row 3: Tab Panel for Data Table and Charts
  fluidRow(
    column(12,
           tabsetPanel(id = "main_tabs",
                       tabPanel("Data Table",
                                br(),
                                DT::dataTableOutput("data_table", width = "100%")
                       ),
                       tabPanel("Run Chart",
                                br(),
                                fluidRow(
                                  column(12,
                                         uiOutput("date_info")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(4,
                                         textInput("run_title", "Chart Title:", value = "Run Chart")
                                  ),
                                  column(4,
                                         textInput("run_subtitle", "Subtitle:", value = "")
                                  ),
                                  column(4,
                                         textInput("run_caption", "Caption:", value = "")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         div(style = "text-align: center;",
                                             downloadButton("download_run_png", "Download PNG", class = "btn-primary"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_run_svg", "Download SVG", class = "btn-success"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_run_pdf", "Download PDF", class = "btn-info")
                                         )
                                  )
                                ),
                                br(),
                                plotOutput("run_chart", height = "600px")
                       ),
                       tabPanel("Line Chart",
                                br(),
                                fluidRow(
                                  column(12,
                                         uiOutput("grouping_info")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(4,
                                         textInput("line_title", "Chart Title:", value = "Line Chart")
                                  ),
                                  column(4,
                                         textInput("line_subtitle", "Subtitle:", value = "")
                                  ),
                                  column(4,
                                         textInput("line_caption", "Caption:", value = "")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         div(style = "text-align: center;",
                                             downloadButton("download_line_png", "Download PNG", class = "btn-primary"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_line_svg", "Download SVG", class = "btn-success"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_line_pdf", "Download PDF", class = "btn-info")
                                         )
                                  )
                                ),
                                br(),
                                plotOutput("line_chart", height = "600px")
                       ),
                       tabPanel("Bar Chart",
                                br(),
                                fluidRow(
                                  column(12,
                                         uiOutput("bar_info")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(4,
                                         textInput("bar_title", "Chart Title:", value = "Bar Chart")
                                  ),
                                  column(4,
                                         textInput("bar_subtitle", "Subtitle:", value = "")
                                  ),
                                  column(4,
                                         textInput("bar_caption", "Caption:", value = "")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         div(style = "text-align: center;",
                                             downloadButton("download_bar_png", "Download PNG", class = "btn-primary"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_bar_svg", "Download SVG", class = "btn-success"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_bar_pdf", "Download PDF", class = "btn-info")
                                         )
                                  )
                                ),
                                br(),
                                plotOutput("bar_chart", height = "600px")
                       ),
                       tabPanel("Untrended Expectation Chart",
                                br(),
                                fluidRow(
                                  column(12,
                                         uiOutput("control_info")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         conditionalPanel(
                                           condition = "output.data_uploaded",
                                           div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
                                               fluidRow(
                                                 column(3,
                                                        checkboxInput("enable_recalc", "Enable Recalculation", value = FALSE)
                                                 ),
                                                 column(4,
                                                        conditionalPanel(
                                                          condition = "input.enable_recalc",
                                                          dateInput("recalc_date", "Recalculate limits starting from:",
                                                                    value = NULL, min = NULL, max = NULL)
                                                        )
                                                 ),
                                                 column(5,
                                                        conditionalPanel(
                                                          condition = "input.enable_recalc",
                                                          tags$small("Select date to split process and recalculate expectation limits",
                                                                     style = "color: #6c757d; margin-top: 5px; display: block;")
                                                        )
                                                 )
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(3,
                                                        checkboxInput("use_autocorr_modifier", "Use Auto-correlation Modifier", value = FALSE)
                                                 ),
                                                 column(9,
                                                        conditionalPanel(
                                                          condition = "input.use_autocorr_modifier",
                                                          tags$small("Adjusts control limits using: σ = R-bar / (d2 × √(1 - r²)), where R-bar is avg moving range, d2 = 1.128, and r is the lag-1 sample correlation. This widens limits relative to the standard moving range method (σ = R-bar / d2) when autocorrelation is present.",
                                                                     style = "color: #6c757d; margin-top: 5px; display: block;")
                                                        )
                                                 )
                                               )
                                           )
                                         )
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(4,
                                         textInput("control_title", "Chart Title:", value = "Expectation Chart")
                                  ),
                                  column(4,
                                         textInput("control_subtitle", "Subtitle:", value = "")
                                  ),
                                  column(4,
                                         textInput("control_caption", "Caption:", value = "")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         div(style = "text-align: center;",
                                             downloadButton("download_control_png", "Download PNG", class = "btn-primary"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_control_svg", "Download SVG", class = "btn-success"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_control_pdf", "Download PDF", class = "btn-info")
                                         )
                                  )
                                ),
                                br(),
                                plotOutput("control_chart", height = "600px")
                       ),
                       tabPanel("Trended Expectation Chart",
                                br(),
                                fluidRow(
                                  column(12,
                                         uiOutput("trended_info")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(4,
                                         textInput("trended_title", "Chart Title:", value = "Trended Expectation Chart")
                                  ),
                                  column(4,
                                         textInput("trended_subtitle", "Subtitle:", value = "")
                                  ),
                                  column(4,
                                         textInput("trended_caption", "Caption:", value = "")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         div(style = "text-align: center;",
                                             downloadButton("download_trended_png", "Download PNG", class = "btn-primary"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_trended_svg", "Download SVG", class = "btn-success"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_trended_pdf", "Download PDF", class = "btn-info")
                                         )
                                  )
                                ),
                                br(),
                                plotOutput("trended_chart", height = "600px")
                       ),
                       tabPanel("Cohort Chart",
                                br(),
                                fluidRow(
                                  column(12,
                                         uiOutput("cohort_info")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         conditionalPanel(
                                           condition = "output.data_uploaded",
                                           div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #dee2e6;",
                                               fluidRow(
                                                 column(2,
                                                        selectInput("cohort_grade_var", "Grade Column:",
                                                                    choices = NULL,
                                                                    selected = NULL)
                                                 ),
                                                 column(2,
                                                        numericInput("cohort_start_grade", "Starting Grade:",
                                                                     value = 3, min = 1, max = 12, step = 1)
                                                 ),
                                                 column(2,
                                                        numericInput("cohort_end_grade", "Ending Grade:",
                                                                     value = 8, min = 1, max = 12, step = 1)
                                                 ),
                                                 column(3,
                                                        numericInput("cohort_start_year", "Starting Year:",
                                                                     value = 2018, min = 1900, max = 2030, step = 1)
                                                 ),
                                                 column(3,
                                                        numericInput("cohort_end_year", "Ending Year:",
                                                                     value = 2023, min = 1900, max = 2030, step = 1)
                                                 )
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(12,
                                                        tags$small("Track a demographic cohort's progression through grades over consecutive years. Use filtering controls above to select your cohort (e.g., Asian, Reading).",
                                                                   style = "color: #6c757d;")
                                                 )
                                               )
                                           )
                                         )
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(4,
                                         textInput("cohort_title", "Chart Title:", value = "Cohort Analysis")
                                  ),
                                  column(4,
                                         textInput("cohort_subtitle", "Subtitle:", value = "")
                                  ),
                                  column(4,
                                         textInput("cohort_caption", "Caption:", value = "")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         div(style = "text-align: center;",
                                             downloadButton("download_cohort_png", "Download PNG", class = "btn-primary"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_cohort_svg", "Download SVG", class = "btn-success"),
                                             tags$span(style = "margin: 0 10px;"),
                                             downloadButton("download_cohort_pdf", "Download PDF", class = "btn-info")
                                         )
                                  )
                                ),
                                br(),
                                plotOutput("cohort_chart", height = "600px")
                       ),
                       tabPanel("Auto-correlation Analysis",
                                br(),
                                fluidRow(
                                  column(12,
                                         uiOutput("autocorr_info")
                                  )
                                ),
                                br(),
                                fluidRow(
                                  column(12,
                                         h4("Auto-correlation Coefficients and Sample Correlation"),
                                         tags$div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                                                  tags$p(style = "font-weight: bold; color: #0066cc;", "This analysis shows BOTH autocorrelation coefficients (ACF) and sample correlation coefficients (r) for lag-1, lag-2, and lag-3."),
                                                  tags$p("The Autocorrelation Coefficient (ACF) measures correlation normalized by the overall variance of the series."),
                                                  tags$p("The Sample Correlation Coefficient (r) is the Pearson correlation coefficient calculated from the paired lag-k observations."),
                                                  tags$p(style = "color: #666;", "Values close to 0 indicate no correlation. Values close to ±1 indicate strong correlation.")
                                         ),
                                         br(),
                                         DT::dataTableOutput("autocorr_table")
                                  )
                                )
                       ),
                       tabPanel("Runs Debug",
                                br(),
                                fluidRow(
                                  column(12,
                                         h4("Runs Analysis Debug Information"),
                                         tags$div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                                                  tags$p(style = "font-weight: bold; color: #0066cc;", "This tab shows detailed runs analysis for debugging."),
                                                  tags$p("The runs rule detects 8+ consecutive points on the same side of the centerline."),
                                                  tags$p(style = "color: #666;", "Points marked TRUE have triggered a runs signal (8th point onward in each run).")
                                         ),
                                         br(),
                                         verbatimTextOutput("runs_debug_info"),
                                         br(),
                                         h4("Runs Analysis Data Table"),
                                         DT::dataTableOutput("runs_debug_table")
                                  )
                                )
                       )
           )
    )
  )
)



#####  END OF CODE CHUNK #2  #####




#####  BEGIN CODE CHUNK #3  #####


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




  #####  END OF CODE CHUNK #3  #####




  #####  BEGIN CODE CHUNKS 4-5  #####

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
      ggsave(file, plot = run_plot(), device = "png", width = 20, height = 10, dpi = 300, bg = "white")
    }
  )

  output$download_run_svg <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = run_plot(), device = "svg", width = 20, height = 10, bg = "white")
    }
  )

  output$download_run_pdf <- downloadHandler(
    filename = function() paste0("run_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = run_plot(), device = cairo_pdf, width = 20, height = 10, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = run_plot(), device = "pdf", width = 20, height = 10, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 20, height = 10)
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
      ggsave(file, plot = line_plot(), device = "png", width = 20, height = 10, dpi = 300, bg = "white")
    }
  )

  output$download_line_svg <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "svg", width = 20, height = 10, bg = "white")
    }
  )

  output$download_line_pdf <- downloadHandler(
    filename = function() paste0("line_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = line_plot(), device = cairo_pdf, width = 20, height = 10, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = line_plot(), device = "pdf", width = 20, height = 10, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 20, height = 10)
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
      ggsave(file, plot = control_plot(), device = "png", width = 20, height = 10, dpi = 300, bg = "white")
    }
  )

  output$download_control_svg <- downloadHandler(
    filename = function() paste0("expectation_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = control_plot(), device = "svg", width = 20, height = 10, bg = "white")
    }
  )

  output$download_control_pdf <- downloadHandler(
    filename = function() paste0("expectation_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = control_plot(), device = cairo_pdf, width = 20, height = 10, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = control_plot(), device = "pdf", width = 20, height = 10, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 20, height = 10)
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
      ggsave(file, plot = bar_plot(), device = "png", width = 20, height = 10, dpi = 300, bg = "white")
    }
  )

  output$download_bar_svg <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = bar_plot(), device = "svg", width = 20, height = 10, bg = "white")
    }
  )

  output$download_bar_pdf <- downloadHandler(
    filename = function() paste0("bar_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = bar_plot(), device = cairo_pdf, width = 20, height = 10, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = bar_plot(), device = "pdf", width = 20, height = 10, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 20, height = 10)
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
      ggsave(file, plot = trended_plot(), device = "png", width = 20, height = 10, dpi = 300, bg = "white")
    }
  )

  output$download_trended_svg <- downloadHandler(
    filename = function() paste0("trended_expectation_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = trended_plot(), device = "svg", width = 20, height = 10, bg = "white")
    }
  )

  output$download_trended_pdf <- downloadHandler(
    filename = function() paste0("trended_expectation_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = trended_plot(), device = cairo_pdf, width = 20, height = 10, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = trended_plot(), device = "pdf", width = 20, height = 10, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 20, height = 10)
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
      ggsave(file, plot = cohort_plot(), device = "png", width = 20, height = 10, dpi = 300, bg = "white")
    }
  )

  output$download_cohort_svg <- downloadHandler(
    filename = function() paste0("cohort_chart_", Sys.Date(), ".svg"),
    content = function(file) {
      ggsave(file, plot = cohort_plot(), device = "svg", width = 20, height = 10, bg = "white")
    }
  )

  output$download_cohort_pdf <- downloadHandler(
    filename = function() paste0("cohort_chart_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        ggsave(file, plot = cohort_plot(), device = cairo_pdf, width = 20, height = 10, bg = "white")
      }, error = function(e) {
        tryCatch({
          ggsave(file, plot = cohort_plot(), device = "pdf", width = 20, height = 10, bg = "white")
        }, error = function(e2) {
          pdf(file, width = 20, height = 10)
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









