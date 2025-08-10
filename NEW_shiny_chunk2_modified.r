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