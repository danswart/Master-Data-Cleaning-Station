# Data Cleaning Script for EDA Dashboard
# This script standardizes CSV/Excel files for the Ultimate EDA Shiny App

library(dplyr)
library(readr)
library(readxl)
library(janitor)
library(lubridate)

# Data exploration functions
explore_data <- function(data, show_all_columns = FALSE) {
  cat("=== DATA EXPLORATION REPORT ===\n\n")

  # Basic dimensions
  cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")

  # Column names (great for planning renames)
  cat("Column Names:\n")
  cat(paste(1:ncol(data), names(data), sep = ". "), sep = "\n")
  cat("\n")

  # Data structure (types and sample values)
  cat("Data Structure:\n")
  glimpse(data)
  cat("\n")

  # First few rows (see actual values)
  cat("First 6 rows:\n")
  print(head(data, 6))
  cat("\n")

  # Identify potential date columns
  potential_dates <- names(data)[sapply(data, function(x) any(grepl("\\d{4}", as.character(x))))]
  if(length(potential_dates) > 0) {
    cat("Potential date columns:", paste(potential_dates, collapse = ", "), "\n")
  }

  # Identify potential value columns (numeric)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if(length(numeric_cols) > 0) {
    cat("Numeric columns:", paste(numeric_cols, collapse = ", "), "\n")
  }

  # Identify categorical columns
  categorical_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  if(length(categorical_cols) > 0) {
    cat("Categorical columns:", paste(categorical_cols, collapse = ", "), "\n")
  }

  cat("\n")
  return(invisible(data))
}



# Quick exploration function (just the essentials)
quick_look <- function(data) {
  cat("Quick Look:\n")
  cat("Dimensions:", nrow(data), "x", ncol(data), "\n")
  cat("Columns:", paste(names(data), collapse = ", "), "\n\n")
  print(head(data, 6))
}






# Function to load and explore data before cleaning
load_and_explore <- function(file_path, header = TRUE) {
  cat("Loading file:", file_path, "\n\n")

  # Load the raw data
  file_ext <- tools::file_ext(file_path)

  if(file_ext == "csv") {
    raw_data <- read_csv(file_path, col_names = header, show_col_types = FALSE)
  } else if(file_ext %in% c("xlsx", "xls")) {
    raw_data <- read_excel(file_path, col_names = header)
  } else {
    stop("File must be CSV or Excel format")
  }

  # Explore the raw data
  explore_data(raw_data)

  return(raw_data)
}




# Function to clean and standardize data for EDA dashboard
clean_eda_data <- function(file_path, header = TRUE, date_col = NULL, value_col = NULL) {

  # Detect file type and load data
  file_ext <- tools::file_ext(file_path)

  if(file_ext == "csv") {
    raw_data <- read_csv(file_path, col_names = header, show_col_types = FALSE)
  } else if(file_ext %in% c("xlsx", "xls")) {
    raw_data <- read_excel(file_path, col_names = header)
  } else {
    stop("File must be CSV or Excel format")
  }

  # Clean column names (remove spaces, special characters, standardize case)
  clean_data <- raw_data %>%
    clean_names() %>%  # janitor function: snake_case, remove special chars
    rename_with(~ gsub("_+", "_", .x)) %>%  # Remove multiple underscores
    rename_with(~ gsub("^_|_$", "", .x))    # Remove leading/trailing underscores

  # Auto-detect date column if not specified
  if(is.null(date_col)) {
    date_patterns <- c("date", "time", "week", "month", "year", "period", "day")
    date_col_candidates <- names(clean_data)[grepl(paste(date_patterns, collapse = "|"),
                                                   names(clean_data), ignore.case = TRUE)]

    if(length(date_col_candidates) > 0) {
      date_col <- date_col_candidates[1]
      cat("Auto-detected date column:", date_col, "\n")
    }
  }

  # Auto-detect value column if not specified
  if(is.null(value_col)) {
    value_patterns <- c("value", "pct", "percent", "amount", "count", "measure",
                       "score", "rate", "revenue", "sales", "total", "sum")
    value_col_candidates <- names(clean_data)[grepl(paste(value_patterns, collapse = "|"),
                                                    names(clean_data), ignore.case = TRUE)]

    if(length(value_col_candidates) > 0) {
      value_col <- value_col_candidates[1]
      cat("Auto-detected value column:", value_col, "\n")
    }
  }

  # Standardize column names
  standardized_data <- clean_data

  # Rename date column to 'date' if found
  if(!is.null(date_col) && date_col %in% names(clean_data)) {
    standardized_data <- standardized_data %>%
      rename(date = !!sym(date_col))

    # Try to parse as date if it's character
    if(is.character(standardized_data$date)) {
      standardized_data$date <- parse_date_time(standardized_data$date,
                                               orders = c("ymd", "mdy", "dmy", "ym", "my"))
    }
  }

  # Rename value column to 'value' if found
  if(!is.null(value_col) && value_col %in% names(clean_data)) {
    standardized_data <- standardized_data %>%
      rename(value = !!sym(value_col))

    # Ensure value column is numeric
    if(!is.numeric(standardized_data$value)) {
      standardized_data$value <- as.numeric(standardized_data$value)
    }
  }

  # Reorder columns: Choose your preferred style
  # Style 1: R/Tidy approach - date, value, then categories
  # Style 2: Excel approach - date, categories, then value

  excel_style <- FALSE  # Set to TRUE for Excel-style ordering

  col_order <- c()

  if("date" %in% names(standardized_data)) {
    col_order <- c(col_order, "date")
  }

  if(!excel_style && "value" %in% names(standardized_data)) {
    col_order <- c(col_order, "value")
  }

  # Add remaining columns alphabetically
  remaining_cols <- names(standardized_data)[!names(standardized_data) %in% col_order]
  col_order <- c(col_order, sort(remaining_cols))

  # For Excel style, add value at the end
  if(excel_style && "value" %in% names(standardized_data)) {
    col_order <- c(col_order, "value")
  }

  # Reorder and sort data
  final_data <- standardized_data %>%
    select(all_of(col_order)) %>%
    arrange(if("date" %in% names(.)) date else 1, across(everything()))

  return(final_data)
}


# Convert specific column from whole number percentages to decimals
convert_column_to_decimal <- function(data) {
  # Safety checks
  if (!"units" %in% names(data)) {
    stop("No 'units' column found")
  }
  if (!"value" %in% names(data)) {
    stop("No 'value' column found")
  }

  # Count how many rate rows we're transforming
  rate_count <- sum(data$units == "rate", na.rm = TRUE)

  # Transform only rate values
  data$value[data$units == "rate"] <- data$value[data$units == "rate"] / 100

  # Helpful message
  cat("✓ Transformed", rate_count, "rate values from whole numbers to decimals\n")

  return(data)
}

# Usage:
# data <- convert_column_to_decimal(data)





# Function to save cleaned data
save_cleaned_data <- function(cleaned_data, output_path) {
  write_csv(cleaned_data, output_path)
  cat("Cleaned data saved to:", output_path, "\n")
  cat("Dimensions:", nrow(cleaned_data), "rows,", ncol(cleaned_data), "columns\n")
  cat("Columns:", paste(names(cleaned_data), collapse = ", "), "\n")
}

# Example usage:
# ===============

# Step 1: Load and explore your data first
# raw_data <- load_and_explore("your_file.csv")

# Alternative: Load then explore separately
# raw_data <- read_csv("your_file.csv")
# explore_data(raw_data)

# Quick peek (minimal output)
# quick_look(raw_data)

# Step 2: Clean the data after you know the column structure
# cleaned_data <- clean_eda_data("your_file.csv",
#                                date_col = "week_ended",
#                                value_col = "pct")

# Step 3: Explore the cleaned data
# explore_data(cleaned_data)

# Step 4: Save for dashboard
# save_cleaned_data(cleaned_data, "dashboard_ready.csv")

# Example batch processing multiple files:
# ========================================
#
# file_list <- list.files("raw_data/", pattern = "*.csv", full.names = TRUE)
#
# for(file in file_list) {
#   cat("\nProcessing:", file, "\n")
#   cleaned <- clean_eda_data(file)
#
#   # Create output filename
#   output_name <- paste0("cleaned_", basename(tools::file_path_sans_ext(file)), ".csv")
#   save_cleaned_data(cleaned, file.path("cleaned_data/", output_name))
# }
