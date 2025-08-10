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