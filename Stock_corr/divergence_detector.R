#!/usr/bin/env Rscript
# divergence_detector.R
# Detects when stocks diverge from their expected correlation patterns

library(quantmod)
library(jsonlite)
library(ggplot2)
library(gridExtra)
library(grid)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

get_data_safe <- function(symbol, start_date, end_date) {
  tryCatch({
    data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, 
                       auto.assign = FALSE, warnings = FALSE)
    return(data)
  }, error = function(e) {
    return(NULL)
  })
}

load_stock_correlation_cache <- function() {
  # Load from stock_info_cache.json if it exists
  if(file.exists("stock_info_cache.json")) {
    cache <- fromJSON("stock_info_cache.json")
    return(cache)
  }
  return(NULL)
}

get_stock_correlation_beta <- function(ticker, benchmark = "SPY", lookback_years = 1) {
  # Calculate correlation and beta against specified benchmark
  
  end_date <- Sys.Date()
  start_date <- end_date - (lookback_years * 365)
  
  # Get stock and benchmark data
  stock_data <- get_data_safe(ticker, start_date, end_date)
  benchmark_data <- get_data_safe(benchmark, start_date, end_date)
  
  if(is.null(stock_data) || is.null(benchmark_data)) {
    return(list(correlation = NA, beta = NA))
  }
  
  # Calculate returns
  stock_prices <- if(ncol(stock_data) >= 6) Ad(stock_data) else Cl(stock_data)
  benchmark_prices <- if(ncol(benchmark_data) >= 6) Ad(benchmark_data) else Cl(benchmark_data)
  
  stock_returns <- na.omit(diff(log(stock_prices)))
  benchmark_returns <- na.omit(diff(log(benchmark_prices)))
  
  # Align dates
  common_dates <- intersect(index(stock_returns), index(benchmark_returns))
  
  if(length(common_dates) < 20) {
    return(list(correlation = NA, beta = NA))
  }
  
  stock_aligned <- as.numeric(stock_returns[common_dates])
  benchmark_aligned <- as.numeric(benchmark_returns[common_dates])
  
  # Calculate correlation and beta
  correlation <- cor(stock_aligned, benchmark_aligned, use = "complete.obs")
  beta <- cov(stock_aligned, benchmark_aligned) / var(benchmark_aligned)
  
  return(list(correlation = correlation, beta = beta))
}

# =============================================================================
# DIVERGENCE DETECTION
# =============================================================================

detect_divergence <- function(ticker, benchmark = "SPY", lookback_days = 20, profile_years = 1) {
  cat("Analyzing", ticker, "vs", benchmark, "...\n")
  
  # Get stock profile (correlation and beta)
  profile <- get_stock_correlation_beta(ticker, benchmark, profile_years)
  
  if(is.na(profile$correlation) || is.na(profile$beta)) {
    cat("  WARNING: Could not calculate profile for", ticker, "\n")
    return(NULL)
  }
  
  cat("  Profile: correlation =", round(profile$correlation, 3), 
      ", beta =", round(profile$beta, 2), "\n")
  
  # Get recent data for divergence detection
  end_date <- Sys.Date()
  start_date <- end_date - (lookback_days + 10)  # Extra buffer
  
  stock_data <- get_data_safe(ticker, start_date, end_date)
  benchmark_data <- get_data_safe(benchmark, start_date, end_date)
  
  if(is.null(stock_data) || is.null(benchmark_data)) {
    cat("  WARNING: Could not fetch recent data for", ticker, "\n")
    return(NULL)
  }
  
  # Calculate daily returns
  stock_prices <- if(ncol(stock_data) >= 6) Ad(stock_data) else Cl(stock_data)
  benchmark_prices <- if(ncol(benchmark_data) >= 6) Ad(benchmark_data) else Cl(benchmark_data)
  
  stock_returns <- diff(log(stock_prices))
  benchmark_returns <- diff(log(benchmark_prices))
  
  # Align dates
  common_dates <- intersect(index(stock_returns), index(benchmark_returns))
  
  if(length(common_dates) < lookback_days) {
    cat("  WARNING: Not enough recent data for", ticker, "\n")
    return(NULL)
  }
  
  # Take most recent data
  recent_dates <- tail(common_dates, lookback_days)
  
  stock_recent <- as.numeric(stock_returns[recent_dates])
  benchmark_recent <- as.numeric(benchmark_returns[recent_dates])
  
  # Calculate expected vs actual moves
  expected_moves <- benchmark_recent * profile$correlation * profile$beta
  actual_moves <- stock_recent
  divergences <- (actual_moves - expected_moves) * 100  # Convert to percentage points
  
  # Remove any NAs from divergences
  valid_indices <- !is.na(divergences)
  divergences_clean <- divergences[valid_indices]
  
  if(length(divergences_clean) < 5) {
    cat("  WARNING: Not enough valid divergence data for", ticker, "\n")
    return(NULL)
  }
  
  # Calculate cumulative divergence (on clean data)
  cumulative_div <- cumsum(divergences_clean)
  
  # Identify significant divergence days (>1% difference)
  significant_days <- which(abs(divergences_clean) > 1.0)
  
  # Calculate divergence metrics
  avg_divergence <- mean(divergences_clean, na.rm = TRUE)
  recent_5day_div <- mean(tail(divergences_clean, 5), na.rm = TRUE)
  current_cumulative <- tail(cumulative_div, 1)
  
  # Divergence score: combination of recent and cumulative
  divergence_score <- recent_5day_div + (current_cumulative / 10)
  
  # Handle NA divergence scores
  if(is.na(divergence_score) || is.nan(divergence_score) || !is.finite(divergence_score)) {
    cat("  WARNING: Could not calculate divergence score for", ticker, "\n")
    cat("  Debug: avg_div =", avg_divergence, "recent_5day =", recent_5day_div, 
        "cumulative =", current_cumulative, "\n")
    return(NULL)
  }
  
  # Classify divergence type
  if(divergence_score > 1.5) {
    divergence_type <- "Strong Positive"
    signal <- "BULLISH"
    color <- "darkgreen"
  } else if(divergence_score > 0.5) {
    divergence_type <- "Weak Positive"
    signal <- "Slightly Bullish"
    color <- "green"
  } else if(divergence_score < -1.5) {
    divergence_type <- "Strong Negative"
    signal <- "BEARISH"
    color <- "darkred"
  } else if(divergence_score < -0.5) {
    divergence_type <- "Weak Negative"
    signal <- "Slightly Bearish"
    color <- "red"
  } else {
    divergence_type <- "Neutral"
    signal <- "No Clear Signal"
    color <- "gray"
  }
  
  cat("  Divergence Score:", round(divergence_score, 2), "-", divergence_type, "\n")
  
  return(list(
    ticker = ticker,
    benchmark = benchmark,
    correlation = profile$correlation,
    beta = profile$beta,
    divergence_score = divergence_score,
    divergence_type = divergence_type,
    signal = signal,
    color = color
  ))
}

# =============================================================================
# VISUALIZATION - SUMMARY TABLE ONLY
# =============================================================================

create_summary_table_plot <- function(all_results_spy, all_results_iwm) {
  # Filter out NULL results
  valid_spy <- all_results_spy[!sapply(all_results_spy, is.null)]
  valid_iwm <- all_results_iwm[!sapply(all_results_iwm, is.null)]
  
  if(length(valid_spy) == 0 && length(valid_iwm) == 0) {
    plot.new()
    text(0.5, 0.5, "No valid results", size = 6, col = "red")
    return()
  }
  
  # Get all unique tickers from both benchmarks
  all_tickers <- unique(c(names(valid_spy), names(valid_iwm)))
  
  # Create summary data frames with consistent ordering
  if(length(valid_spy) > 0) {
    summary_spy <- data.frame(
      Ticker = sapply(valid_spy, function(x) x$ticker),
      Correlation = sapply(valid_spy, function(x) round(x$correlation, 2)),
      Beta = sapply(valid_spy, function(x) round(x$beta, 2)),
      Div_Score = sapply(valid_spy, function(x) round(x$divergence_score, 2)),
      Signal = sapply(valid_spy, function(x) x$signal),
      Type = sapply(valid_spy, function(x) x$divergence_type)
    )
    # Keep original ticker order from all_tickers
    summary_spy$Ticker <- factor(summary_spy$Ticker, levels = all_tickers)
    summary_spy <- summary_spy[order(summary_spy$Ticker), ]
  } else {
    summary_spy <- NULL
  }
  
  if(length(valid_iwm) > 0) {
    summary_iwm <- data.frame(
      Ticker = sapply(valid_iwm, function(x) x$ticker),
      Correlation = sapply(valid_iwm, function(x) round(x$correlation, 2)),
      Beta = sapply(valid_iwm, function(x) round(x$beta, 2)),
      Div_Score = sapply(valid_iwm, function(x) round(x$divergence_score, 2)),
      Signal = sapply(valid_iwm, function(x) x$signal),
      Type = sapply(valid_iwm, function(x) x$divergence_type)
    )
    # Keep original ticker order from all_tickers
    summary_iwm$Ticker <- factor(summary_iwm$Ticker, levels = all_tickers)
    summary_iwm <- summary_iwm[order(summary_iwm$Ticker), ]
  } else {
    summary_iwm <- NULL
  }
  
  # Create side-by-side tables
  plot.new()
  
  # Main title
  text(0.5, 0.97, "Divergence Summary", cex = 2.0, font = 2)
  text(0.5, 0.93, paste("Analysis Date:", format(Sys.Date(), "%B %d, %Y")), cex = 1.1)
  
  # SPY Table (Left side)
  if(!is.null(summary_spy)) {
    text(0.25, 0.88, "vs SPY (S&P 500)", cex = 1.4, font = 2, col = "darkblue")
    
    # Headers
    y_start <- 0.82
    text(0.10, y_start, "Ticker", cex = 1.0, font = 2)
    text(0.20, y_start, "Corr", cex = 1.0, font = 2)
    text(0.28, y_start, "Beta", cex = 1.0, font = 2)
    text(0.36, y_start, "Score", cex = 1.0, font = 2)
    text(0.45, y_start, "Signal", cex = 1.0, font = 2)
    
    segments(0.05, y_start - 0.015, 0.50, y_start - 0.015, lwd = 1.5)
    
    # Rows
    y_pos <- y_start - 0.04
    row_height <- 0.04
    
    for(i in 1:min(nrow(summary_spy), 20)) {
      row <- summary_spy[i, ]
      text_color <- if(grepl("BULLISH", row$Signal)) "darkgreen" 
      else if(grepl("BEARISH", row$Signal)) "darkred"
      else "black"
      
      text(0.10, y_pos, row$Ticker, cex = 0.9, font = 2)
      text(0.20, y_pos, row$Correlation, cex = 0.9)
      text(0.28, y_pos, row$Beta, cex = 0.9)
      text(0.36, y_pos, row$Div_Score, cex = 0.9, col = text_color, font = 2)
      text(0.45, y_pos, row$Signal, cex = 0.8, col = text_color)
      
      y_pos <- y_pos - row_height
    }
  }
  
  # IWM Table (Right side)
  if(!is.null(summary_iwm)) {
    text(0.75, 0.88, "vs IWM (Russell 2000)", cex = 1.4, font = 2, col = "darkgreen")
    
    # Headers
    y_start <- 0.82
    text(0.60, y_start, "Ticker", cex = 1.0, font = 2)
    text(0.70, y_start, "Corr", cex = 1.0, font = 2)
    text(0.78, y_start, "Beta", cex = 1.0, font = 2)
    text(0.86, y_start, "Score", cex = 1.0, font = 2)
    text(0.95, y_start, "Signal", cex = 1.0, font = 2)
    
    segments(0.55, y_start - 0.015, 1.00, y_start - 0.015, lwd = 1.5)
    
    # Rows
    y_pos <- y_start - 0.04
    row_height <- 0.04
    
    for(i in 1:min(nrow(summary_iwm), 20)) {
      row <- summary_iwm[i, ]
      text_color <- if(grepl("BULLISH", row$Signal)) "darkgreen" 
      else if(grepl("BEARISH", row$Signal)) "darkred"
      else "black"
      
      text(0.60, y_pos, row$Ticker, cex = 0.9, font = 2)
      text(0.70, y_pos, row$Correlation, cex = 0.9)
      text(0.78, y_pos, row$Beta, cex = 0.9)
      text(0.86, y_pos, row$Div_Score, cex = 0.9, col = text_color, font = 2)
      text(0.95, y_pos, row$Signal, cex = 0.8, col = text_color)
      
      y_pos <- y_pos - row_height
    }
  }
  
  # Legend at bottom
  text(0.5, 0.08, "Divergence Score: Positive = Stock outperforming expectations", 
       cex = 0.9, col = "darkgreen")
  text(0.5, 0.04, "Divergence Score: Negative = Stock underperforming expectations", 
       cex = 0.9, col = "darkred")
}

# =============================================================================
# PDF REPORT GENERATOR - SINGLE PAGE ONLY
# =============================================================================

generate_divergence_pdf <- function(all_results_spy, all_results_iwm, output_filename = NULL) {
  if(is.null(output_filename)) {
    output_filename <- "divergence_report.pdf"  # Fixed filename
  }
  
  # Delete old PDF if it exists
  if(file.exists(output_filename)) {
    cat("Removing old PDF:", output_filename, "\n")
    file.remove(output_filename)
  }
  
  cat("\n=== Generating Divergence PDF Report ===\n")
  cat("Output file:", output_filename, "\n")
  
  # Filter out NULL results first
  valid_spy <- all_results_spy[!sapply(all_results_spy, is.null)]
  valid_iwm <- all_results_iwm[!sapply(all_results_iwm, is.null)]
  
  if(length(valid_spy) == 0 && length(valid_iwm) == 0) {
    cat("ERROR: No valid results to generate PDF\n")
    return(NULL)
  }
  
  pdf(output_filename, width = 11, height = 8.5)
  
  tryCatch({
    # Single page: Side-by-side summary tables
    create_summary_table_plot(all_results_spy, all_results_iwm)
    
    cat("PDF report generated successfully!\n")
    
  }, error = function(e) {
    cat("Error generating PDF:", e$message, "\n")
  }, finally = {
    dev.off()
  })
  
  return(output_filename)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  cat("=== Divergence Detector ===\n\n")
  
  # Check arguments
  if(length(args) < 1) {
    cat("Usage: Rscript divergence_detector.R config.json [lookback_days] [profile_years]\n")
    cat("\n")
    cat("Arguments:\n")
    cat("  config.json    - Portfolio configuration file\n")
    cat("  lookback_days  - Days to analyze for divergence (default: 20)\n")
    cat("  profile_years  - Years for correlation/beta profile (default: 1)\n")
    cat("\n")
    cat("Example: Rscript divergence_detector.R config.json 30 2\n")
    quit(status = 1)
  }
  
  json_file <- args[1]
  lookback_days <- if(length(args) >= 2) as.numeric(args[2]) else 20
  profile_years <- if(length(args) >= 3) as.numeric(args[3]) else 1
  
  # Validate parameters
  if(is.na(lookback_days) || lookback_days < 5 || lookback_days > 60) {
    cat("Invalid lookback_days. Using default: 20\n")
    lookback_days <- 20
  }
  
  if(is.na(profile_years) || profile_years < 1 || profile_years > 5) {
    cat("Invalid profile_years. Using default: 1\n")
    profile_years <- 1
  }
  
  cat("Configuration:\n")
  cat("  Portfolio:", json_file, "\n")
  cat("  Lookback period:", lookback_days, "days\n")
  cat("  Profile period:", profile_years, "year(s)\n\n")
  
  # Load portfolio
  if(!file.exists(json_file)) {
    cat("ERROR: Configuration file not found:", json_file, "\n")
    quit(status = 1)
  }
  
  config <- fromJSON(json_file)
  tickers <- config$portfolio$stocks
  
  if(is.null(tickers) || length(tickers) == 0) {
    cat("ERROR: No stocks found in portfolio\n")
    quit(status = 1)
  }
  
  tickers <- toupper(tickers[!is.na(tickers) & tickers != ""])
  
  cat("Analyzing", length(tickers), "stocks:", paste(tickers, collapse = ", "), "\n\n")
  
  # Detect divergences for all stocks vs SPY
  cat("=== Analyzing vs SPY ===\n")
  all_results_spy <- list()
  
  for(ticker in tickers) {
    result <- detect_divergence(ticker, "SPY", lookback_days, profile_years)
    all_results_spy[[ticker]] <- result
  }
  
  # Detect divergences for all stocks vs IWM
  cat("\n=== Analyzing vs IWM ===\n")
  all_results_iwm <- list()
  
  for(ticker in tickers) {
    result <- detect_divergence(ticker, "IWM", lookback_days, profile_years)
    all_results_iwm[[ticker]] <- result
  }
  
  # Generate PDF report
  cat("\n")
  pdf_file <- generate_divergence_pdf(all_results_spy, all_results_iwm)
  
  cat("\n=== Analysis Complete ===\n")
  cat("Results saved to:", pdf_file, "\n")
}

# Run main
if(!interactive()) {
  main()
}