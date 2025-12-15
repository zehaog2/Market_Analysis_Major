# pdf_report_functions.R
# Simplified PDF Report - Risk Matrix Only

library(ggplot2)
library(gridExtra)
library(grid)

# Suppress ggplot2 deprecation warnings
options(warn = -1)

# Source helper functions
source("advanced_correlation_functions.R")

# Get data safely
get_data_safe <- function(symbol, start_date, end_date) {
  tryCatch({
    data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, 
                       auto.assign = FALSE, warnings = FALSE)
    return(data)
  }, error = function(e) {
    return(NULL)
  })
}

# =============================================================================
# PDF REPORT GENERATOR - TWO PAGES: SPY AND IWM RISK MATRICES
# =============================================================================

generate_pdf_report <- function(all_results, tickers, years_selected, output_filename = NULL) {
  
  if(is.null(output_filename)) {
    output_filename <- "portfolio_risk_matrix.pdf"  # Fixed filename
  }
  
  # Delete old PDF if it exists
  if(file.exists(output_filename)) {
    cat("Removing old PDF:", output_filename, "\n")
    file.remove(output_filename)
  }
  
  cat("=== Generating PDF Report ===\n")
  cat("Output file:", output_filename, "\n")
  
  # Start PDF device with proper settings
  pdf(output_filename, width = 11, height = 8.5)
  
  tryCatch({
    # Page 1: Risk Assessment Matrix vs SPY
    cat("Creating Page 1: SPY analysis...\n")
    create_risk_matrix_page(all_results, tickers, years_selected, "SPY")
    
    # EXPLICITLY START NEW PAGE
    plot.new()
    
    # Page 2: Risk Assessment Matrix vs IWM
    cat("Creating Page 2: IWM analysis...\n")
    all_results_iwm <- run_iwm_analysis(tickers, years_selected)
    create_risk_matrix_page(all_results_iwm, tickers, years_selected, "IWM")
    
    cat("PDF report generated successfully!\n")
    
  }, error = function(e) {
    cat("Error generating PDF:", e$message, "\n")
  }, finally = {
    dev.off()  # Always close the PDF device
  })
  
  return(output_filename)
}

# =============================================================================
# RUN IWM ANALYSIS
# =============================================================================

run_iwm_analysis <- function(tickers, years_selected) {
  # This function mirrors the SPY analysis but uses IWM as benchmark
  # We'll call the same advanced analysis but with IWM
  
  benchmark <- "IWM"
  
  # Calculate dates
  end_date <- Sys.Date()
  start_date <- end_date - (years_selected * 365)
  
  cat("Fetching IWM data...\n")
  benchmark_data <- get_data_safe(benchmark, start_date, end_date)
  
  if(is.null(benchmark_data)) {
    cat("WARNING: Could not fetch IWM data\n")
    return(list())
  }
  
  benchmark_prices <- if(ncol(benchmark_data) >= 6) Ad(benchmark_data) else Cl(benchmark_data)
  benchmark_returns <- na.omit(diff(log(benchmark_prices)))
  
  # Analyze each stock against IWM
  all_results <- list()
  
  for(ticker in tickers) {
    cat("  Analyzing", ticker, "vs IWM...\n")
    
    stock_data <- get_data_safe(ticker, start_date, end_date)
    
    if(!is.null(stock_data)) {
      stock_prices <- if(ncol(stock_data) >= 6) Ad(stock_data) else Cl(stock_data)
      stock_returns <- na.omit(diff(log(stock_prices)))
      
      # Run advanced correlation analysis
      result <- advanced_correlation_analysis(stock_returns, benchmark_returns, ticker, benchmark)
      all_results[[ticker]][[benchmark]] <- result
    }
  }
  
  return(all_results)
}

# =============================================================================
# RISK ASSESSMENT MATRIX PAGE (WITH BENCHMARK PARAMETER)
# =============================================================================

create_risk_matrix_page <- function(all_results, tickers, years_selected, benchmark = "SPY") {
  # Source the risk plot functions
  source("risk_plot_functions.R")
  
  # Create the risk matrix plot - pass benchmark as second parameter
  risk_plot <- create_risk_matrix_plot(all_results, benchmark, use_icons = FALSE)
  
  # Add title and metadata
  benchmark_name <- if(benchmark == "SPY") "S&P 500" else "Russell 2000"
  
  title_grob <- textGrob(
    paste("Portfolio Risk Assessment Matrix vs", benchmark, paste0("(", benchmark_name, ")")),
    gp = gpar(fontsize = 18, fontface = "bold", col = "darkblue"),
    just = "center"
  )
  
  subtitle_grob <- textGrob(
    paste0("Analysis Period: ", years_selected, ifelse(years_selected == 1, " Year", " Years"),
           " | Generated: ", format(Sys.Date(), "%B %d, %Y")),
    gp = gpar(fontsize = 12, col = "gray40"),
    just = "center"
  )
  
  stocks_grob <- textGrob(
    paste("Stocks Analyzed:", paste(tickers, collapse = ", ")),
    gp = gpar(fontsize = 10, col = "gray60"),
    just = "center"
  )
  
  # Combine title, subtitle, plot
  final_plot <- arrangeGrob(
    title_grob,
    subtitle_grob,
    stocks_grob,
    risk_plot,
    ncol = 1,
    heights = c(0.8, 0.5, 0.5, 10)
  )
  
  # Print to PDF
  grid.draw(final_plot)
}