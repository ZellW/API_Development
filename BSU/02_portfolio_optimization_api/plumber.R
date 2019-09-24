# LEARNING LAB 10 - PLUMBER APIS ----
# STOCK OPTIMIZATION ----

#* @apiTitle Stock Optimization API
#* @apiDescription Endpoints for working with SP500 Data
library(plumber)
library(tidyquant)
library(jsonlite)

source("portfolio_optimization.R")

#* Parse and run optimization
#* @filter optimize
function(req, res) { #req is a special function with plumber
    
    # Only parse responseBody
    if (str_detect(req$PATH_INFO, "optimize")) {
        
        req$stock_symbols <- tryCatch(fromJSON(req$postBody), error = function(e) NULL)
        
        # Handle no data or bad format
        if (is.null(req$stock_symbols)) {
            res$status <- 400
            return(
                list(error = "No JSON data included in the request body.")
            )
        }
        
        # Add optimization logic
        assets <- unlist(req$stock_symbols)
        req$optimization_tbl <- optimize_portfolio(assets = assets, iter = 50, seed = 123)
        
    }
    
    forward()
}

#* Optimize the portfolio of given stock symbols
#* @post /optimize/stock_symbols
function(req) {
    # browser() # useful fpr troubleshooting
    req$optimization_tbl %>%
        collect_best_portfolio_allocation()
    
}



