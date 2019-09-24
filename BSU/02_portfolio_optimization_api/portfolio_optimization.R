# PORTFOLIO OPTIMIZATION ----

library(tidyquant)

sp500_stock_prices_tbl <- read_rds("../data/sp500_stock_prices.rds")

# assets <- c("AAPL", "GOOG", "NFLX", "GE")
# iter <- 10
# seed <- 123
# optimize_portfolio(assets, 50, 123)

optimize_portfolio <- function(assets, iter, seed) {
    
    returns_m_components_tbl <- sp500_stock_prices_tbl %>%
        filter(symbol %in% assets) %>%
        group_by(symbol) %>%
        tq_transmute(select     = adjusted, 
                     mutate_fun = periodReturn, 
                     period     = "monthly") %>%
        ungroup()
    
    weight_iterator <- function(assets, iter = 100, seed = NULL) {
        
        n <- length(assets)
        
        if (!is.null(seed)) set.seed(seed)
        mtx <- matrix(runif(n = iter*n, min = 0, max = 1), nrow = n)
        
        mtx_normalized <- mtx %*% diag(1/colSums(mtx))
        
        vectorized_output <- as.vector(mtx_normalized)
        
        return(vectorized_output)
        
    }
    
    weights_tbl <- tibble(
        portfolio_id = rep(1:iter, each = length(assets)),
        symbol  = rep(assets, times = iter),
        weights = weight_iterator(assets, iter = iter, seed = seed)
    ) %>%
        group_by(portfolio_id)
    
    portfolio_optim_tbl <- weights_tbl %>%
        nest(.key = portfolio_weights) %>%
        
        # Map tq_portfolio() to nested weights
        mutate(portfolio_agg = map(portfolio_weights, ~ tq_portfolio(
            data = returns_m_components_tbl,
            assets_col  = symbol,
            returns_col = monthly.returns,
            weights     = .x,
            rebalance_on = "quarters"
        ))) %>%
        
        # Map tq_performance() to nested portfolio aggregations
        mutate(sharp_ratio = map(portfolio_agg, ~ tq_performance(
            data = .x,
            Ra = portfolio.returns,
            performance_fun = SharpeRatio.annualized,
            scale = 12
        )))
    
    return(portfolio_optim_tbl)
    
}

collect_best_portfolio_allocation <- function(data) {
    data %>%
        unnest(sharp_ratio) %>%
        filter(`AnnualizedSharpeRatio(Rf=0%)` == max(`AnnualizedSharpeRatio(Rf=0%)`)) %>%
        select(portfolio_weights, contains("Sharpe")) %>%
        unnest(portfolio_weights) %>%
        spread(symbol, weights) %>%
        rename(SharpeRatio = `AnnualizedSharpeRatio(Rf=0%)`)
}
