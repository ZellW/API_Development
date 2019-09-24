# LEARNING LAB 10 - PLUMBER APIS ----
# FIFA 2019 PLAYER PREDICTION ----
# DATA SET: https://www.kaggle.com/karangadiya/fifa19

#* @apiTitle FIFA 2019 Player Prediction API
#* @apiDescription Endpoints for prediction of player market value
library(plumber)
library(tidyverse)
library(parsnip)
library(xgboost)

model_xgboost <- read_rds("model_xgboost.rds")

#* Optimize the portfolio of given stock symbols
#* @param nationality: Nationality of player
#* @param age: Age of player
#* @param overall: FIFA Rating of player
#* @get /predict_market_value
function(nationality = "Argentina", age = 30, overall = 85) {
    
    tibble(
        Age         = as.numeric(age),
        Nationality = str_to_title(nationality),
        Overall     = as.numeric(overall)
    ) %>%
        predict(model_xgboost, new_data = .) %>%
        rename(MarketValue = .pred) %>%
        mutate(MarketValue = scales::dollar(MarketValue, prefix = "€", accuracy = 1))
    
}

