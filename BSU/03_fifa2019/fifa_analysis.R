library(tidyverse)
library(parsnip)
library(yardstick)

setwd("~/GitHub/R_Programming/API_Testing/BSU")

data_raw_tbl <- read_csv("data/data.csv")

data_raw_tbl %>% glimpse()

# 10 Data Prep ----
data_prep_tbl <- data_raw_tbl %>%
    select(Age, Nationality, Overall, Value) %>%
    mutate(unit = str_sub(Value, -1, -1)) %>%
    mutate(Value = str_sub(Value, 2, -1) %>%
               str_replace_all("[:alpha:]", "") %>%
               as.numeric() ) %>%
    mutate(Value = case_when(
        unit == "M" ~ Value * 1e6,
        TRUE ~ Value * 1e3
        )) %>%
    select(-unit)

# 2.0 Visualize Features ----
data_prep_tbl %>% ggplot(aes(Age, Value)) + geom_point(alpha = 0.2)

data_prep_tbl %>% ggplot(aes(Overall, Value)) + geom_point(alpha = 0.2) 

# 3.0 Model ----

message(
"THIS MODEL HAS NOT BEEN TESTED OUT OF SAMPLE. 
TAKE DS4B 101-R TO LEARN HOW TO MAKE A GOOD MODEL!")

model_xgboost <- boost_tree(
        mode = "regression", 
        mtry = 100, trees = 100, min_n = 5,
        tree_depth = 5, learn_rate = 0.1, loss_reduction = 0.01) %>%
    set_engine(engine = "xgboost") %>%
    fit(Value ~ ., data = data_prep_tbl)

model_xgboost <- read_rds("03_fifa2019/model_xgboost.rds")

predict(model_xgboost, new_data = data_prep_tbl) %>%
    bind_cols(data_prep_tbl %>% select(Value)) %>%
    metrics(.pred, Value)

# 4.0 Save Model ----
write_rds(x = model_xgboost, path = "03_fifa2019/model_xgboost.rds")
