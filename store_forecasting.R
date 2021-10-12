# Libraries  
library(tidyverse)
library(prophet)

# Data comes from Kaggle.
# We are making a 3 month forecast of sales for
# 50 different items from 10 different stores.
# That is, 500 individual forecasting models.

store_data <- read_csv("data/store_train_data.csv")

# Method: nest data for the 500 models and fit a prophet object to each.
nested.df <- store_data %>% 
  rename(y = sales, ds = date) %>% 
  group_by(store, item) %>% 
  nest() %>% 
  ungroup()

# Create prophet model function and make predictions
prophet.mod <- function(df) {
  model <- prophet(df = df)
  return(model)
}

stores_model.df <- nested.df %>%
  mutate(model = map(data, prophet_model))

# Prepare forecasting horizon
forecast_horizon.df <- tibble(ds = seq(as.Date("2018-01-01"), as.Date("2018-03-31"), by = "days"))

# Make forecast with each model
g <- NULL
for (i in 1:nrow(stores_model.df)) {
  g$forecast[i] <- nest(predict(stores_model.df$model[[i]],
                                forecast_horizon.df) %>%
                          mutate(name = i)
  )
}

forecasts <- do.call(dplyr::bind_rows, g)


# Bind historical data
j <- NULL
for (i in 1:nrow(stores_model.df)) {
  j$history[i] <- nest(stores_model.df$model[[i]]$history %>% 
                         mutate(name = i)
  )
}

histories <- do.call(dplyr::bind_rows, j)

# Bind historical data and predictions
forecasts_full.df <- histories %>% 
  bind_rows(forecasts) %>% 
  left_join(stores_model.df %>% 
              mutate(name = row_number()) %>% 
              select(store, item, name)) %>% 
  mutate(forecast_id = paste0("Store: ", store, ". Item: ",item))
