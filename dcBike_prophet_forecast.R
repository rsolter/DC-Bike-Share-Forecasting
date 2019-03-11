
# Resources 

# https://pythondata.com/forecasting-time-series-data-with-prophet-part-1/
# https://pythondata.com/forecasting-time-series-data-with-prophet-part-2/
# https://facebook.github.io/prophet/docs/quick_start.html#r-api
# https://www.analyticsvidhya.com/blog/2018/05/generate-accurate-forecasts-facebook-prophet-python-r/ 



# Set Up  ----

# Forecasting DC BikeShare Data

library(tidyverse)
library(prophet)
library(scales)
library(forecast)
library(tsoutliers)


# Loading cleaned bike data from Sept, 2010 - Oct, 2018 
  # source: (https://s3.amazonaws.com/capitalbikeshare-data/index.html)
load(file="bike_trips.rdata")
  

# Viz  ---- 

# Histogram of Daily Ride Totals
ggplot(bike_trips, aes(x=n)) + 
  geom_histogram(binwidth = 100) +
  theme_minimal() +
  ggtitle("Daily Counts of DC Bike Share Rides") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") + ylab("") +
  scale_x_continuous(label=comma)


# Visualzing time series
ggplot(bike_trips, aes(x=Date, y=n)) + 
  geom_line() + 
  theme_minimal() + 
  ggtitle("Daily DC Bike Share Ridership") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") + ylab("") +
  scale_y_continuous(label=comma) +
  scale_x_date(breaks = "1 year",date_labels = "%Y")


# Checking on Removing outliers with 'tsoutliers'  ----
  # From both visualizations above, it's clear there are some outliers which can be removed.

  # Converting to ts() object
  bike_trips_TS <- as.ts(bike_trips)
  
  # tso(bike_trips_TS[,2])


### HOLD THIS STEP FOR NOW


# Prophet Forecast 1  ----


# Change column names for propet package
names(bike_trips) <- c("ds","y")

# Paritioning
train <- bike_trips %>% filter(ds<'2016-10-01')
valid <- bike_trips %>% filter(ds>='2016-10-01',ds<'2017-10-01')
test <- bike_trips %>% filter(ds>='2017-10-01')


## Prophet Forecast

  # Training 
  prophetFit1 <- prophet(train)
  
  # Cross-validation - https://facebook.github.io/prophet/docs/diagnostics.html
  df.cv <- cross_validation(prophetFit1, initial = 1825, period = 180, horizon = 365, units = 'days')
  performance_metrics(df.cv)
  plot_cross_validation_metric(df.cv, metric = 'mape')
  
  
  
  # Creating dataframe for forecast
  future <- make_future_dataframe(prophetFit1, periods = nrow(test))
  
  # Predicting
  prophetForecast1 <- predict(prophetFit1, future)
  
  # Visualize forecast
  plot(prophetFit1, prophetForecast1)
  
  # Visualizing ts components - trend, weekly and yearly seasonalities
  prophet_plot_components(prophetFit1, prophetForecast1)
    
  # Calculating Fit
  Fit <- test
  Fit$y_hat <- tail(prophetForecast1$yhat,nrow(test))
  Fit$resid <- Fit$y-Fit$y_hat
  Fit$resid_perc <- (Fit$resid/Fit$y)*100
  
  error_summary <-summary(Fit$resid_perc)
  
  
  # Visualize forecast
  ggplot(Fit,aes(resid_perc)) + geom_histogram(binwidth = 10)
  
  

  
  
# Prophet Forecast 2 : Log Transform  ----
  
  
  ## The log transform results in slightly lower MAE rate
  
  # Transform 
  log_train <- train
  log_train$y <- log(log_train$y)
  
  
  # Trainging
  m_log <- prophet(log_train)
  
  # Creating dataframe for log forecast
  future_log <- make_future_dataframe(m_log, periods = nrow(test))
  
  # Predicting
  forecast_log <- predict(m_log, future_log)

  # Visualize forecast
  plot(m_log, forecast_log)

  
  # Visualizing ts components - trend, weekly and yearly seasonalities
  prophet_plot_components(m_log, forecast_log)

  
  # Function for back transform of log
  log_back <- function(y){
    e <- exp(1)
    return(e^y)
  }
  
  
  # Calculating Fit 
  Log_Fit <- test
  Log_Fit$y_hat_log <- tail(forecast_log$yhat,nrow(Log_Fit))
  Log_Fit$y_hat <- log_back(Log_Fit$y_hat_log)

  Log_Fit$resid <- Log_Fit$y-Log_Fit$y_hat
  Log_Fit$resid_perc <- (Log_Fit$resid/Log_Fit$y)*100
  
  log_error_summary <-summary(Log_Fit$resid_perc)


  # Vizualizing Errors
  ggplot(Log_Fit,aes(resid_perc)) + geom_histogram(binwidth = 10)



  
# Prophet Forecast 3 : w/Tuning Parameters  ----
  
  
  # Search grid
  prophetGrid <- expand.grid(changepoint_prior_scale = c(0.05, 0.5, 0.001),
                             seasonality_prior_scale = c(100, 10, 1),
                             #holidays_prior_scale = c(100, 10, 1),
                             capacity = c(14000, 14500, 15000, 16000), # Setting maximum values # https://facebook.github.io/prophet/docs/saturating_forecasts.html
                             growth = 'logistic')
  
  # The Model
  results <- vector(mode = 'numeric', length = nrow(prophetGrid))
  
  # Search best parameters
  for (i in seq_len(nrow(prophetGrid))) {
    parameters <- prophetGrid[i, ]
    if (parameters$growth == 'logistic') {train$cap <- parameters$capacity}
    
    m <- prophet(train, growth = parameters$growth, 
                 #holidays = holidays,
                 seasonality.prior.scale = parameters$seasonality_prior_scale, 
                 changepoint.prior.scale = parameters$changepoint_prior_scale)
                #,holidays.prior.scale = parameters$holidays_prior_scale)
    
    future <- make_future_dataframe(m, periods = nrow(valid))
    if (parameters$growth == 'logistic') {future$cap <- parameters$capacity}
    
    # NOTE: There's a problem in function names with library(caret)
    forecast <- predict(m, future)
    
    forecast_tail <- tail(forecast,nrow(valid))
    
    #results[i] <- forecast::accuracy(forecast[forecast$ds %in% valid$ds, 'yhat'], valid$y)[ , 'MAE']
    
    results[i] <- forecast::accuracy(forecast_tail$yhat, valid$y)[ , 'MAE']
    
  }
  
  prophetGrid <- cbind(prophetGrid, results)
  best_params <- prophetGrid[prophetGrid$results == min(results), ]
  

  
  # Retrain using train and validation set
  retrain <- bind_rows(train, valid)
  retrain$cap <- best_params$capacity
  m <- prophet(retrain, growth = best_params$growth,
               #holidays = holidays,
               seasonality.prior.scale = best_params$seasonality_prior_scale,
               changepoint.prior.scale = best_params$changepoint_prior_scale)
               #,holidays.prior.scale = best_params$holidays_prior_scale)
  
  future <- make_future_dataframe(m, periods = 184)
  future$cap <- best_params$capacity
  
  forecast <- predict(m, future)

  
  # Final plot
  p <- ggplot()
  p <- p + geom_point(data = train, aes(x = ds, y = y), size = 0.5)
  p <- p + geom_line(data = forecast, aes(x = as.Date(ds), y = yhat), color = "#0072B2")
  p <- p + geom_ribbon(data = forecast, aes(x = as.Date(ds), ymin = yhat_lower, ymax = yhat_upper), fill = "#0072B2", alpha = 0.3)
  p <- p + geom_point(data = valid, aes(x = ds, y = y), size = 0.5, color = '#4daf4a')
  p <- p + geom_point(data = test, aes(x = ds, y = y), size = 0.5, color = 'red')
  p  
  
  