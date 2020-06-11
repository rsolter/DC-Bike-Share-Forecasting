### Predicting Daily DC Bike Share Ridership with **prophet**

The
[prophet](https://facebook.github.io/prophet/docs/quick_start.html#r-api)
package was created by Facebook in 2017 and provides a procedure for
forecasting time series data. It’s a great package that’s meant to help
business and data analysts build reasonable forecasts with messy data
without much manual effort. Prophet can handle, yearly, weekly, and
daily seasonality, regressors, missing data, and can account for
holidays or other dates that have a profound effect on the time series.

I’ve used prophet for goal-setting in a previous role, but in this post
will apply it to the DC Capital BikeShare daily ridership. This post is
an extension of the [Forecasting Monthly Usage of Capital
Bikeshare](https://rsolter.github.io/r/forecasting/Monthly_Bike_Forecast/);
its dataset is from the same source, just not aggregated to the monthly
level.

To try and improve the accuracy of the ridership forecasts, daily
percipitation data from NOAA will be included. The percipitation data
was collected from the Reagan National Airport weather station and
measures percipitation in tenths of a millimeter.

------------------------------------------------------------------------

### Visual Exploration of Daily Ridership

A few takeaways:

-   The average number of trips taken in a given day is just under
    7,500.

-   Unsurprisingly, the series exhibits clear seasonality, with more
    trips taking place during the summer months and fewer during the
    winter. From this plot, it’s unclear if a weekly seasonality exists.

-   There are also a number of outliers apparent in the data. Most of
    these outliers are registering below the curve, and could possibly
    be due to bad weather on those particular days. There also appear to
    be a few positive outliers in the first quarter of the year and
    could relate to a seasonal event such as the arrival of the cherry
    blossoms.

-   Given that the size of the seasonal fluctuations grow over time, a
    multiplicative model may be best for modeling this series.

-   Similarly, over time the trend appears to grow less and less,
    indicating a model incorporating a logistic trend may be most
    appropriate. Ostensibly, this growth is down to the
    [expansion](https://en.wikipedia.org/wiki/Capital_Bikeshare#Expansion)
    in the number of bike stations and bikes over the last \~10 years.

<!-- -->

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1    4378    7296    7499   10804   19113

![](Daily_bike_forecast_files/figure-markdown_github/exploratory%20viz-1.png)![](Daily_bike_forecast_files/figure-markdown_github/exploratory%20viz-2.png)

------------------------------------------------------------------------

#### Removing Outliers

Since there appear to be multiple outliers in the dataset and the
prophet package can handle missing data points, we’ll remove some
outliers before proceeding to modeling the data.

*Identifying and processing outliers*

One way to identify outliers is to use [moving-median
decomposition](https://anomaly.io/anomaly-detection-moving-median-decomposition/).
In the case below, a two-week-long running median trend is subtracted
from the original series. Next, observations which fall outside of 4
standard deviations of that running median trend are isolated and
removed from the series for future modeling.

![](Daily_bike_forecast_files/figure-markdown_github/Handing%20Outliers%20in%20overall%20dataset-1.png)![](Daily_bike_forecast_files/figure-markdown_github/Handing%20Outliers%20in%20overall%20dataset-2.png)

------------------------------------------------------------------------

### Modeling with Prophet

To test the model, the daily observations of bike rides and daily
percipitation are split into training sets (2010-05-15 to 2016-08-31)
and test splits (2016-09-01 to 2018-09-17). Effectively, 25% of the data
is being set aside for testing.

**First Model** The first model is set to account for daily, weekly, and
yearly seasonality.

Ultimately, the MAPE is close to 31%. From both the plot of the forecast
as well as the residual distribution, we can see that the current model
generally underestimates daily ridership.

``` r
# https://facebook.github.io/prophet/docs/quick_start.html#r-api

## Prophet Forecast 1

  # Training 
  prophetFit1 <- prophet(train,
                         yearly.seasonality = T,
                         weekly.seasonality = T,
                         daily.seasonality = F)
  
  # Creating dataframe for future forecast, will only have date values
  future <- make_future_dataframe(prophetFit1, periods = nrow(test),freq = 'day')

  # Predicting - resulting 'prophetForecast1' dataframe contains columns for predictions, trend data, and uncertainty intervals
  prophetForecast1 <- predict(prophetFit1, future)
  
  # Visualizing ts components - trend, weekly and yearly seasonalities
  prophet_plot_components(prophetFit1, prophetForecast1)
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%201-1.png)

``` r
  # Visualize forecast
  plot(prophetFit1, prophetForecast1,ylabel = "Daily Rides",xlabel = "Date")
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%201-2.png)

``` r
  # Dynamic Plot of forecast
  #dyplot.prophet(prophetFit1, prophetForecast1)  

   
  
## Prophet Foreceast 1 - Evaluation
  
  
  # Calculating Fit
  Fit <- test
  Fit$y_hat <- tail(prophetForecast1$yhat,nrow(test))
  Fit$resid <- Fit$y-Fit$y_hat
  Fit$resid_perc <- (Fit$resid/Fit$y)*100
  Fit$abs_resid_perc <- abs(Fit$resid_perc)
  
  error_summary <-summary(Fit$abs_resid_perc)
  
  
  # Results
  error_summary # A summary of the errors as percentages
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   0.0069   8.0666  15.7430  31.2611  25.1704 621.1372

``` r
  ggplot(Fit,aes(resid_perc)) + geom_histogram(binwidth = 10) + ggtitle("Prophet Model 1 : Distribution of Residuals (%)") +
    xlab("Residual Percentage") + ylab("") + theme_minimal()
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%201-3.png)

------------------------------------------------------------------------

#### Second Prophet Model - Logistic Growth + Carrying Capacity

Given that the overall trend appears to be leveling off, we’ll try a
second modeling approach with a logistic growth specified and a carrying
capacity. The *carry capacity* refers to the maxium possible value for
the series. In this case, we’ll set it at 19,113 which was the maximum
value of rides in the original dataset.

Unforuntately, taking both of these steps doesn’t seem to have a
material difference on the MAPE (32%) or the overall

``` r
  # Training 

  train$cap <- 19113  

  prophetFit2 <- prophet(train,
                         yearly.seasonality = T,
                         weekly.seasonality = T,
                         daily.seasonality = F,
                         growth = "logistic")
  
  # Creating dataframe for future forecast
  future <- make_future_dataframe(prophetFit2, periods = nrow(test),freq = 'day')
  future$cap <- 19113

  
  # Predicting
  prophetForecast2 <- predict(prophetFit2, future)
  
  # The resulting prophetForecast1 dataframe contains columns for predictions, trend data, and uncertainty intervals 

  # Visualizing ts components - trend, weekly and yearly seasonalities
  prophet_plot_components(prophetFit2, prophetForecast2)
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%202-1.png)

``` r
  # Visualize forecast
  plot(prophetFit2, prophetForecast2,ylabel = "Daily Rides",xlabel = "Date")
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%202-2.png)

``` r
  # Dynamic Plot of forecast
  #dyplot.prophet(prophetFit1, prophetForecast1)  
    
  
# Evaluation of the second prediction:

  # Calculating Fit
  Fit <- test
  Fit$y_hat <- tail(prophetForecast2$yhat,nrow(test))
  Fit$resid <- Fit$y-Fit$y_hat
  Fit$resid_perc <- (Fit$resid/Fit$y)*100
  Fit$abs_resid_perc <- abs(Fit$resid_perc)
  
  error_summary <-summary(Fit$abs_resid_perc)
  
  
  # Results
  error_summary # A summary of the errors as percentages
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   0.0174   7.5454  14.9604  32.1028  26.0863 648.6879

``` r
  ggplot(Fit,aes(resid_perc)) + geom_histogram(binwidth = 10) + ggtitle("Prophet Model 2 : Distribution of Residuals (%)") +
    xlab("Residual Percentage") + ylab("") + theme_minimal()
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%202-3.png)

#### Third Prophet Model - Logistic Transformation

Another step we can take is transforming the data with logarithmic
transformation. This is done when the time series shows
heteroskedasticity, or a non-constant variance. This is evident in the
way the size of the seasonal swings in ridership grow over time.

Transforming the data back to compare our predictions to the test set
reports a MAPE of 26%, a clear improvement!

#### Fourth Prophet Model - Regression with Percipitation

In this attempt, we will add the daily percipitation data in as a
regressor. Technically, we would want to actually use predictions of
daily percipitation if we wanted to do an actual forecast, but this is
just an illustrative example.

    ## $growth
    ## [1] "linear"
    ## 
    ## $changepoints
    ## NULL
    ## 
    ## $n.changepoints
    ## [1] 25
    ## 
    ## $changepoint.range
    ## [1] 0.8
    ## 
    ## $yearly.seasonality
    ## [1] "auto"
    ## 
    ## $weekly.seasonality
    ## [1] "auto"
    ## 
    ## $daily.seasonality
    ## [1] "auto"
    ## 
    ## $holidays
    ## NULL
    ## 
    ## $seasonality.mode
    ## [1] "additive"
    ## 
    ## $seasonality.prior.scale
    ## [1] 10
    ## 
    ## $changepoint.prior.scale
    ## [1] 0.05
    ## 
    ## $holidays.prior.scale
    ## [1] 10
    ## 
    ## $mcmc.samples
    ## [1] 0
    ## 
    ## $interval.width
    ## [1] 0.8
    ## 
    ## $uncertainty.samples
    ## [1] 1000
    ## 
    ## $specified.changepoints
    ## [1] FALSE
    ## 
    ## $start
    ## NULL
    ## 
    ## $y.scale
    ## NULL
    ## 
    ## $logistic.floor
    ## [1] FALSE
    ## 
    ## $t.scale
    ## NULL
    ## 
    ## $changepoints.t
    ## NULL
    ## 
    ## $seasonalities
    ## list()
    ## 
    ## $extra_regressors
    ## $extra_regressors$percip
    ## $extra_regressors$percip$prior.scale
    ## [1] 10
    ## 
    ## $extra_regressors$percip$standardize
    ## [1] "auto"
    ## 
    ## $extra_regressors$percip$mu
    ## [1] 0
    ## 
    ## $extra_regressors$percip$std
    ## [1] 1
    ## 
    ## $extra_regressors$percip$mode
    ## [1] "additive"
    ## 
    ## 
    ## 
    ## $country_holidays
    ## NULL
    ## 
    ## $stan.fit
    ## NULL
    ## 
    ## $params
    ## list()
    ## 
    ## $history
    ## NULL
    ## 
    ## $history.dates
    ## NULL
    ## 
    ## $train.holiday.names
    ## NULL
    ## 
    ## $train.component.cols
    ## NULL
    ## 
    ## $component.modes
    ## NULL
    ## 
    ## $fit.kwargs
    ## list()
    ## 
    ## attr(,"class")
    ## [1] "prophet" "list"

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%204%20Regression-1.png)

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   0.0069   8.0666  15.7430  31.2611  25.1704 621.1372

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%204%20Regression-2.png)

#### Fifth Prophet Model - Prediction with Tuning Parameters

------------------------------------------------------------------------

#### Next Steps

-   Make use of prophet’s holiday feature which allows for certain
    observations to be marked as influential in establishing change
    points in the trend.

-   Tuning the parameters related to the flexibility that we want to
    give the model to fit change points, seasonality and holidays.
