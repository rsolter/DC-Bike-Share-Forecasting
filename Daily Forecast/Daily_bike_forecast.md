### Predicting Daily DC Bike Share Ridership with **prophet**

This second post is an extension of the [Forecasting Monthly Usage of
Capital
Bikeshare](https://rsolter.github.io/r/forecasting/Monthly_Bike_Forecast/)
post in which I forecast daily usage of the Bikeshare program using
using Facebook’s forecasting package
[prophet](https://facebook.github.io/prophet/docs/quick_start.html#r-api).
The dataset is the same, just not aggregated to the monthly level.

------------------------------------------------------------------------

#### Visual Exploration

A few takeaways:

-   Unsurprisingly, the series exhibits clear seasonality, with more
    trips taking place during the summer months and fewer during the
    winter. From this plot, it’s unclear if a weekly seasonality exists.

-   There are also a number of outliers apparent in the data. Most of
    these outliers are registering below the curve, and are less than
    expected, possibly due to bad weather on those particular days.
    There also appear to be a few positive outliers in the first quarter
    of the year and could relate to a seasonal event such as the arrival
    of the cherry blossoms.

-   Given that the size of the seasonal fluctuations grow over time, a
    multiplicative model may be best for modeling this series.

-   Similarly, over time the trend appears to grow less and less,
    indicating a model incorporating a logistic trend may be most
    appropriate. Ostensibly, this growth is down to the
    [expansion](https://en.wikipedia.org/wiki/Capital_Bikeshare#Expansion)
    in the number of bike stations and bikes over the last \~10 years.

![](Daily_bike_forecast_files/figure-markdown_github/viz-1.png)![](Daily_bike_forecast_files/figure-markdown_github/viz-2.png)

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

![](Daily_bike_forecast_files/figure-markdown_github/Handing%20Outliers%20in%20Test%20Set-1.png)![](Daily_bike_forecast_files/figure-markdown_github/Handing%20Outliers%20in%20Test%20Set-2.png)

------------------------------------------------------------------------

### First Prophet model

Due to the complexity inherent in daily forecasting, the modeling will
be done with Facebook’s semi-automated
[prophet](https://facebook.github.io/prophet/docs/quick_start.html#r-api)
package which can account for multiple seasonalities.

#### First Prophet Prediction

``` r
# https://facebook.github.io/prophet/docs/quick_start.html#r-api

## Prophet Forecast 1

  # Training 
  prophetFit1 <- prophet(train,
                         yearly.seasonality = T,
                         weekly.seasonality = T,
                         daily.seasonality = F)
  
  # Creating dataframe for future forecast
  future <- make_future_dataframe(prophetFit1, periods = nrow(test))

  # Predicting
  prophetForecast1 <- predict(prophetFit1, future)
  
  # The resulting prophetForecast1 dataframe contains columns for predictions, trend data, and uncertainty intervals 

  # Visualizing ts components - trend, weekly and yearly seasonalities
  prophet_plot_components(prophetFit1, prophetForecast1)
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%201-1.png)

``` r
  # Visualize forecast
  plot(prophetFit1, prophetForecast1)
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%201-2.png)

Evaluation of the first prediction:

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -621.76  -16.56    4.77  -15.00   15.35   47.22

![](Daily_bike_forecast_files/figure-markdown_github/unnamed-chunk-1-1.png)

------------------------------------------------------------------------

#### Second Prophet Model - Logistic Growth + Carrying Capacity

Given that the overall trend appears to be leveling off, we’ll try a
second modeling approach with a logistic growth specified and a carrying
capacity.

A *carry capacity* refers to the maxium possible value for the series.
In this case, we’ll set it at 17500

``` r
# https://facebook.github.io/prophet/docs/quick_start.html#r-api

## Prophet Forecast 1

  # Training 

  train$cap <- 17500  

  prophetFit1 <- prophet(train,
                         yearly.seasonality = T,
                         weekly.seasonality = T,
                         daily.seasonality = F,
                         growth = "logistic")
  
  # Creating dataframe for future forecast
  future <- make_future_dataframe(prophetFit1, periods = nrow(test))
  future$cap <- 17500

  
  # Predicting
  prophetForecast1 <- predict(prophetFit1, future)
  
  # The resulting prophetForecast1 dataframe contains columns for predictions, trend data, and uncertainty intervals 

  # Visualizing ts components - trend, weekly and yearly seasonalities
  prophet_plot_components(prophetFit1, prophetForecast1)
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%201a-1.png)

``` r
  # Visualize forecast
  plot(prophetFit1, prophetForecast1)
```

![](Daily_bike_forecast_files/figure-markdown_github/Prophet%20Forecast%201a-2.png)

``` r
  # Dynamic Plot of forecast
  #dyplot.prophet(prophetFit1, prophetForecast1)  
```

Evaluation of the second prediction:

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -645.538  -19.964    2.513  -17.790   13.610   45.788

![](Daily_bike_forecast_files/figure-markdown_github/unnamed-chunk-2-1.png)

------------------------------------------------------------------------

#### Next Steps

-   Make use of prophet’s holiday feature which allows for certain
    observations to be marked as influential in establishing change
    points in the trend.

-   Tuning the parameters related to the flexibility that we want to
    give the model to fit change points, seasonality and holidays.
