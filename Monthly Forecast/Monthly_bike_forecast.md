
### Monthly DC BikeShare Ridership

------------------------------------------------------------------------

The original data was gathered from the official DC Bike Share [site](https://s3.amazonaws.com/capitalbikeshare-data/index.html), spans bike trips logged from October, 2010 to August, 2018. For this markdown, the daily ridership data has been aggregated up to a monthly level.

![](Monthly_bike_forecast_files/figure-markdown_github/Plots-1.png)

#### Decomposition

A decomposition of the monthly data using a multiplicative seasonal component since the seasonal fluctuations tend to grow over time:

``` r
decompose(ts_month, type="multiplicative") %>% autoplot() + 
  xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of monthly bike rentals")
```

![](Monthly_bike_forecast_files/figure-markdown_github/unnamed-chunk-1-1.png)

#### Stationarity

A requirement of ARIMA modeling is **stationarity** of the series which is achieved by having *time invariant* mean, variance, and co-variance of the series. Stationarity is tested formally using the augmented Dickey-Fuller test.

To accomplish this, the first step is to remove the seasonal trend from the original series.

![](Monthly_bike_forecast_files/figure-markdown_github/Removing%20Seasonality-1.png)

However, the "de-seasoned" data fails the Adjusted Dickey-Fuller test, indicating the series is not yet stationary:

``` r
adf.test(deseasonal_cnt, alternative = "stationary") 
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  deseasonal_cnt
    ## Dickey-Fuller = -3.9739, Lag order = 4, p-value = 0.01378
    ## alternative hypothesis: stationary

------------------------------------------------------------------------

Examining the ACF and PACF plots indicate that differencing the series by 1 (\_d\_\_=1), could help:

![](Monthly_bike_forecast_files/figure-markdown_github/ACF%20and%20PACF%20plots-1.png)![](Monthly_bike_forecast_files/figure-markdown_github/ACF%20and%20PACF%20plots-2.png)

Differencing the data by one period appears to bring stationarity to the series as tested by the ADF test.

``` r
deseasoned_count_d1 = diff(deseasonal_cnt, differences = 1)

adf.test(deseasoned_count_d1, alternative = "stationary") 
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  deseasoned_count_d1
    ## Dickey-Fuller = -5.7992, Lag order = 4, p-value = 0.01
    ## alternative hypothesis: stationary

From the plot, it appears that the differenced, deseasoned data has a stationary mean, though the variance does not appear constant:

![](Monthly_bike_forecast_files/figure-markdown_github/Searching%20for%20Stationarity:%20Differencing%20Plot-1.png)

------------------------------------------------------------------------

Running ACF, PACF plots of the differeced data to see what values for *q*, *p* would be for an ARIMA model:

![](Monthly_bike_forecast_files/figure-markdown_github/Differenced%20ACF-1.png)

The ACF plot shows significant auto-correlations at lags 1,2,8,9,11 (*q*)

![](Monthly_bike_forecast_files/figure-markdown_github/Differenced%20PACF-1.png)

The PACF plot shows significant partial-correlations at 1,2, and beyond (*p*)

#### ARIMA

Using the findings from the ACF, PACF plots above, an ARIMA(2,1,9) model is chosen.

    ## 
    ## Call:
    ## arima(x = deseasoned_count_d1, order = c(2, 1, 9))
    ## 
    ## Coefficients:
    ##           ar1      ar2      ma1      ma2      ma3      ma4      ma5
    ##       -0.6738  -0.6415  -0.5796  -0.1744  -0.8391  -0.0723  -0.0498
    ## s.e.   0.1069   0.1014   0.1965   0.1638   0.1301   0.1766   0.1902
    ##          ma6     ma7     ma8      ma9
    ##       0.4855  0.5922  0.3399  -0.6488
    ## s.e.  0.1856  0.1361  0.1390   0.1653
    ## 
    ## sigma^2 estimated as 434834666:  log likelihood = -1067.37,  aic = 2158.74

Evaluating the diagnostic plots for the (2,1,9) residuals return a seemingly random residual plot and no significant autocorrelations. There does appear to be some

![](Monthly_bike_forecast_files/figure-markdown_github/Fit%20Evaluation-1.png)

*auto.arima()*

Comparing the ARIMA(2,1,9) to the results from auto.arima we can see the automated function does not account for the autocorrelation at q(9). Furthermore, the AIC is slightyl smaller in the ARIMA(2,1,9) model.

``` r
auto.arima(deseasoned_count_d1)
```

    ## Series: deseasoned_count_d1 
    ## ARIMA(2,0,2) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1     ma2      mean
    ##       1.5664  -0.7621  -1.8584  0.9260  2642.453
    ## s.e.  0.0843   0.0814   0.0881  0.0825   905.082
    ## 
    ## sigma^2 estimated as 669557486:  log likelihood=-1087.21
    ## AIC=2186.42   AICc=2187.39   BIC=2201.68

``` r
fit <- auto.arima(deseasoned_count_d1,seasonal=FALSE) 

tsdisplay(residuals(fit), lag.max=45, main='(2,0,2) Model Residuals')
```

![](Monthly_bike_forecast_files/figure-markdown_github/Auto-Arima-1.png)

#### Forecasting and Back-Testing

To fully evaluate the model's predictive power, we set aside the last 12 months of the *deseasonal\_cnt* object and compare our predictions to what was actually observed.

![](Monthly_bike_forecast_files/figure-markdown_github/Partition%20and%20Forecast-1.png)
