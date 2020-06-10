Forecasting Capital Bikeshare usage with ARIMA
==============================================

------------------------------------------------------------------------

Having used DC Capital BikeShare data
[previously](https://rsolter.github.io/r/forecasting/Monthly_Bike_Forecast_ETS/)
to forecast with exponential smoothing models, I wanted to do the same
with ARIMA modeling. ARIMA a complementary method for forecasting
univariate timeseries, but it also allows for explanatory variables in
the regression. In this case, I’ve added monthly average temperatures as
a regressor. The
[data](https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-month)
was downloaded from NOAA and represents measurements from the Regan
National Airport weather station.

------------------------------------------------------------------------

Tools for Stationarity
======================

A requirement of ARIMA modeling is stationarity of the series which is
achieved by having *time invariant* mean, variance, and co-variance of
the series. Any time series with a trend or seasonality is not
stationary.

One tool used to achieve stationarity is **differencing.** or computing
the difference between consecutive observations. As an example, we can
show this using stock data downloaded using the **quantmod** R package.
By differencing the data by just one observation, the trend in the stock
prices completely disappears:

    ## [1] "AAPL"

![](Monthly_bike_forecast_files/figure-markdown_github/differencing%20example-1.png)

However, was that enough to achieve stationarity? Formally, stationarity
can be assessed by using one of many unit tests, one of which is the
Kwiatkowski-Phillips-Schmidt-Shin (**KPSS**) test. The null hypothesis
for the KPSS test is that the data are stationary, and we look for
evidence that the null hypothesis is false. Consequently, small p-values
(e.g., less than 0.05), indicate the data is not stationary and suggest
differencing is required. Read more
[here](https://nwfsc-timeseries.github.io/atsa-labs/sec-boxjenkins-aug-dickey-fuller.html).

In the first test on the raw data, the null hypothesis of stationarity
is rejected. Run a second time on differenced data, the null hypothesis
is not rejected, indicating that our differencing worked!

    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  closingApple
    ## KPSS Level = 1.9588, Truncation lag parameter = 4, p-value = 0.01

    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  DclosingApple
    ## KPSS Level = 0.187, Truncation lag parameter = 4, p-value = 0.1

Standard variance is another requirement for stationarity. Handling
non-standard variance is done by transforming the time series, either
with logarithmic functions or the Box-Cox transformation. To show it in
action, I’ll create a fake dataset whose variance grows over time:

![](Monthly_bike_forecast_files/figure-markdown_github/heteroskedasticity-1.png)

And use BoxCox.lambda() to determine the correct lambda for transforming
the data to get a constant variance:

![](Monthly_bike_forecast_files/figure-markdown_github/Box-Cox-1.png)

**ARIMA Theory**

ARIMA is an acronym for Auto Regressive (AR) Integrated (I) Moving
Average (MA) and can be implemented in R using the automated function
`auto.arima` from the forecast package. ARIMA modeling is best
understood by breaking it into its components:

**Auto-regressive** models forecast using a linear combination of past
values of the variable. An AR model of order *p* can be written as:

*y*<sub>*t*</sub> = *c* + *θ**y*<sub>*t* − 1</sub> + *θ**y*<sub>*t* − 2</sub> + ... + *θ**y*<sub>*t* − *p*</sub> + *ϵ*<sub>*t*</sub>

Where *ϵ* is white noise and the model is denoted **AR(*p*)**, an
auto-regressive model of order p. Read more
[here](https://otexts.com/fpp2/AR.html)

**Moving Average** models use past values forecast errors as regressors
for forecasting:

*y*<sub>*t*</sub> = *c* + *ϵ*<sub>*t*</sub> + *θ*<sub>1</sub>*ϵ*<sub>*t* − 1</sub> + *θ*<sub>2</sub>*ϵ*<sub>*t* − 2</sub> + ... + *θ*<sub>*q*</sub>*ϵ*<sub>*t* − *q*</sub>
 Again, in this model, *ϵ* is white noise and the model is denoted
**MA(*q*)**, a moving-average model of order q. Read more
[here](https://otexts.com/fpp2/MA.html)

**ARIMA Formulation**

Combining the autoregression and moving average model, we obtain a
non-seasonal ARIMA model which is denoted as
**ARIMA**(*p,d,q*)**model**, where

-   p - order of the auto-regressive part
-   d - degree of first differencing involved
-   q - order of the moving average part

The **seasonal** version of ARIMA builds upon this to include 4
additional seasonal terms:

-   m - number of observations per year (12 for months, 4 for quarters,
    etc.)
-   P - order of the seasonal auto-regressive part
-   D - degree of first differencing involved for seasonal observations
-   Q - order of the seasonal moving average part

------------------------------------------------------------------------

#### Application to DC Bike Share

Now we’ll apply the ARIMA model to the monthly bike share data while
using the temperature data as a regressor.

From the initial plot of the bike data, we can tell the data has clear
seasonality with many fewer riders in the winter months. At the same
time, we see an growth in the number of total riders that has appeared
to slow in recent years. In the plot for temperature we see a
predicatble seasonality whose peaks and valleys remain almost equal in
size throughout the entire time frame:

![](Monthly_bike_forecast_files/figure-markdown_github/Train%20Plots-1.png)

The function **auto.arima()** automates the process of choosing the
parameters. [‘The auto.arima() function in R uses a variation of the
Hyndman-Khandakar algorithm, which combines unit root tests,
minimisation of the AICc and MLE to obtain an ARIMA
model’](https://otexts.com/fpp2/arima-r.html).

A seasonal ARIMA model produces a MAPE of 9.81% on the training set and
a similar MAPE on the test set (9.6%)

    ## Series: bike_train 
    ## ARIMA(0,1,4)(0,1,0)[12] 
    ## 
    ## Coefficients:
    ##           ma1      ma2     ma3      ma4
    ##       -0.7402  -0.1356  0.3388  -0.3313
    ## s.e.   0.1096   0.1548  0.1198   0.1373
    ## 
    ## sigma^2 estimated as 964359991:  log likelihood=-822.08
    ## AIC=1654.15   AICc=1655.09   BIC=1665.4
    ## 
    ## Training set error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE      MASE
    ## Training set -3688.112 27691.88 19733.45 -3.736181 9.817585 0.4543361
    ##                    ACF1
    ## Training set 0.01308924

![](Monthly_bike_forecast_files/figure-markdown_github/arima1-1.png)

Second attempt at auto.arima() with NOAA data used as a regressor,
results in slightly worse performance. The training MAPE is 11.9% and
the test MAPE is 10.6%. Why might this be? It could be that the
temperature isn’t actually a good predictor of ridership, at least at a
monthly level. Referring to the charts above, it’s clear that the
temperature data reflects the seasonality in the data, but that’s also
built into our auto.arima() function with the seasonal=“TRUE” argument
and m=12 parameter, so perhaps avg. monthly temperature is unecessary
for building our model.

    ## Series: bike_train 
    ## Regression with ARIMA(0,1,2)(1,0,0)[12] errors 
    ## 
    ## Coefficients:
    ##           ma1      ma2    sar1     drift       xreg
    ##       -0.3702  -0.4345  0.5719  3247.913  3798.2219
    ## s.e.   0.1142   0.1118  0.1043  1029.499   365.4886
    ## 
    ## sigma^2 estimated as 535667157:  log likelihood=-940.59
    ## AIC=1893.17   AICc=1894.29   BIC=1907.61
    ## 
    ## Training set error measures:
    ##                     ME     RMSE     MAE      MPE     MAPE      MASE        ACF1
    ## Training set -291.7736 22292.25 18183.7 1.379566 11.88621 0.4186552 -0.02153787

![](Monthly_bike_forecast_files/figure-markdown_github/arima2-1.png)
