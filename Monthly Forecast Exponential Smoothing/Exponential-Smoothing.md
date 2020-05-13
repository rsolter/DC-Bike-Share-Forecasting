One of the fundamental methods for forecasting univariate series is
[exponential
smoothing](https://en.wikipedia.org/wiki/Exponential_smoothing). The
basic idea behind the method is that forecasts are produced using a
weighted average of past observations. Furthermore, the weights applied
to the past observations decay exponentially as the observations get
older, with more recent observations receiving greater weight. This post
will examine applying this method to predict monthly ridership on the
Capital BikeShare program.

**Partition**

To test the accuracy of the predictions, we will split the data stored
in `ts_month` using the `TSstudio` package into `test` and `train`
objects. The test period is set to be the final 12 observations, or one
year.

``` r
ts_month
```

    ##         Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
    ## 2010                                                                 36613
    ## 2011  38116  48089  63855  94632 135537 143266 141105 136447 127158 123292
    ## 2012  96590 102979 164654 173954 195637 202600 203607 214503 218572 198840
    ## 2013 126142 111190 158838 238604 252922 257118 270919 292083 285187 257652
    ## 2014 114006 124031 167568 272490 310151 320523 331157 324218 327597 301244
    ## 2015 128592 109766 193107 318127 366930 314016 364015 364313 328038 290351
    ## 2016 123252 145654 283493 285516 288720 368096 366435 357306 344246 343243
    ## 2017 174804 226303 245403 365990 339677 398751 397680 402534 391371 384833
    ## 2018 168590 182378 238998 328907 374115 392338 404761 403866              
    ##         Nov    Dec
    ## 2010  48114  28765
    ## 2011 101949  87087
    ## 2012 152664 123713
    ## 2013 194621 138090
    ## 2014 199298 153221
    ## 2015 228296 187357
    ## 2016 259106 168719
    ## 2017 252534 177897
    ## 2018

``` r
ts_month_partition <- TSstudio::ts_split(ts_month,sample.out = 12)
train <- ts_month_partition$train
test <- ts_month_partition$test
```

**Holt-Winters Modeling**

![](Exponential-Smoothing_files/figure-markdown_github/viz-1.png)

Given both the clear seasonality and growing trend in the series, we’ll
use the Holt-Winters method (sometimes known as the Triple Exponential
Smoothing) which will capture both factors. Specifically, the
Holt-Winters uses three separate smoothing factors to compose the
forecast; one for the level (alpha), another for trend (beta), and a
final for the seasonal component (gamma).

Furthermore, given that the size of the seasonal swings in ridership are
not constant and have grown from \~100k in 2012 to over +200k in 2017
and 2018, it makes sense to try a multiplicative method as well as
additive. Read more about the methodology
[here](https://otexts.com/fpp2/holt-winters.html).

``` r
add_fit <- hw(train,seasonal="additive",h = 12)
mult_fit <- hw(train,seasonal="multiplicative",h = 12)
autoplot(ts_month) +
  autolayer(add_fit, series="HW additive forecasts", PI=FALSE) +
  autolayer(mult_fit, series="HW multiplicative forecasts",
    PI=FALSE) +
  scale_y_continuous(label=comma) + theme_minimal() +
  xlab(" ") +
  ylab("") +
  ggtitle("Holt-Winters Forecasts for Monthly Bike Rentals") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(plot.title = element_text(hjust = 0.5))
```

![](Exponential-Smoothing_files/figure-markdown_github/holt-winters-1.png)

It certainly appears that the multiplicative model does a better job
than the additive one in estimating the ridership, at least until the
final few months of the predictive window at which point the two
estimates are quite similar.

We can confirm this by looking at the errors on a monthly basis. Over
the 12 months, the mean absolute percent error for the additive model is
23.9% while the multiplicative method is closer at 10.6%.

|  Actual|  Add.Forecast|  Add.Error.Abs|  Add.Error.Perc|  Mult.Forecast|  Mult.Error.Abs|  Mult.Error.Perc|
|-------:|-------------:|--------------:|---------------:|--------------:|---------------:|----------------:|
|  391371|      394964.9|       3593.933|            0.92|       383812.5|       -7558.548|            -1.93|
|  384833|      376862.2|      -7970.817|           -2.07|       355866.2|      -28966.766|            -7.53|
|  252534|      314872.2|      62338.235|           24.69|       274616.9|       22082.928|             8.74|
|  177897|      269802.0|      91905.036|           51.66|       206849.5|       28952.470|            16.27|
|  168590|      254859.0|      86268.970|           51.17|       185347.2|       16757.204|             9.94|
|  182378|      265063.3|      82685.256|           45.34|       205223.4|       22845.435|            12.53|
|  238998|      336311.3|      97313.296|           40.72|       286876.9|       47878.856|            20.03|
|  328907|      397253.7|      68346.681|           20.78|       383590.8|       54683.760|            16.63|
|  374115|      425827.6|      51712.589|           13.82|       412234.7|       38119.712|            10.19|
|  392338|      437097.9|      44759.941|           11.41|       422610.3|       30272.296|             7.72|
|  404761|      451522.4|      46761.354|           11.55|       435415.4|       30654.421|             7.57|
|  403866|      455687.8|      51821.826|           12.83|       439027.5|       35161.529|             8.71|

**Utilizing ets()**

A more customizable forecasting method than using the `hw()` model is
the `ets()` function in the `forecast` package which allows greate
specification of the mode.

The function’s **model** parameter can be specified with a three
character string. The first letter denotes the error type, the second
letter denotes the trend type, and the third letter denotes the season
type. The options you can specify for each component are below:

-   error: additive (“A”), multiplicative (“M”), unknown (“Z”)
-   trend: none (“N”), additive (“A”), multiplicative (“M”), unknown
    (“Z”)
-   seasonality: none (“N”), additive (“A”), multiplicative (“M”),
    unknown (“Z”)

For example, setting `model='AAA'` would produce a model with additive
error, additive trend, and additive seasonality. By default, the
parameter is set to “ZZZ” which passes unknown values to each component
and allows the algorithm to select the ‘optimal’ model by minimizing
RMSE, AIC, and BIC on the training data set. See reference
[here](https://www.rdocumentation.org/packages/forecast/versions/8.12/topics/ets).

Running the model with the default setting returns ETS(M,Ad,M):

    ## ETS(M,Ad,M) 
    ## 
    ## Call:
    ##  ets(y = train) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 1e-04 
    ##     beta  = 1e-04 
    ##     gamma = 1e-04 
    ##     phi   = 0.977 
    ## 
    ##   Initial states:
    ##     l = 47634.0206 
    ##     b = 6990.7145 
    ##     s = 1.227 1.2774 1.2962 1.2853 1.239 1.1361
    ##            0.8626 0.6097 0.5644 0.6105 0.8225 1.0693
    ## 
    ##   sigma:  0.1273
    ## 
    ##      AIC     AICc      BIC 
    ## 2059.187 2069.874 2102.726

Forecasting a year forward with this model provide a much better
prediction, returning an average absolute error of just 4.6%

![](Exponential-Smoothing_files/figure-markdown_github/ets%20evaluation-1.png)

|  Actual|  ETS.Forecast|  ETS.Error.Abs.|  ETS.Error.Perc|
|-------:|-------------:|---------------:|---------------:|
|  391371|      370883.7|      -20487.327|           -5.23|
|  384833|      324247.9|      -60585.135|          -15.74|
|  252534|      250232.9|       -2301.063|           -0.91|
|  177897|      186289.9|        8392.908|            4.72|
|  168590|      172754.4|        4164.381|            2.47|
|  182378|      187163.9|        4785.866|            2.62|
|  238998|      265576.9|       26578.889|           11.12|
|  328907|      350742.4|       21835.353|            6.64|
|  374115|      383581.3|        9466.327|            2.53|
|  392338|      398949.3|        6611.274|            1.69|
|  404761|      403389.4|       -1371.609|           -0.34|
|  403866|      398558.4|       -5307.588|           -1.31|
