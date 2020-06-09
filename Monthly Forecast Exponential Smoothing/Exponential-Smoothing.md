Exponential smoothing is one of the fundamental methods for forecasting
univariate series. The basic idea behind the method is that forecasts
are produced using a weighted average of past observations. This post
will examine applying exponential smoothing to forecast monthly
ridership on the Capital BikeShare program.

For a simple time series, represented by {*x*<sub>*t*</sub>}, beginning
at *t* = 0, and the forecast of the next value in our sequence
represented as *x̂*<sub>*t* + 1</sub>, the simplest form of exponential
smoothing takes the form:

*x̂*<sub>*t* + 1</sub> = *α**x*<sub>*t*</sub> + *α*(1 − *α*)*x*<sub>*t* − 1</sub> + *α*(1 − *α*)<sup>2</sup>*x*<sub>*t* − 2</sub>..

In the equation above, the rate at which the weights decrease is
determined by the *α*, or the **smoothing factor** which is bound by
0 &lt; *α* &lt; 1. If *α* is closer to 0, more weight is given to
observations from the more distant past, while a value of *α* that is
closer to 1 will give more more weight to recent observations.

This idea can be expanded to different components of a time series, with
each component having its own smoothing factors. The standard way of
breaking apart the series is into three components: the level
*l*<sub>*t*</sub>, trend *b*<sub>*t*</sub>, and seasonal
*s*<sub>*t*</sub> components. This model is known as the **Holt-Winter’s
multiplicative method** and each smoothing factor is estimated on the
basis of minimizing the sum of the square residuals (SSE):

**Overall model** with *h* denoting the number of periods forecast into
the future (horizon), *m* denoting the frequency of the seasonality
(m=12 for monthly data), and k representing the integer part of
*(h-1)/m* which ensures that the estimate of the seasona indices used
for forecasting come from the final year of the sample.

*ŷ*<sub>*t* + *h*\|*t*</sub> = (*l*<sub>*t*</sub> + *h**b*<sub>*t*</sub>)*s*<sub>*t* + *h* − *m*(*k* + 1)</sub>

**Level** component, with *a**l**p**h**a* as the smoothing parameter
bound between 0 and 1.

$$ l\_{t} = \\alpha \\frac{y\_{t}}{s\_{t-m}}+(1-\\alpha)(l\_{t-1}+b\_{t-1}) $$

**Trend** component, with *β* as the smoothing parameter bound between 0
and 1.

*b*<sub>*t*</sub> = *β*(*l*<sub>*t*</sub> − *l*<sub>*t* − 1</sub>) + (1 − *β*)*b*<sub>*t* − 1</sub>

**Season** component, with *γ* as the smoothing parameter bound between
0 and 1.

$$ s\_{t} = \\gamma \\frac{y\_{t}}{l\_{t-1}+b\_{t-1}}+(1-\\gamma)s\_{t-m} $$

Read more about the Holt-Winters methodology
[here](https://otexts.com/fpp2/holt-winters.html).

**Applying Holt-Winters to BikeShare data**

As can be seen below, the bike data demonstrates clear seasonality and a
growing trend in overall ridership, so our exponential smoothing model
will need to account for both. Furthermore, the size of the seasonal
swings in ridership have grown over time, meaning our method will need
to account for that as well. Note that the chart below does not include
the final 12 observations in the dataset which have been set aside for
testing model accuracy.

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

![](Exponential-Smoothing_files/figure-markdown_github/viz-1.png)

Given that the size of the seasonal fluctuations are not constant over
time, the data is likely better fit with a multiplicative method,
however an additive model will be run for comparison:

``` r
add_fit <- hw(train,seasonal="additive",h = 12)
mult_fit <- hw(train,seasonal="multiplicative",h = 12)
autoplot(ts_month) +
  autolayer(add_fit, series="HW additive forecasts", PI=FALSE) +
  autolayer(mult_fit, series="HW multiplicative forecasts",
    PI=FALSE) +
  scale_y_continuous(label=comma) + theme_minimal() +
  xlab(" ") +
  ylab("Monthly Bike Rentals") +
  ggtitle("Holt-Winters Additive and Multiplicative Methods") +
  guides(colour=guide_legend(title="Forecast")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "bottom")
```

![](Exponential-Smoothing_files/figure-markdown_github/holt-winters-1.png)

The smoothing parameters and overall fit statistics are reported out in
the model portion of the forecast:

    ## Holt-Winters' additive method 
    ## 
    ## Call:
    ##  hw(y = train, h = 12, seasonal = "additive") 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.656 
    ##     beta  = 1e-04 
    ##     gamma = 1e-04 
    ## 
    ##   Initial states:
    ##     l = 76944.0423 
    ##     b = 4429.7097 
    ##     s = 54228.15 66405.15 66653.3 56638 49788.1 25620.48
    ##            -30898.26 -97742.35 -103535.8 -84179.53 -34696.54 31719.3
    ## 
    ##   sigma:  32605.17
    ## 
    ##      AIC     AICc      BIC 
    ## 2108.099 2117.514 2149.219

    ## Holt-Winters' multiplicative method 
    ## 
    ## Call:
    ##  hw(y = train, h = 12, seasonal = "multiplicative") 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.1802 
    ##     beta  = 0.0083 
    ##     gamma = 1e-04 
    ## 
    ##   Initial states:
    ##     l = 48742.7635 
    ##     b = 6048.0226 
    ##     s = 1.2036 1.2746 1.2727 1.2437 1.2215 1.1445
    ##            0.862 0.621 0.5648 0.6348 0.8488 1.108
    ## 
    ##   sigma:  0.1343
    ## 
    ##      AIC     AICc      BIC 
    ## 2071.091 2080.506 2112.211

It certainly appears that the multiplicative model does a better job
than the additive one in estimating the ridership, at least until the
final few months of the predictive window at which point the two
estimates are quite similar.

We can confirm this by looking at the errors on a monthly basis. Over
the 12 months, the mean absolute percent error for the additive model is
18.5% while the multiplicative method is closer at 10.1%.

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

Since the multiplicative forecast appeared to over-estimate ridership in
the second half of the prediction period, we can try and improve the
forecast by adding a damped trend. The damped trend introduced another
parameter to the trend equation that will eventually turn the trend to a
flat line sometime in the future. This is a popular method and often
improves the performance of the model.

Unfortunately, the addition of the damped method doesn’t appear to
improve our model’s performance. Although the forecast appears to be
better in the second half of the year, the damped method over-estimates
ridership in the first half of the year and returns an overall mape of
17.1%

![](Exponential-Smoothing_files/figure-markdown_github/unnamed-chunk-1-1.png)

    ## 
    ## Forecast method: Damped Holt-Winters' additive method
    ## 
    ## Model Information:
    ## Damped Holt-Winters' additive method 
    ## 
    ## Call:
    ##  hw(y = train, h = 12, seasonal = "additive", damped = TRUE) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.6634 
    ##     beta  = 1e-04 
    ##     gamma = 2e-04 
    ##     phi   = 0.98 
    ## 
    ##   Initial states:
    ##     l = 66591.7264 
    ##     b = 6286.9906 
    ##     s = 54227.86 66405.14 66653.16 56637.41 49788.04 25701.69
    ##            -30898.23 -92565.77 -100297.8 -84179.48 -34696.41 23224.4
    ## 
    ##   sigma:  32102.06
    ## 
    ##      AIC     AICc      BIC 
    ## 2106.269 2116.957 2149.809 
    ## 
    ## Error measures:
    ##                    ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
    ## Training set 322.6186 28626.34 22412.05 0.1826105 17.16446 0.5160072 0.05871017
    ## 
    ## Forecasts:
    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Sep 2017       390073.5 348933.1 431214.0 327154.6 452992.4
    ## Oct 2017       360201.8 310829.7 409574.0 284693.7 435710.0
    ## Nov 2017       303394.8 246977.4 359812.2 217111.9 389677.8
    ## Dec 2017       255002.9 192325.3 317680.5 159145.8 350860.0
    ## Jan 2018       239947.4 171578.8 308316.1 135386.6 344508.3
    ## Feb 2018       248720.2 175097.5 322342.8 136124.0 361316.3
    ## Mar 2018       311403.3 232875.9 389930.6 191306.1 431500.5
    ## Apr 2018       369022.8 285878.3 452167.4 241864.2 496181.5
    ## May 2018       394077.6 306557.9 481597.3 260227.7 527927.5
    ## Jun 2018       401904.0 310216.4 493591.5 261679.9 542128.0
    ## Jul 2018       412855.6 317180.5 508530.7 266533.2 559178.0
    ## Aug 2018       413533.5 314029.5 513037.5 261355.3 565711.7

    ## 
    ## Forecast method: Holt-Winters' multiplicative method
    ## 
    ## Model Information:
    ## Holt-Winters' multiplicative method 
    ## 
    ## Call:
    ##  hw(y = train, h = 12, seasonal = "multiplicative") 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.1802 
    ##     beta  = 0.0083 
    ##     gamma = 1e-04 
    ## 
    ##   Initial states:
    ##     l = 48742.7635 
    ##     b = 6048.0226 
    ##     s = 1.2036 1.2746 1.2727 1.2437 1.2215 1.1445
    ##            0.862 0.621 0.5648 0.6348 0.8488 1.108
    ## 
    ##   sigma:  0.1343
    ## 
    ##      AIC     AICc      BIC 
    ## 2071.091 2080.506 2112.211 
    ## 
    ## Error measures:
    ##                     ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
    ## Training set -4035.313 21647.46 17298.83 -4.192721 10.10881 0.3982823 0.1216346
    ## 
    ## Forecasts:
    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Sep 2017       383812.5 317773.1 449851.8 282813.9 484811.0
    ## Oct 2017       355866.2 293554.3 418178.1 260568.4 451164.1
    ## Nov 2017       274616.9 225651.0 323582.8 199730.1 349503.8
    ## Dec 2017       206849.5 169269.3 244429.6 149375.6 264323.3
    ## Jan 2018       185347.2 151018.6 219675.8 132846.2 237848.2
    ## Feb 2018       205223.4 166456.2 243990.7 145934.0 264512.8
    ## Mar 2018       286876.9 231582.8 342170.9 202311.9 371441.8
    ## Apr 2018       383590.8 308125.3 459056.2 268176.4 499005.2
    ## May 2018       412234.7 329431.0 495038.4 285597.4 538872.0
    ## Jun 2018       422610.3 335918.7 509301.9 290026.9 555193.7
    ## Jul 2018       435415.4 344181.1 526649.7 295884.6 574946.2
    ## Aug 2018       439027.5 345048.8 533006.3 295299.4 582755.6

**Utilizing ets()**

A more general approach to exponential smoothing than Holt-Winters is to
use the `ets()` function which automatically chooses an exponential
smoothing model based upon 15 potential models (see more
[here](https://robjhyndman.com/talks/RevolutionR/6-ETS.pdf) on slide
10). The ets framework (error, trend, seasonality) tries out multiple
models and estimates the likelihood that the data gathered could be
generated from those individual models. Final model is chosen based upon
AIC or other fit statistics and accounts for any combination of
seasonality and damping.

The **model** parameter in the `ets()` function can be specified with a
three character string. The first letter denotes the error type, the
second letter denotes the trend type, and the third letter denotes the
season type. The options you can specify for each component are below:

-   error: additive (“A”), multiplicative (“M”), unknown (“Z”)
-   trend: none (“N”), additive (“A”), multiplicative (“M”), unknown
    (“Z”)
-   seasonality: none (“N”), additive (“A”), multiplicative (“M”),
    unknown (“Z”)

For example, setting `model='AAM'` would produce a model with additive
error, additive trend, and multiplicative seasonality. By default, the
parameter is set to “ZZZ” which passes unknown values to each component
and allows the algorithm to select the ‘optimal’ model. See `ets()`
reference
[here](https://www.rdocumentation.org/packages/forecast/versions/8.12/topics/ets)
and more on comparing to `hw()`,
[here](https://robjhyndman.com/hyndsight/estimation2/).

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

------------------------------------------------------------------------

References:

Hyndman, R.J., & Athanasopoulos, G. (2018) Forecasting: principles and
practice, 2nd edition, OTexts: Melbourne, Australia.
[OTexts.com/fpp2](https://otexts.com/fpp2/)
