
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/IMMM-SFA/horizon.svg?branch=master)](https://travis-ci.org/IMMM-SFA/horizon) [![codecov](https://codecov.io/gh/IMMM-SFA/horizon/branch/master/graph/badge.svg)](https://codecov.io/gh/IMMM-SFA/horizon)

horizon
=======

The goal of `horizon` is to infer operating policies from daily dam and reservoir data, including the use of foresight in release decisions.

Installation
------------

You can install `horizon` from this repository using `devtools`:

``` r
devtools::install_github("IMMM-SFA/horizon")
```

Once installed, load the library:

``` r
library(horizon)
```

Walk-through example
--------------------

The following example walks through the derivation of weekly operating policy and forecast use signature for Glen Canyon Dam (Lake Powell), which is operated by the US Bureau of Reclamation (<https://www.usbr.gov/uc/water/crsp/cs/gcd.html>).

### Read and pre-process data

`horizon` is designed to read daily operational csv files in the format: date (yyyy-mm-dd), storage (acrefeet), release (cubic feet per second), and inflow (cubic feet per second).

The `show_dams` function may be used to explore the default dataset. Running `show_dams("usbr"`) will show all US Bureau of Reclamation dams available for analysis. The data for any dam is read into the environment using `read_dam()`:

``` r
read_dam("usbr_lakepowell") -> lakepowell_raw
lakepowell_raw
#> # A tibble: 19,899 x 4
#>    date        s_af r_cfs i_cfs
#>    <date>     <dbl> <dbl> <dbl>
#>  1 1963-06-28     0    NA 8199.
#>  2 1963-06-29  5400    NA 7481.
#>  3 1963-06-30  7400    NA 4911.
#>  4 1963-07-01  9400    NA 4845.
#>  5 1963-07-02 13400    NA 5064.
#>  6 1963-07-03 17400    NA 4847.
#>  7 1963-07-04 17400    NA 3524.
#>  8 1963-07-05 20400    NA 4087.
#>  9 1963-07-06 20400    NA 3162.
#> 10 1963-07-07 20400    NA 3037.
#> # ... with 19,889 more rows
```

The function argument is simply a combined string indicating the data source ("usbr") follwed by the dam ("lakepowell") separated by an underscore.

The data include daily inflow and storage. Release data are unavailable and will have to be estimated later...

<img src="man/figures/README-dam data plot-1.png" width="80%" style="display: block; margin: auto;" />

`horizon` contains a variety of functions for pre-processing the data into the correct format for deriving "forecast use signatures".

#### Step 1: `convert_to_metric`

First, we convert the units to metric volumes (so that inflow and release are daily totals rather than flow rates):

``` r
lakepowell_raw %>% convert_to_metric() ->
  lakepowell_metric  # all units now in Million cubic meters

lakepowell_metric
#> # A tibble: 19,899 x 4
#>    date           s     i     r
#>    <date>     <dbl> <dbl> <dbl>
#>  1 1963-06-28  0    20.1     NA
#>  2 1963-06-29  6.66 18.3     NA
#>  3 1963-06-30  9.13 12.0     NA
#>  4 1963-07-01 11.6  11.9     NA
#>  5 1963-07-02 16.5  12.4     NA
#>  6 1963-07-03 21.5  11.9     NA
#>  7 1963-07-04 21.5   8.62    NA
#>  8 1963-07-05 25.2  10.00    NA
#>  9 1963-07-06 25.2   7.74    NA
#> 10 1963-07-07 25.2   7.43    NA
#> # ... with 19,889 more rows
```

#### Step 2: `fill_NAs`

Next we fill any small gaps in the time series. `fill_NAs` fills gaps up to a maximum of `max_fill_gap` days (default = 10); records containing longer gaps will be excluded from the analysis at a later stage.

``` r
lakepowell_metric %>%
  fill_NAs(max_fill_gap = 10) ->
  # ^^ gaps of maximum 10 days are filled using cubic spline interpolation
  lakepowell_gapfilled

lakepowell_gapfilled
#> # A tibble: 19,899 x 4
#>    date           s     i     r
#>    <date>     <dbl> <dbl> <dbl>
#>  1 1963-06-28  1.56 20.1     NA
#>  2 1963-06-29  6.66 18.3     NA
#>  3 1963-06-30  9.13 12.0     NA
#>  4 1963-07-01 11.6  11.9     NA
#>  5 1963-07-02 16.5  12.4     NA
#>  6 1963-07-03 21.5  11.9     NA
#>  7 1963-07-04 21.5   8.62    NA
#>  8 1963-07-05 25.2  10.00    NA
#>  9 1963-07-06 25.2   7.74    NA
#> 10 1963-07-07 25.2   7.43    NA
#> # ... with 19,889 more rows
```

#### Step 3: `convert_to_water_years`

``` r
lakepowell_gapfilled %>%
  convert_to_water_years() ->
  lakepowell_wateryrs

lakepowell_wateryrs %>%
  filter(water_year == 2016)
#> # A tibble: 366 x 5
#>    water_year date            s     i     r
#>         <dbl> <date>      <dbl> <dbl> <dbl>
#>  1       2016 2015-10-01 15200.  12.9    NA
#>  2       2016 2015-10-02 15190.  15.6    NA
#>  3       2016 2015-10-03 15183.  16.1    NA
#>  4       2016 2015-10-04 15177.  16.5    NA
#>  5       2016 2015-10-05 15215.  66.4    NA
#>  6       2016 2015-10-06 15248.  61.1    NA
#>  7       2016 2015-10-07 15249.  27.3    NA
#>  8       2016 2015-10-08 15249.  26.3    NA
#>  9       2016 2015-10-09 15244.  20.5    NA
#> 10       2016 2015-10-10 15240.  19.9    NA
#> # ... with 356 more rows

# the water year starts 1st October of the prior calendar year
```

#### Step 4: `aggregate_to_water_weeks`

After identifying water years, the daily time series are aggregated to water weeks 1-52 (`horizon:::gen_water_weeks()` may be used to view calendar days to water week mapping).

``` r
lakepowell_wateryrs %>%
  aggregate_to_water_weeks() ->
  lakepowell_weekly

lakepowell_weekly
#> # A tibble: 2,833 x 9
#>    water_year water_week     i     r s_start s_end s_change    r_    i_
#>         <dbl>      <dbl> <dbl> <dbl>   <dbl> <dbl>    <dbl> <dbl> <dbl>
#>  1       1963         40  69.9    NA    11.6  25.2    13.6   56.3    NA
#>  2       1963         41  47.6    NA    25.2  32.6     7.40  40.2    NA
#>  3       1963         42  54.9    NA    32.6  58.5    25.9   29.0    NA
#>  4       1963         43  35.2    NA    58.5  60.9     2.47  32.7    NA
#>  5       1963         44  26.7    NA    60.9  63.4     2.47  24.2    NA
#>  6       1963         45  73.6    NA    63.4 126.     62.9   10.7    NA
#>  7       1963         46  79.3    NA   126.  179.     53.0   26.3    NA
#>  8       1963         47  69.3    NA   179.  224.     44.4   24.9    NA
#>  9       1963         48 132.     NA   224.  369.    146.     0      NA
#> 10       1963         49 152.     NA   369.  485.    116.    36.4    NA
#> # ... with 2,823 more rows
```

Some new variables are introduced, too. `s_start` and `s_end` are the starting and end storage volumes for each week, and `s_change` is the resulting change in storage. The latter is used to back-calculate new variables `i_` and `r_`, which are the inflow and release volumes **estimated** using `s_change` assuming convervation of mass (and no evaporation or other water losses).

#### Step 5: `back_calc_missing_flows`

The final step is then to select the final set of inflow, release and storage variables for forecast use signature derivation:

``` r
lakepowell_weekly %>%
  back_calc_missing_flows(compute_from = "i") ->
  # ^^ compute_from = "i" tells the function to use the estimated release...
  # ... since observed release is missing
  lakepowell_final

lakepowell_final
#> # A tibble: 2,833 x 5
#>    water_year water_week     i     r s_start
#>         <dbl>      <dbl> <dbl> <dbl>   <dbl>
#>  1       1963         40  69.9  56.3    11.6
#>  2       1963         41  47.6  40.2    25.2
#>  3       1963         42  54.9  29.0    32.6
#>  4       1963         43  35.2  32.7    58.5
#>  5       1963         44  26.7  24.2    60.9
#>  6       1963         45  73.6  10.7    63.4
#>  7       1963         46  79.3  26.3   126.
#>  8       1963         47  69.3  24.9   179.
#>  9       1963         48 132.    0     224.
#> 10       1963         49 152.   36.4   369.
#> # ... with 2,823 more rows
```

### Release-availability functions

All of the above steps are rolled into the `compute_availability` function, which takes the additional step of adding the **availability** variable (`a`) for a chosen water week and given future inflow horizon. The availabiltiy is simply the sum of the starting storage and the cumulative inflow out to the chosen horizon (in weeks). For example:

``` r
compute_availability("usbr_lakepowell",
                     # ^^ note that we can simply supply the name of the dam;
                     # the function carries out pre-processing automatically.
                     water_week = 1, horizon = 1,
                     min_allowable_points = 10,
                     cutoff_year = 1995)
#> # A tibble: 24 x 8
#>    water_year water_week     i     r s_start i_sum      a horizon
#>         <dbl>      <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl>   <int>
#>  1       1995          1 107.   143.  21912. 107.  22019.       1
#>  2       1996          1 152.   264.  27494. 152.  27646.       1
#>  3       1997          1 184.   215.  26080. 184.  26264.       1
#>  4       1998          1 331.   340.  28119. 331.  28450.       1
#>  5       1999          1 135.   260.  27621. 135.  27756.       1
#>  6       2000          1 204.   312.  28353. 204.  28557.       1
#>  7       2001          1 111.   177.  25818. 111.  25929.       1
#>  8       2002          1  58.8  171.  23584.  58.8 23643.       1
#>  9       2003          1  96.0  154.  17849.  96.0 17945.       1
#> 10       2004          1 112.   142.  14936. 112.  15048.       1
#> # ... with 14 more rows
```

This implementation of `compute_availability` provides the required data to display the release-availability scatter for water week 1 with a horizon of 1 week ahead (from the start of the water week). Two additional arguments are supplied. `min_allowable_points` sets a minimum number fo data points, and throws an error in cases where there are less than the specified number of years of release and availability data for a given water week (here set to 10 data points). `cutoff_year` filters the input data to remove all points prior to the cutoff year (here 1995). Long records likely encompass different release policies (and use of forecasts may be relatively recent). The cutoff helps avoid conflating the analysis with multiple operating polcies across many years of operation.

`horizon` features in-built functions for plotting these data:

``` r
hplot_ready_data("usbr_lakepowell", water_week = 1, horizon = 1,
                 cutoff_year = 1995)
```

<img src="man/figures/README-comp av plot-1.png" width="80%" style="display: block; margin: auto;" />

We can add an optimized piecewise linear function with a simple call to `add_piecewise_fn`:

``` r
hplot_ready_data("usbr_lakepowell", water_week = 1, horizon = 1,
                 cutoff_year = 1995,
                 add_piecewise_fn = TRUE)
```

<img src="man/figures/README-comp av plot pw-1.png" width="80%" style="display: block; margin: auto;" />

In the above case it appears that availabilty predics release quite well; the simple policy function fits nicely, particularly for the wetter years of operation when water availability is high. But often availability with . In the following example, where the water week is changed to week 25, we see that water availability with a horizon of 1 week is a poor predictor of the release decision:

``` r
hplot_ready_data("usbr_lakepowell", water_week = 25, horizon = 1,
                 cutoff_year = 1995,
                 add_piecewise_fn = TRUE)
```

<img src="man/figures/README-comp av plot pw wk25-1.png" width="80%" style="display: block; margin: auto;" />

One can use the `hplot_ready_data` function to investigate multiple water weeks and horzions simultaneously:

``` r
hplot_ready_data("usbr_lakepowell", water_week = 24:25, horizon = c(1, 12),
                 cutoff_year = 1995,
                 add_piecewise_fn = TRUE)
```

<img src="man/figures/README-comp av plot pw wk25 mult-1.png" width="80%" style="display: block; margin: auto;" />

Now we see that water availability with a horizon of 12 weeks results in a much closer policy fit than water availability with a horizon of one week. \*\* In deriving the foreccast use signature, we use the above form of analysis to infer the forecast horizon that might be used in determining water release decisions at different weeks of the water year.\*\*

We can generate the piecewise functions for all water weeks with all candidate horizons (1 - 30 weeks ahead) simultaneously using `get_optimized_models`. An optional argument `write_to` (not used here) can be used to specify an output directory where results will be saved in `.csv` format.

``` r
get_optimized_models("usbr_lakepowell") ->
  usbr_lakepowell_pw_all
```

**note:** this can take several minutes to run, since all 30 \* 52 piecewise functions must be optimized. `horizon` is designed to use all cores of your machine to derive these piecewise models.

``` r
usbr_lakepowell_pw_all
#> # A tibble: 1,560 x 7
#>         p1     p2    p3     p4  r_sq water_week horizon
#>      <dbl>  <dbl> <dbl>  <dbl> <dbl>      <int>   <int>
#>  1 0.0027  0.0497  189. 25950. 0.780          1       1
#>  2 0.0068  0.0072  166. 14894. 0.421          2       1
#>  3 0.0028  0.0977  195. 27005. 0.731          3       1
#>  4 0.00240 0.0899  194. 26758. 0.720          4       1
#>  5 0.0051  0.228   225. 27443. 0.825          5       1
#>  6 0.007   0.007   183. 14821. 0.144          6       1
#>  7 0.0017  0.429   219. 27486. 0.31           7       1
#>  8 0.0011  0.361   218. 27345. 0.153          8       1
#>  9 0.0054  0.0461  247. 26479. 0.555          9       1
#> 10 0.0061  0.0061  225. 17605. 0.396         10       1
#> # ... with 1,550 more rows
```

The result includes the parameters (`p1`, `p2`, `p3`, `p4`) and goodness-of-fit (`r_sq`, the coefficient of determination) for piecewise functions for *all* combinations of water weeks and possible inflow horizons from 1 to 30 weeks ahead. The four parameters represent the left-hand and right-hand side function slopes and break-point coordinates (`p3` = release, `p4` = availability), respectively.

### Forecast use signature derivation

If we plot for each water week the R-squared values as a function of the horizon, we begin to get a sense of how foresight may be driving operations:

<img src="man/figures/README-plot policy fits-1.png" width="80%" style="display: block; margin: auto;" />

The plot reveals an interesting shift in pattern throught he water year. Early in the water year (top row) the policy fits (y-axes) are relatively stable across horizons (x-axes); there is no strong evidence of a release policy being driven by any particular inflow horizon more than another. In such cases we infer that releases are dictated by the currently available water (i.e., water in storage plus current period inflow). The shift occurs by around week 16 (mid-January), at which point the policy fits begin to improve with longer horizons. By week 21 (end February) the evidence for foresight in operations becomes very strong, with the 15-week horizon offering a policy fit of approximately 0.8, compared to ~0.2 without foresight use (i.e., horizon = 1 week).

The forecast use signature specifies the horizon used in operations for all weeks of the water year. It is created by selecting the horizon for each water week that provides the closest piecewise function fit.

``` r
usbr_lakepowell_pw_all %>%
  select_best_horizon() %>%
  hplot_selected_models()
```

<img src="man/figures/README-fcast use sig-1.png" width="80%" style="display: block; margin: auto;" />

The above forecast use signature is rather noisy--which is unsurprising given some of the uncertainties in the method. However, we want to avoid inferring forecast use in cases where the policy fits are weak. We also want to avoid sharp spikes. We address the noise with some smoothing:

``` r
usbr_lakepowell_pw_all %>%
  select_best_horizon() %>%
  horizon:::remove_low_rsq(rsq_cutoff = 0.2) %>%
  # ^^ sets horizon to 1 for all cases with R-squared < rsq_cutoff
  horizon:::despike() %>%
  # ^^ removes sharp spikes
  horizon:::post_smooth() %>%
  # ^^ gentle smoothing spline
  hplot_selected_models() + ylim(0, 30)
```

<img src="man/figures/README-fcast use sig smth-1.png" width="80%" style="display: block; margin: auto;" />

The forecast use signature suggests long-range forecast use (~3-4 months ahead) from mid-December (water week 10) through late spring, suggesting that release policy at Glen Canyon may be formed using a combination of planned upstream releases and snowpack information, which indicates the April-June snowmelt-driven inflows.
