
<!-- README.md is generated from README.Rmd. Please edit that file -->
horizon
=======

The goal of `horizon` is to infer operating policies from daily dam and reservoir data, including the use of foresight in release decisions.

Installation
------------

You can install `horizon` from this repository using devtools:

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

Horizon is designed to read daily operational csv files in the format: date (yyyy-mm-dd), storage (acrefeet), release (cubic feet per second), and inflow (cubic feet per second).

The `show_dams` function may be used to explore the default dataset. Running `show_dams("usbr"`) will show all US Bureau of Reclamation dams available for analysis. The data for any dam is read into the environment using `read_dam()`:

``` r
read_dam("usbr_lakepowell") -> lakepowell_raw
print(lakepowell_raw)
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

`Horizon` contains a variety of functions for pre-processing the data into the correct format for deriving "forecast use signatures".

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

Next we fill any small gaps in the time series:

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

The above procedude will fill gaps up to a maximum of `max_fill_gap` days (default = 10), so that dam records with long gaps will be excluded from the analysis later.

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

In this step, the data are aggregated to water weeks 1-52 (you can use `horizon:::gen_water_weeks()` to see how calendar days are mapped to water weeks). Some new variables are introduced, too. `s_start` and `s_end` are the starting and end storage volumes for each week, and `s_change` is the resulting change in storage. The latter is used to back-calculate new variables `i_` and `r_`, which are the inflow and release volumes **estimated** using `s_change` assuming convervation of mass (and no evaporation or other water losses).

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

### Read and pre-process data

All of the above steps are rolled into the `compute_availability` function, which takes the additional step of adding the *availability* variable *for a chosen water week* and given future inflow *horizon*. The availabiltiy is simply the sum of the starting storage and the cumulative inflow out to the horizon h weeks. For example:

``` r
"usbr_lakepowell" %>%
  # ^^ we can simply supply the name of the dam; the function carries out pre-processing automatically.
  compute_availability(water_week = 1, horizon = 1,
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

The function provides the required data to display the release-availability scatter for water week 1 with a horizon of 1 week ahead (from the start of the water week). These are the data we use to understand the likelihood of a given horizon being used to inform the release policy.

Two additional arguments are supplied here. The argument `min_allowable_points` helps identify cases where there is insufficient data for inferring a release policy. This works by throwing an error in cases where there are less than the specified number of years of release and availability data for a given water week (here set to 10 data points). The argument `cutoff_year` filters the input data to remove all points prior to the cutoff year (here 1995). Long records likely encompass different release policies, so the use of a cutoff year improves the chances that the data are representative of one policy.

`horizon` features in-built functions for plotting these data:

``` r
hplot_ready_data("usbr_lakepowell", water_week = 1, horizon =1,
                 cutoff_year = 1995)
```

<img src="man/figures/README-comp av plot-1.png" width="80%" style="display: block; margin: auto;" />

We can add an optimized piecewise linear function with a simple call to `add_piecewise_fn`:

``` r
hplot_ready_data("usbr_lakepowell", water_week = 1, horizon =1,
                 cutoff_year = 1995,
                 add_piecewise_fn = TRUE)
```

<img src="man/figures/README-comp av plot pw-1.png" width="80%" style="display: block; margin: auto;" />

In the above case it appears that availabilty predics release quite well; the simple policy function fits, particularly for the wetter years of operation on the right of the function. This may not always be the case. The following example, where the water week is changed to week 25, we see that water availability with a horizon of 1 week is a poor predictor of the release decision:

``` r
hplot_ready_data("usbr_lakepowell", water_week = 25, horizon =1,
                 cutoff_year = 1995,
                 add_piecewise_fn = TRUE)
```

<img src="man/figures/README-comp av plot pw wk25-1.png" width="80%" style="display: block; margin: auto;" />

One can use the `hplot_ready_data` function to investigate multiple water weeks and horzions simultaneously:

``` r
hplot_ready_data("usbr_lakepowell", water_week = 24:25, horizon = c(1, 15),
                 cutoff_year = 1995,
                 add_piecewise_fn = TRUE)
```

<img src="man/figures/README-comp av plot pw wk25 mult-1.png" width="80%" style="display: block; margin: auto;" />

The result shows that water availability with a horizon of 15 weeks results in a much closer policy fit than water availability with a horizon of one week. We use this form of analysis to infer the forecast horizon that might be used in determining water release decisions at different weeks of the water year.
