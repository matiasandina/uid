# Flag temperature outliers based on rolling difference threshold

Flag temperature outliers based on rolling difference threshold

## Usage

``` r
flag_temperature_outliers(df, threshold = 1)
```

## Arguments

- df:

  A dataframe with `datetime`, `rfid`, and `temperature`.

- threshold:

  Maximum allowed temperature jump (default = 1).

## Value

Data frame with added `temp_diff` and `outlier_global` columns.
