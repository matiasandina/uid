# Calculate Activity Index Based on Zone Transitions

Computes theoretical distance traveled between sequential zone visits
using UID Mouse Matrix coordinates. Adds a new column `activity_index`
to the data frame.

## Usage

``` r
calculate_activity(df)
```

## Arguments

- df:

  A data frame with columns `rfid`, `datetime`, and `zone`

## Value

The input data frame with an added `activity_index` column (in inches).
