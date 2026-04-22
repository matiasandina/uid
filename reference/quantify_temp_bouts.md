# Quantify bouts of temperature above or below a threshold

Identifies contiguous periods ("bouts") where temperature is
consistently above or below a user-defined threshold, allowing optional
gap-tolerant detection and customizable duration calculation.

## Usage

``` r
quantify_temp_bouts(
  df,
  threshold,
  sampling_interval = estimate_sampling_interval,
  direction = c("below", "above"),
  greedy = FALSE,
  max_gap = 10,
  duration_mode = c("strict", "inclusive"),
  drop_single_point_bouts = FALSE,
  fill_undetected_groups = TRUE
)
```

## Arguments

- df:

  A grouped data frame with at least columns: `rfid`, `common_dt`,
  `variable`, and `value`. Must be grouped (e.g., by `rfid`) prior to
  calling this function.

- threshold:

  Numeric threshold to compare against temperature values.

- sampling_interval:

  A numeric value representing the sampling interval in minutes, or a
  function that takes `df` and returns a single numeric value (e.g.,
  [`estimate_sampling_interval()`](https://matiasandina.github.io/uid/reference/estimate_sampling_interval.md)).

- direction:

  Either `"below"` or `"above"` to determine whether to identify values
  less than or equal to (`"below"`) or greater than or equal to
  (`"above"`) the threshold.

- greedy:

  Logical. If `TRUE`, allows `max_gap` NAs within a bout. If `FALSE`,
  bouts are broken by any NA or threshold-violating value.

- max_gap:

  Maximum number of consecutive `NA` values allowed within a bout when
  `greedy = TRUE`.

- duration_mode:

  How to compute bout duration:

  - `"strict"`: calculates `end - start`

  - `"inclusive"`: calculates `end - start + sampling_interval`

- drop_single_point_bouts:

  If `TRUE`, removes bouts consisting of only one time point (i.e.,
  duration = 0 in `"strict"` mode). Default is `FALSE`.

- fill_undetected_groups:

  If `TRUE` (default), ensures that all groups from the input are
  represented in the output, even if no bouts were found, with
  `duration_minutes = 0`.

## Value

A data frame with one row per detected bout per animal. Columns include:

- `start`, `end`: timestamps of each bout

- `duration_minutes`: duration of each bout in minutes
