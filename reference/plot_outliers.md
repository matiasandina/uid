# Plot and save outlier diagnostic

Creates a line plot of temperature over time, highlighting outliers in
red, and writes it to `output_dir/clean_plots/<basename>_outliers.png`.

## Usage

``` r
plot_outliers(df_flagged, output_dir, filepath)
```

## Arguments

- df_flagged:

  Data frame returned by `flag_outliers()`, must include columns
  `datetime`, `temperature`, `rfid`, and `outlier`.

- output_dir:

  Path to the root output directory for plots.

- filepath:

  Original data file path used to derive the base filename.

## Value

Invisibly returns the ggplot object.
