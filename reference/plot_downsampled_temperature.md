# Plot and save downsampled temperature trace

Generates a line plot of downsampled `temperature` over `common_dt`,
faceted by `rfid`, and writes it to
`output_dir/clean_plots/<basename>_downsampled.png`.

## Usage

``` r
plot_downsampled_temperature(df_down, output_dir, filepath)
```

## Arguments

- df_down:

  Data frame returned by `downsample()`. Must contain `common_dt`,
  `temperature`, and `rfid`.

- output_dir:

  Directory where `clean_plots/` will be created (if needed).

- filepath:

  Original CSV path—used to derive the base filename for saving.

## Value

Invisibly returns the ggplot object.
