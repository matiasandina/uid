# Wrapper to clean UID temperature CSV: read, flag outliers, downsample, and plot

The cleaning workflow assumes that each `rfid` belongs to a single
`matrix_name` within a session. If the same `rfid` is detected on more
than one matrix, the function prints a summary table with
`session_name`, `rfid`, `matrix_name`, `n`, `%`, `min_dt`, and `max_dt`.

## Usage

``` r
clean_raw_uid(
  filepath,
  n = 1,
  precision = "minute",
  outlier_threshold_celsius = 1,
  output_dir = "temperature/data",
  plot = TRUE,
  flicker_correction = TRUE,
  flicker_dominant_two_thr = 0.65,
  flicker_alt_rate_thr = 0.4,
  flicker_contiguous_thr = 0.65
)
```

## Arguments

- filepath:

  Path to a raw UID CSV.

- n:

  Downsampling interval size (default = 1) applied to both temperature
  and activity data.

- precision:

  Time unit for downsampling (default = "minute").

- outlier_threshold_celsius:

  Temperature difference threshold to flag outliers.

- output_dir:

  Directory to save diagnostic plots.

- plot:

  boolean to indicate whether to plot the process of outlier removal and
  downsampling

- flicker_correction:

  Whether to apply activity flicker correction before quantification.

- flicker_dominant_two_thr:

  Threshold for dominant two-zone occupancy fraction.

- flicker_alt_rate_thr:

  Threshold for ABAB alternation rate within minute bins.

- flicker_contiguous_thr:

  Threshold for fraction of contiguous transitions within minute bins.

## Value

A list with cleaned, downsampled `temperature` and `activity` data
frames.

## Details

Low-percentage duplicate detections can happen when matrices are placed
too close to each other or when a cage is briefly placed on the wrong
platform. In those cases, the function asks for explicit confirmation
before removing rows from the matrix with the lower percentage of
detections. If duplicate measurements on the secondary matrix exceed 10%
of detections for that `session_name`/`rfid` pair, the function aborts
and requires manual review.
