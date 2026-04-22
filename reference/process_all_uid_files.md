# Process All UID CSV Files in Batch

Finds, groups, and processes all raw UID export CSVs in a directory,
then saves cleaned and downsampled outputs to a target directory.

## Usage

``` r
process_all_uid_files(
  raw_export_dir = "temperature/raw_data",
  output_dir = "temperature/data",
  n = 1,
  precision = "minute",
  outlier_threshold = 1,
  flicker_correction = TRUE,
  flicker_dominant_two_thr = 0.65,
  flicker_alt_rate_thr = 0.4,
  flicker_contiguous_thr = 0.65
)
```

## Arguments

- raw_export_dir:

  Directory containing raw UID .CSV files.

- output_dir:

  Directory to write cleaned files to.

- n:

  Bin size for downsampling (default = 1).

- precision:

  Time unit for binning (default = \\minute\\).

- outlier_threshold:

  Temperature delta threshold for outlier detection.

- flicker_correction:

  Whether to apply activity flicker correction before quantification.

- flicker_dominant_two_thr:

  Threshold for dominant two-zone occupancy fraction.

- flicker_alt_rate_thr:

  Threshold for ABAB alternation rate within minute bins.

- flicker_contiguous_thr:

  Threshold for contiguous-transition fraction within minute bins.

## Value

Invisibly returns a list of output file paths.

## Details

Each cleaned session is expected to contain a single `rfid` to
`matrix_name` mapping. If the same `rfid` appears on more than one
matrix within a session, the cleaning step prints a summary table and
requires an interactive choice. Low-percentage stray detections can be
removed after confirmation; more substantial duplicate detections abort
processing and should be reviewed manually.
