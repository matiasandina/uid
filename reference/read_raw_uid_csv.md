# Read and clean raw UID CSV file

Reads a UID-formatted CSV, cleans column names, and parses datetime.

## Usage

``` r
read_raw_uid_csv(filepath, dt_format = "%Y/%m/%d %H:%M:%S")
```

## Arguments

- filepath:

  Path to the raw CSV file.

- dt_format:

  Sting defining the format for the time `string` to `datetime`
  conversion (e.g,. default is `"%Y/%m/%d %H:%M:%S"`)

## Value

A cleaned data frame with a `datetime` column.
