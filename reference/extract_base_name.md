# Extract Base Name from UID CSV Filename

Internal helper that removes `_x_of_y.CSV` suffix from a UID file name,
returning the shared base name across split parts of a session.

## Usage

``` r
extract_base_name(filename)
```

## Arguments

- filename:

  A character vector of file paths or names.

## Value

A character vector with the base names.
