# Example UID Sample Data

This dataset contains 24 hours of simulated temperature and zone data
from three RFID-tagged animals. Temperatures include noise and realistic
artifacts such as dropped and spurious values. Zone entries are randomly
generated.

## Usage

``` r
uid_sample_data
```

## Format

A data frame with N rows and 6 columns:

- datetime:

  Timestamp in POSIXct format

- rfid:

  Unique identifier for each subject

- zone:

  Integer between 1 and 8

- session_name:

  Session identifier (character)

- temperature:

  Body temperature with artifacts

- matrix_name:

  Group label

## Source

Simulated data for testing uid functionality.
