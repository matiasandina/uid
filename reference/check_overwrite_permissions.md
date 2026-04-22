# Check for Existing Output Files and Prompt Overwrite Options

Given a set of group names and an output directory, this function checks
if the expected output files already exist. If so, prompts the user to
decide whether to overwrite, skip, or abort.

## Usage

``` r
check_overwrite_permissions(output_dir, group_names)
```

## Arguments

- output_dir:

  Path to the directory where output files are written.

- group_names:

  A character vector of file group base names.

## Value

A character vector of group names that should be processed. This
excludes any that the user chose to skip.
