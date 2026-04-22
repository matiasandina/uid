# uid 0.1.3

* Breaking change: `downsample_activity()` now summarizes per-bin activity using total distance (`sum`) instead of `median`.
* Added NA-safe handling in `downsample_activity()` so all-`NA` bins stay `NA` instead of becoming `0`.
* Added default activity flicker correction in `clean_raw_uid()`/`process_all_uid_files()` using minute-level zone-pattern thresholds and propagated a `.flicker_corrected` flag into downsampled activity output.
* Added duplicate RFID/matrix validation in `clean_raw_uid()`/`process_all_uid_files()`: low-percentage stray detections can be removed after explicit confirmation, while substantial duplicate detections now abort with a summary table for manual review.

# uid 0.1.2

* Adding column validation on reading of raw data
* Adding sampling data `uid_sample_data`
* Adding vignette for basic processing (step-by-step).

# uid 0.1.1

* Adding helper function to estimate sampling rate (`uid:::estimate_sampling_interval()`)
* Adding tests for helper function
* Allowing sampling rate to be estimated from data or passed via parameter
* removing `run_id` from bout quantification output
* Fixing some typos
* Updating `README.md`

# uid 0.1.0

* Initial Github submission.
* Adding basic function to read, write, process
* Adding basic linear gap interpolation
* Adding basic activity metrics
* Adding basic above/below threshold bout detection and time calculation
  * Including gap filling for duration processing
