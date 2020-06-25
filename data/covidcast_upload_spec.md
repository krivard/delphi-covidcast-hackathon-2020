# Data format specification for CSV output

The output of the surveys/ package is a set of CSV files suitable for uploading to the COVIDcast API. All CSV files uploaded to the API must abide by the requirements described below.

Criteria for a valid filename:

- File name format: `{date}_{geo}_{signal}.csv`
	- `date` is `YYYYmmdd`
		- Minimum year: 2019
		- Maximum year: 2030
	- `geo` is one of:
		- `state` for first-level administrative regions in the US
		- `county` for second-level administrative regions in the US
		- `msa` for Metropolitan Statistical Areas. Each MSA represents a major urban area as defined by the US Census.
		- `hrr` for Hospital Referral Regions. Each hospital referral region is an area of the US that refers patients to a particular system of regional hospitals.
	- `signal` must be matched by a `/\w+/` regex

If a filename is invalid, it is rejected.

Criteria for a valid file:

- First row is the header
	- Required columns: `geo_id`,`val`,`se`,`sample_size`
	- Additional columns are permitted but will be ignored
- Remaining rows are the data
	- `geo_id` for `hrr`, `msa` files must be interpretable as a `/[0-9]+/` string
	  (ints and floats are allowed but discouraged)
	- `geo_id` for `county` files must have length 5 and sort between '01000' and '80000'
	- `geo_id` for `hrr` files must sort between '001' and '500'
	- `geo_id` for `msa` files must have length 5 and sort between '10000' and'99999'
	- `geo_id` for `state` files must have length 2 and sort between 'aa' and 'zz'
	- `value` must be a real number (ie not nan, inf, empty, na, or none)
	- `se` may be nan; if it is a number, it must be nonnegative
	- `sample_size` may be nan; if it is a number, it must be non-negative

If a file has invalid headers, it is rejected.

If a row has invalid data, the file is rejected.

If the filename is valid and all rows in the file are valid, the file is accepted for submission.
