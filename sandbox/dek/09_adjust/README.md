Hypothesis 1: all "adjusted" data are NA for realtime data.

Hypothesis 2: some "adjusted" data are non-NA for delayed data.

The adjustment process is complicated.  There are two cases, depending on
whether a `PARAMETER_DATA_MODE` is present in the metadata stored in the netcdf
file.

1. **Case 1.** If the `PARAMETER_DATA_MODE` field is present, then its columns
   are mapped to the parameter columns, and the following action is undertaken.

* If `PARAMETER_DATA_MODE` is `"A"` (meaning "Adjusted") then (fill in ).  

* If `PARAMETER_DATA_MODE` is `"D"` (meaning "Delayed"), use the Adjusted values.  

* If `PARAMETER_DATA_MODE` is `"R"` (meaning "Realtime") then use the raw
  values, even if the file contains data with `<PARAM>_ADJUSTED` (e.g.
`"TEMP_ADJUSTED"`, which is the name used in netcdf files for the adjusted
temperature) in their names.

2. **Case 2.** If no `PARAMETER_DATA_MODE` field is present, the `DATA_MODE`
   field is used.  This does not apply to individual columns, so the action
depends on the data within the columns.  

* If `DATA_MODE` is `"D"` then each data type is considered in turn, and if the
  `<PARAM>_ADJUSTED` values are *all* `NA` then the raw data are used, but if
any of the `<PARAM>_ADJUSTED` values are not `NA`, then *all* the
`<PARAM>_ADJUSTED` data are used.

* If `DATA_MODE` is `"R"` then then use the raw values, even if the file
  contains data with `<PARAM>_ADJUSTED` in their names.

