See https://github.com/ArgoCanada/argoFloats/issues/408

# Files

* `13_useBest_01.R` examines data and demonstrates idea of copying things over,
  but does not do a complete job.  This is mostly used as a tool to examine
  what is in the data.  See 'WARNING' and 'ERROR' in the output file.
* `13_useBest_03.R` revision of `13_useBest_01.R` to accomplish plans set out
  by JH and myself on 2021-03-01.


# Issues.

## multiple DATA_MODES

As shown below, some (possibly many) files have data **columns** that are of
unequal modes.  Should useBest() work differently on each column?

```
 3. /Users/kelley/Dropbox/data/argo/D6902829_047.nc (filename suggests delayed data)
      non-BGC dataset since dataMode exists (it is DRRRRRR)
      pressureAdjusted -> pressure (976 finite data)
      temperatureAdjusted -> temperature (976 finite data)
      salinityAdjusted -> salinity (976 finite data)
```
```
6. /Users/kelley/Dropbox/data/argo/D5904013_153.nc (filename suggests delayed data)
gg
      non-BGC dataset since dataMode exists (it is DAD)
      pressureAdjusted -> pressure (1293 finite data)
      temperatureAdjusted -> temperature (1293 finite data)
      conductivityAdjusted -> conductivity (958 finite data)
      salinityAdjusted -> salinity (1039 finite data)
```

