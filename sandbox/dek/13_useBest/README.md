See https://github.com/ArgoCanada/argoFloats/issues/408

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
      non-BGC dataset since dataMode exists (it is DAD)
      pressureAdjusted -> pressure (1293 finite data)
      temperatureAdjusted -> temperature (1293 finite data)
      conductivityAdjusted -> conductivity (958 finite data)
      salinityAdjusted -> salinity (1039 finite data)
```

