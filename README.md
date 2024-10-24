# IGBPveg (Vegetation and land-use for CCAM)

IGBPveg is used to create vegetation, land-use, soil and urban maps for the
Conformal Cubic Atmospheric Model (CCAM).  Maps can be based on different
source datasets, include CMIP6 land-use changes and can be customised with
regional user defined datasets.

## Website

For documentation, see our website at

[https://research.csiro.au/ccam/]

## Dependencies

IGBPveg requires the NetCDF C library.

## Building IGBPveg

To build IGBPveg with intel, gnu and cray fortran compiler use

```
make
make GFORTRAN=yes
make CRAY=yes
```

Debugging is also enabled with

```
make TEST=yes
```
