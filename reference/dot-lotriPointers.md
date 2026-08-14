# This function is used to get the pointers to the C objects that are used in the lotri package.

This function is used to get the pointers to the C objects that are used
in the lotri package.

## Usage

``` r
.lotriPointers()
```

## Value

A list of function pointers

## Author

Matthew L. Fidler

## Examples

``` r

.lotriPointers()
#> $lotriLstToMat
#> <pointer: 0x7fe29d1c7590>
#> 
#> $asLotriMat
#> <pointer: 0x7fe29d1c5280>
#> 
#> $lotriSep
#> <pointer: 0x7fe29d1c93d0>
#> 
#> $lotriAllNames
#> <pointer: 0x7fe29d1c9980>
#> 
#> $lotriGetBounds
#> <pointer: 0x7fe29d1c5930>
#> 
#> $lotriMaxNu
#> <pointer: 0x7fe29d1c8e60>
#> 
#> $isLotri
#> <pointer: 0x7fe29d1c9850>
#> 
#> $lotriRcm
#> <pointer: 0x7fe29d1d3c80>
#> 
#> $lotriNearPDc
#> <pointer: 0x7fe29d1cef00>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7fe29d1cf440>
#> 
```
