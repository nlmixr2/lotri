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
#> <pointer: 0x7f87efc7c8f0>
#> 
#> $asLotriMat
#> <pointer: 0x7f87efc7a5e0>
#> 
#> $lotriSep
#> <pointer: 0x7f87efc7ec40>
#> 
#> $lotriAllNames
#> <pointer: 0x7f87efc7f1f0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f87efc7ac90>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f87efc7e6d0>
#> 
#> $isLotri
#> <pointer: 0x7f87efc7f0c0>
#> 
#> $lotriRcm
#> <pointer: 0x7f87efc897e0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f87efc84300>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f87efc84d10>
#> 
```
