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
#> <pointer: 0x7fe0c31c78f0>
#> 
#> $asLotriMat
#> <pointer: 0x7fe0c31c55e0>
#> 
#> $lotriSep
#> <pointer: 0x7fe0c31c9c40>
#> 
#> $lotriAllNames
#> <pointer: 0x7fe0c31ca1f0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7fe0c31c5c90>
#> 
#> $lotriMaxNu
#> <pointer: 0x7fe0c31c96d0>
#> 
#> $isLotri
#> <pointer: 0x7fe0c31ca0c0>
#> 
#> $lotriRcm
#> <pointer: 0x7fe0c31d47e0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7fe0c31cf300>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7fe0c31cfd10>
#> 
```
