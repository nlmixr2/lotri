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
#> <pointer: 0x7f845a0bd0b0>
#> 
#> $asLotriMat
#> <pointer: 0x7f845a0bb2b0>
#> 
#> $lotriSep
#> <pointer: 0x7f845a0beda0>
#> 
#> $lotriAllNames
#> <pointer: 0x7f845a0bf340>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f845a0bb7f0>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f845a0be830>
#> 
#> $isLotri
#> <pointer: 0x7f845a0bf220>
#> 
#> $lotriRcm
#> <pointer: 0x7f845a0c96f0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f845a0c49d0>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f845a0c4f10>
#> 
```
