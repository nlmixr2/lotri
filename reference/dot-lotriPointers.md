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
#> <pointer: 0x7f99d59f1590>
#> 
#> $asLotriMat
#> <pointer: 0x7f99d59ef280>
#> 
#> $lotriSep
#> <pointer: 0x7f99d59f33d0>
#> 
#> $lotriAllNames
#> <pointer: 0x7f99d59f3980>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f99d59ef930>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f99d59f2e60>
#> 
#> $isLotri
#> <pointer: 0x7f99d59f3850>
#> 
#> $lotriRcm
#> <pointer: 0x7f99d59fdc80>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f99d59f8f00>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f99d59f9440>
#> 
```
