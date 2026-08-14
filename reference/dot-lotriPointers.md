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
#> <pointer: 0x7fa06e417590>
#> 
#> $asLotriMat
#> <pointer: 0x7fa06e415280>
#> 
#> $lotriSep
#> <pointer: 0x7fa06e4193d0>
#> 
#> $lotriAllNames
#> <pointer: 0x7fa06e419980>
#> 
#> $lotriGetBounds
#> <pointer: 0x7fa06e415930>
#> 
#> $lotriMaxNu
#> <pointer: 0x7fa06e418e60>
#> 
#> $isLotri
#> <pointer: 0x7fa06e419850>
#> 
#> $lotriRcm
#> <pointer: 0x7fa06e423c80>
#> 
#> $lotriNearPDc
#> <pointer: 0x7fa06e41ef00>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7fa06e41f440>
#> 
```
