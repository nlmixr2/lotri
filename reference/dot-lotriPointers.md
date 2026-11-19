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
#> <pointer: 0x74bef15d6ff0>
#> 
#> $asLotriMat
#> <pointer: 0x74bef15d4bd0>
#> 
#> $lotriSep
#> <pointer: 0x74bef15d9510>
#> 
#> $lotriAllNames
#> <pointer: 0x74bef15d9aa0>
#> 
#> $lotriGetBounds
#> <pointer: 0x74bef15d52b0>
#> 
#> $lotriMaxNu
#> <pointer: 0x74bef15d8fb0>
#> 
#> $isLotri
#> <pointer: 0x74bef15d9980>
#> 
#> $lotriRcm
#> <pointer: 0x74bef15e7450>
#> 
#> $lotriNearPDc
#> <pointer: 0x74bef15df5c0>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x74bef15e0010>
#> 
```
