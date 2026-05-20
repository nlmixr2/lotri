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
#> <pointer: 0x7f8bed8170a0>
#> 
#> $asLotriMat
#> <pointer: 0x7f8bed815270>
#> 
#> $lotriSep
#> <pointer: 0x7f8bed818c80>
#> 
#> $lotriAllNames
#> <pointer: 0x7f8bed819230>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f8bed815920>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f8bed818710>
#> 
#> $isLotri
#> <pointer: 0x7f8bed819100>
#> 
#> $lotriRcm
#> <pointer: 0x7f8bed8235c0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f8bed81e800>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f8bed81ed40>
#> 
```
