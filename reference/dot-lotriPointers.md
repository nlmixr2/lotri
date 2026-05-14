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
#> <pointer: 0x7f280d850070>
#> 
#> $asLotriMat
#> <pointer: 0x7f280d84e2a0>
#> 
#> $lotriSep
#> <pointer: 0x7f280d851ce0>
#> 
#> $lotriAllNames
#> <pointer: 0x7f280d852280>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f280d84e7e0>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f280d851770>
#> 
#> $isLotri
#> <pointer: 0x7f280d852160>
#> 
#> $lotriRcm
#> <pointer: 0x7f280d85c600>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f280d857840>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f280d857d80>
#> 
```
