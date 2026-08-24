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
#> <pointer: 0x7fa0d8cec8d0>
#> 
#> $asLotriMat
#> <pointer: 0x7fa0d8cea5c0>
#> 
#> $lotriSep
#> <pointer: 0x7fa0d8cee710>
#> 
#> $lotriAllNames
#> <pointer: 0x7fa0d8ceecc0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7fa0d8ceac70>
#> 
#> $lotriMaxNu
#> <pointer: 0x7fa0d8cee1a0>
#> 
#> $isLotri
#> <pointer: 0x7fa0d8ceeb90>
#> 
#> $lotriRcm
#> <pointer: 0x7fa0d8cf92b0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7fa0d8cf3dd0>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7fa0d8cf47e0>
#> 
```
