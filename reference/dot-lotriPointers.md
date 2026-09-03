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
#> <pointer: 0x7fe54318f8f0>
#> 
#> $asLotriMat
#> <pointer: 0x7fe54318d5e0>
#> 
#> $lotriSep
#> <pointer: 0x7fe543191c40>
#> 
#> $lotriAllNames
#> <pointer: 0x7fe5431921f0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7fe54318dc90>
#> 
#> $lotriMaxNu
#> <pointer: 0x7fe5431916d0>
#> 
#> $isLotri
#> <pointer: 0x7fe5431920c0>
#> 
#> $lotriRcm
#> <pointer: 0x7fe54319c7e0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7fe543197300>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7fe543197d10>
#> 
```
