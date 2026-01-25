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
#> <pointer: 0x7ff189665140>
#> 
#> $asLotriMat
#> <pointer: 0x7ff1896632e0>
#> 
#> $lotriSep
#> <pointer: 0x7ff189666e30>
#> 
#> $lotriAllNames
#> <pointer: 0x7ff1896673d0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7ff189663880>
#> 
#> $lotriMaxNu
#> <pointer: 0x7ff1896668c0>
#> 
#> $isLotri
#> <pointer: 0x7ff1896672b0>
#> 
#> $lotriRcm
#> <pointer: 0x7ff1896717b0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7ff18966ca90>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7ff18966cfd0>
#> 
```
