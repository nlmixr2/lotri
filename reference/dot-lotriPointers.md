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
#> <pointer: 0x7f30d5539140>
#> 
#> $asLotriMat
#> <pointer: 0x7f30d55372e0>
#> 
#> $lotriSep
#> <pointer: 0x7f30d553ae30>
#> 
#> $lotriAllNames
#> <pointer: 0x7f30d553b3d0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f30d5537880>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f30d553a8c0>
#> 
#> $isLotri
#> <pointer: 0x7f30d553b2b0>
#> 
#> $lotriRcm
#> <pointer: 0x7f30d55457b0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f30d5540a90>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f30d5540fd0>
#> 
```
