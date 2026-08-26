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
#> <pointer: 0x7f41c2a3a8d0>
#> 
#> $asLotriMat
#> <pointer: 0x7f41c2a385c0>
#> 
#> $lotriSep
#> <pointer: 0x7f41c2a3c710>
#> 
#> $lotriAllNames
#> <pointer: 0x7f41c2a3ccc0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f41c2a38c70>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f41c2a3c1a0>
#> 
#> $isLotri
#> <pointer: 0x7f41c2a3cb90>
#> 
#> $lotriRcm
#> <pointer: 0x7f41c2a472b0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f41c2a41dd0>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f41c2a427e0>
#> 
```
