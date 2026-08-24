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
#> <pointer: 0x7f639482e8d0>
#> 
#> $asLotriMat
#> <pointer: 0x7f639482c5c0>
#> 
#> $lotriSep
#> <pointer: 0x7f6394830710>
#> 
#> $lotriAllNames
#> <pointer: 0x7f6394830cc0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f639482cc70>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f63948301a0>
#> 
#> $isLotri
#> <pointer: 0x7f6394830b90>
#> 
#> $lotriRcm
#> <pointer: 0x7f639483b2b0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f6394835dd0>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f63948367e0>
#> 
```
