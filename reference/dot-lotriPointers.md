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
#> <pointer: 0x7f812d6fd070>
#> 
#> $asLotriMat
#> <pointer: 0x7f812d6fb2a0>
#> 
#> $lotriSep
#> <pointer: 0x7f812d6fece0>
#> 
#> $lotriAllNames
#> <pointer: 0x7f812d6ff280>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f812d6fb7e0>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f812d6fe770>
#> 
#> $isLotri
#> <pointer: 0x7f812d6ff160>
#> 
#> $lotriRcm
#> <pointer: 0x7f812d709600>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f812d704840>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f812d704d80>
#> 
```
