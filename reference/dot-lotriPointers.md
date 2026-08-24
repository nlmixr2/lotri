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
#> <pointer: 0x7f4e30bc78d0>
#> 
#> $asLotriMat
#> <pointer: 0x7f4e30bc55c0>
#> 
#> $lotriSep
#> <pointer: 0x7f4e30bc9710>
#> 
#> $lotriAllNames
#> <pointer: 0x7f4e30bc9cc0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f4e30bc5c70>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f4e30bc91a0>
#> 
#> $isLotri
#> <pointer: 0x7f4e30bc9b90>
#> 
#> $lotriRcm
#> <pointer: 0x7f4e30bd42b0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f4e30bcedd0>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f4e30bcf7e0>
#> 
```
