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
#> <pointer: 0x7fde9acef140>
#> 
#> $asLotriMat
#> <pointer: 0x7fde9aced2e0>
#> 
#> $lotriSep
#> <pointer: 0x7fde9acf0e30>
#> 
#> $lotriAllNames
#> <pointer: 0x7fde9acf13d0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7fde9aced880>
#> 
#> $lotriMaxNu
#> <pointer: 0x7fde9acf08c0>
#> 
#> $isLotri
#> <pointer: 0x7fde9acf12b0>
#> 
#> $lotriRcm
#> <pointer: 0x7fde9acfb7b0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7fde9acf6a90>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7fde9acf6fd0>
#> 
```
