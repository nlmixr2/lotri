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
#> <pointer: 0x7fbd28865140>
#> 
#> $asLotriMat
#> <pointer: 0x7fbd288632e0>
#> 
#> $lotriSep
#> <pointer: 0x7fbd28866e30>
#> 
#> $lotriAllNames
#> <pointer: 0x7fbd288673d0>
#> 
#> $lotriGetBounds
#> <pointer: 0x7fbd28863880>
#> 
#> $lotriMaxNu
#> <pointer: 0x7fbd288668c0>
#> 
#> $isLotri
#> <pointer: 0x7fbd288672b0>
#> 
#> $lotriRcm
#> <pointer: 0x7fbd288717b0>
#> 
#> $lotriNearPDc
#> <pointer: 0x7fbd2886ca90>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7fbd2886cfd0>
#> 
```
