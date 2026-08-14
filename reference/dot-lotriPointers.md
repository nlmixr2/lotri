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
#> <pointer: 0x7f313d49c590>
#> 
#> $asLotriMat
#> <pointer: 0x7f313d49a280>
#> 
#> $lotriSep
#> <pointer: 0x7f313d49e3d0>
#> 
#> $lotriAllNames
#> <pointer: 0x7f313d49e980>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f313d49a930>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f313d49de60>
#> 
#> $isLotri
#> <pointer: 0x7f313d49e850>
#> 
#> $lotriRcm
#> <pointer: 0x7f313d4a8c80>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f313d4a3f00>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f313d4a4440>
#> 
```
