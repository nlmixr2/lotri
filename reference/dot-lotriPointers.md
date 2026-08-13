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
#> <pointer: 0x7fa6143770b0>
#> 
#> $asLotriMat
#> <pointer: 0x7fa614375280>
#> 
#> $lotriSep
#> <pointer: 0x7fa614378c90>
#> 
#> $lotriAllNames
#> <pointer: 0x7fa614379240>
#> 
#> $lotriGetBounds
#> <pointer: 0x7fa614375930>
#> 
#> $lotriMaxNu
#> <pointer: 0x7fa614378720>
#> 
#> $isLotri
#> <pointer: 0x7fa614379110>
#> 
#> $lotriRcm
#> <pointer: 0x7fa614383540>
#> 
#> $lotriNearPDc
#> <pointer: 0x7fa61437e7c0>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7fa61437ed00>
#> 
```
