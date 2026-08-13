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
#> <pointer: 0x7f2c938ad590>
#> 
#> $asLotriMat
#> <pointer: 0x7f2c938ab280>
#> 
#> $lotriSep
#> <pointer: 0x7f2c938af3d0>
#> 
#> $lotriAllNames
#> <pointer: 0x7f2c938af980>
#> 
#> $lotriGetBounds
#> <pointer: 0x7f2c938ab930>
#> 
#> $lotriMaxNu
#> <pointer: 0x7f2c938aee60>
#> 
#> $isLotri
#> <pointer: 0x7f2c938af850>
#> 
#> $lotriRcm
#> <pointer: 0x7f2c938b9c80>
#> 
#> $lotriNearPDc
#> <pointer: 0x7f2c938b4f00>
#> 
#> $lotriNearPDsexp
#> <pointer: 0x7f2c938b5440>
#> 
```
