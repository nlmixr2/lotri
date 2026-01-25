# Separate a lotri matrix into above and below lotri matrices

This is used for creating nesting simulations in \`rxode2()\` and may
not be useful for external function calls.

## Usage

``` r
lotriSep(x, above, below, aboveStart = 1L, belowStart = 1L)
```

## Arguments

- x:

  lotri matrix

- above:

  Named integer vector listing variability above the id level. Each
  element lists the number of population differences in the whole
  data-set (as integer)

- below:

  Named integer vector listing variability below the id level. Each
  element lists the number of items below the individual level. For
  example with 3 occasions per individual you could use 'c(occ=3L)'

- aboveStart:

  Add the attribute of where THETA\[#\] will be added

- belowStart:

  Add the attribute of where ETA\[#\] will be added

## Value

List of two lotri matrices

## Author

Matthew Fidler

## Examples

``` r
omega <- lotri(lotri(eta.Cl ~ 0.1,
                        eta.Ka ~ 0.1) | id(nu=100),
                  lotri(eye.Cl ~ 0.05,
                        eye.Ka ~ 0.05) | eye(nu=50),
                  lotri(iov.Cl ~ 0.01,
                        iov.Ka ~ 0.01) | occ(nu=200),
                  lotri(inv.Cl ~ 0.02,
                        inv.Ka ~ 0.02) | inv(nu=10))

lotriSep(omega, above=c(inv=10L), below=c(eye=2L, occ=4L))
#> $above
#> $inv
#>        inv.Cl inv.Ka
#> inv.Cl   0.02   0.00
#> inv.Ka   0.00   0.02
#> 
#> attr(,"format")
#> [1] "THETA[%d]"
#> attr(,"start")
#> [1] 1
#> Properties: nu, same 
#> 
#> $below
#> $id
#>        eta.Cl eta.Ka
#> eta.Cl    0.1    0.0
#> eta.Ka    0.0    0.1
#> 
#> $eye
#>        eye.Cl eye.Ka
#> eye.Cl   0.05   0.00
#> eye.Ka   0.00   0.05
#> 
#> $occ
#>        iov.Cl iov.Ka
#> iov.Cl   0.01   0.00
#> iov.Ka   0.00   0.01
#> 
#> attr(,"format")
#> [1] "ETA[%d]"
#> attr(,"start")
#> [1] 1
#> Properties: nu, same 
#> 
```
