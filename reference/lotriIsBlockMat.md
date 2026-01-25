# Determine if the matrix is a block matrix

Determine if the matrix is a block matrix

## Usage

``` r
lotriIsBlockMat(mat)
```

## Arguments

- mat:

  matrix to determine if it is a block matrix

## Value

logical value, TRUE if it is a block matrix and FALSE otherwise

## Author

Matthew L. Fidler

## Examples

``` r
m <- lotri({
  a ~ c(a = 0.4)
  b ~ c(a = 0, b = 0.3)
  c ~ c(a = 0, b = 0, c = 0)
  d ~ c(a = -0.1, b = 0, c = 0, d = 0.2)
  e ~ c(a = 0, b = 0, c = 0, d = 0, e = 0.5)
  f ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 1.3)
  g ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = -0.6, g = 0.8)
  h ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0)
  i ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
        i = 0.2)
  j ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
        i = 0, j = 0.9)
  k ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
        i = 0, j = 0, k = 0.9)
  l ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
        i = 0, j = -0.2, k = 0, l = 0.3)
  m ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
        i = 0, j = 0, k = 0, l = 0, m = 2.1)
  n ~ c(a = 0.2, b = 0, c = 0, d = 0.2, e = 0, f = 0, g = 0,
        h = 0, i = 0, j = 0, k = 0, l = 0, m = 0, n = 0.4)
  o ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = -1.1, g = 0.9,
        h = 0, i = 0, j = 0, k = 0, l = 0, m = 0, n = 0, o = 4.7)
  p ~ c(a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0,
        i = 0, j = 0.5, k = 0, l = 0.2, m = 0, n = 0, o = 0,
       p = 1.9)
})

lotriIsBlockMat(m)
#> [1] FALSE

lotriIsBlockMat(rcm(m))
#> [1] TRUE
```
