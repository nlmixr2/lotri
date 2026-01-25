# Converts a matrix into a list of block matrices

Converts a matrix into a list of block matrices

## Usage

``` r
lotriMatInv(mat)
```

## Arguments

- mat:

  Matrix to convert to a list of block matrices

## Value

A list of block matrixes

## Details

This is the inverse of \`lotriMat()\`

## Author

Matthew Fidler

## Examples

``` r
# Create a block matrix using `lotri()`
mat <- lotri({
   a+b ~ c(1,
           0.5, 1)
   c ~ 1
   d +e ~ c(1,
            0.5, 1)
})

print(mat)
#>     a   b c   d   e
#> a 1.0 0.5 0 0.0 0.0
#> b 0.5 1.0 0 0.0 0.0
#> c 0.0 0.0 1 0.0 0.0
#> d 0.0 0.0 0 1.0 0.5
#> e 0.0 0.0 0 0.5 1.0

# now convert t a list of matrices

mat2 <- lotriMatInv(mat)
print(mat2)
#> [[1]]
#>     a   b
#> a 1.0 0.5
#> b 0.5 1.0
#> 
#> [[2]]
#>   c
#> c 1
#> 
#> [[3]]
#>     d   e
#> d 1.0 0.5
#> e 0.5 1.0
#> 

# Of course you can convert it back to a full matrix:

mat3 <- lotriMat(mat2)

print(mat3)
#>     a   b c   d   e
#> a 1.0 0.5 0 0.0 0.0
#> b 0.5 1.0 0 0.0 0.0
#> c 0.0 0.0 1 0.0 0.0
#> d 0.0 0.0 0 1.0 0.5
#> e 0.0 0.0 0 0.5 1.0
```
