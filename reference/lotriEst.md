# Extract or remove lotri estimate data frame from lotri object

Extract or remove lotri estimate data frame from lotri object

## Usage

``` r
lotriEst(x, drop = FALSE)
```

## Arguments

- x:

  lotri object

- drop:

  boolean indicating if the lotri estimate should be dropped

## Value

data frame with estimates or NULL if there is not a data.frame attached

## Examples

``` r
fix1 <- lotri({
   a <- c(0, 1); backTransform("exp"); label("a label")
   b <- c(0, 1, 2)
   c <- fix(1)
   d <- fix(0, 1, 2)
   e <- c(0, 1, 2, fixed)
   f+g ~ c(1,
           0.5, 1)
 })

# Extract the attached lotri estimate data frame
lotriEst(fix1)
#>   name lower est upper   fix   label backTransform
#> 1    a     0   1   Inf FALSE a label           exp
#> 2    b     0   1     2 FALSE    <NA>          <NA>
#> 3    c  -Inf   1   Inf  TRUE    <NA>          <NA>
#> 4    d     0   1     2  TRUE    <NA>          <NA>
#> 5    e     0   1     2  TRUE    <NA>          <NA>

# Remove the attached lotri estimate data frame
lotriEst(fix1, drop=TRUE)
#>     f   g
#> f 1.0 0.5
#> g 0.5 1.0
```
