# Convert a lotri data frame to a lotri expression

Convert a lotri data frame to a lotri expression

## Usage

``` r
lotriDataFrameToLotriExpression(data, useIni = FALSE)
```

## Arguments

- data:

  lotri data frame

- useIni:

  Use \`ini\` instead of \`lotri\` in the expression

## Value

expression of the lotri syntax equivalent to the data.frame provided

## Author

Matthew L. Fidler

## Examples

``` r
 x <- lotri({
  tka <- 0.45; label("Log Ka")
  tcl <- 1; label("Log Cl")
  tv <- 3.45; label("Log V")
  eta.ka ~ 0.6
  eta.cl ~ 0.3
  eta.v ~ 0.1
  add.err <- 0.7
})

df <- as.data.frame(x)

lotriDataFrameToLotriExpression(df)
#> lotri({
#>     tka <- 0.45
#>     label("Log Ka")
#>     tcl <- 1
#>     label("Log Cl")
#>     tv <- 3.45
#>     label("Log V")
#>     add.err <- 0.7
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })

# You may also call as.expression directly from the lotri object

as.expression(x)
#> lotri({
#>     tka <- 0.45
#>     label("Log Ka")
#>     tcl <- 1
#>     label("Log Cl")
#>     tv <- 3.45
#>     label("Log V")
#>     add.err <- 0.7
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })
```
