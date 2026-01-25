# Change a matrix or lotri matrix to a lotri expression

Change a matrix or lotri matrix to a lotri expression

## Usage

``` r
lotriAsExpression(
  x,
  useIni = FALSE,
  plusNames = getOption("lotri.plusNames", FALSE),
  nameEst = getOption("lotri.nameEst", 5L)
)
```

## Arguments

- x:

  matrix

- useIni:

  use the ini block

- plusNames:

  logical, when \`TRUE\` use the \`a + b ~ c(1, 0.1, 1)\` naming
  convention. Otherwise use the lotri single line convention \`a ~ 1; b
  ~ c(0.1, 1)\`

- nameEst:

  logical or integerish. When logical \`TRUE\` will add names to all
  matrix estimates and \`TRUE\` when using the lotri single line
  convention i.e. \`a~c(a=1); b~c(a=0.1, b=1)\`. When an integer, the
  dimension of the matrix being displayed needs to have a dimension
  above this number before names are displayed.
