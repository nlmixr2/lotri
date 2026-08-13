# Return the prior distributions supported by \`lotri()\`

This is the table used to validate the \`prior()\` statements in a
\`lotri()\`/\`ini()\` block. It is exported so that downstream packages
(like 'nlmixr2est') can translate a stored prior into the 'Stan'
spelling when generating a model.

## Usage

``` r
lotriPriorDists()
```

## Value

data frame with columns \`rName\`, \`stanName\`, \`name\`, \`parNames\`,
\`nPar\`, \`support\` and \`kind\`

## Details

The \`name\` column is the canonical name that \`lotri()\` stores. It is
the R spelling (\`rName\`) whenever R parameterizes the distribution the
same way 'Stan' does, and the 'Stan' spelling (\`stanName\`) otherwise.

## Author

Matthew L. Fidler

## Examples

``` r

head(lotriPriorDists())
#>     rName       stanName    camelName         name        parNames nPar nReq
#> 1   dnorm         normal       normal        dnorm         mean,sd    2    2
#> 2    <NA>     std_normal    stdNormal    stdNormal                    0    0
#> 3    <NA> exp_mod_normal expModNormal expModNormal mu,sigma,lambda    3    3
#> 4    <NA>    skew_normal   skewNormal   skewNormal  xi,omega,alpha    3    3
#> 5    <NA>      student_t     studentT     studentT     nu,mu,sigma    3    3
#> 6 dcauchy         cauchy       cauchy      dcauchy  location,scale    2    2
#>   support       kind
#> 1    real univariate
#> 2    real univariate
#> 3    real univariate
#> 4    real univariate
#> 5    real univariate
#> 6    real univariate
```
