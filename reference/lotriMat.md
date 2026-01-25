# Create a matrix from a list of matrices

This creates a named banded symmetric matrix from a list of named
symmetric matrices.

## Usage

``` r
lotriMat(matList, format = NULL, start = 1L)
```

## Arguments

- matList:

  list of symmetric named matrices

- format:

  The format of dimension names when a sub-matrix is repeated. The
  format will be called with the dimension number, so "ETA\[%d\]" would
  represent "ETA\[1\]", "ETA\[2\]", etc

- start:

  The number the counter of each repeated dimension should start.

## Value

Named symmetric block diagonal matrix based on concatenating the list of
matrices together

## Author

Matthew Fidler

## Examples

``` r
testList <- list(lotri({et2 + et3 + et4 ~ c(40,
                           0.1, 20,
                           0.1, 0.1, 30)}),
                 lotri(et5 ~ 6))

testList
#> [[1]]
#>      et2  et3  et4
#> et2 40.0  0.1  0.1
#> et3  0.1 20.0  0.1
#> et4  0.1  0.1 30.0
#> 
#> [[2]]
#>     et5
#> et5   6
#> 

lotriMat(testList)
#>      et2  et3  et4 et5
#> et2 40.0  0.1  0.1   0
#> et3  0.1 20.0  0.1   0
#> et4  0.1  0.1 30.0   0
#> et5  0.0  0.0  0.0   6


# Another option is to repeat a matrix a number of times.  This
# can be done with list(matrix, # times to repeat).

# In the example below, the first matrix is repeated 3 times
testList <- list(list(lotri({et2 + et3 + et4 ~ c(40,
                           0.1, 20,
                           0.1, 0.1, 30)}), 3),
                 lotri(et5 ~ 6))

lotriMat(testList)
#>      et2  et3  et4  et2  et3  et4  et2  et3  et4 et5
#> et2 40.0  0.1  0.1  0.0  0.0  0.0  0.0  0.0  0.0   0
#> et3  0.1 20.0  0.1  0.0  0.0  0.0  0.0  0.0  0.0   0
#> et4  0.1  0.1 30.0  0.0  0.0  0.0  0.0  0.0  0.0   0
#> et2  0.0  0.0  0.0 40.0  0.1  0.1  0.0  0.0  0.0   0
#> et3  0.0  0.0  0.0  0.1 20.0  0.1  0.0  0.0  0.0   0
#> et4  0.0  0.0  0.0  0.1  0.1 30.0  0.0  0.0  0.0   0
#> et2  0.0  0.0  0.0  0.0  0.0  0.0 40.0  0.1  0.1   0
#> et3  0.0  0.0  0.0  0.0  0.0  0.0  0.1 20.0  0.1   0
#> et4  0.0  0.0  0.0  0.0  0.0  0.0  0.1  0.1 30.0   0
#> et5  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   6

# Notice that the dimension names `et2`, `et3` and `et4` are
# repeated.

# Another option is to name the dimensions.  For example it could
# be `ETA[1]`, `ETA[2]`, etc by using the 'format' option:

lotriMat(testList, "ETA[%d]")
#>        ETA[1] ETA[2] ETA[3] ETA[4] ETA[5] ETA[6] ETA[7] ETA[8] ETA[9] et5
#> ETA[1]   40.0    0.1    0.1    0.0    0.0    0.0    0.0    0.0    0.0   0
#> ETA[2]    0.1   20.0    0.1    0.0    0.0    0.0    0.0    0.0    0.0   0
#> ETA[3]    0.1    0.1   30.0    0.0    0.0    0.0    0.0    0.0    0.0   0
#> ETA[4]    0.0    0.0    0.0   40.0    0.1    0.1    0.0    0.0    0.0   0
#> ETA[5]    0.0    0.0    0.0    0.1   20.0    0.1    0.0    0.0    0.0   0
#> ETA[6]    0.0    0.0    0.0    0.1    0.1   30.0    0.0    0.0    0.0   0
#> ETA[7]    0.0    0.0    0.0    0.0    0.0    0.0   40.0    0.1    0.1   0
#> ETA[8]    0.0    0.0    0.0    0.0    0.0    0.0    0.1   20.0    0.1   0
#> ETA[9]    0.0    0.0    0.0    0.0    0.0    0.0    0.1    0.1   30.0   0
#> et5       0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0   6

# Or could start with ETA[2]:

lotriMat(testList, "ETA[%d]", 2)
#>         ETA[2] ETA[3] ETA[4] ETA[5] ETA[6] ETA[7] ETA[8] ETA[9] ETA[10] et5
#> ETA[2]    40.0    0.1    0.1    0.0    0.0    0.0    0.0    0.0     0.0   0
#> ETA[3]     0.1   20.0    0.1    0.0    0.0    0.0    0.0    0.0     0.0   0
#> ETA[4]     0.1    0.1   30.0    0.0    0.0    0.0    0.0    0.0     0.0   0
#> ETA[5]     0.0    0.0    0.0   40.0    0.1    0.1    0.0    0.0     0.0   0
#> ETA[6]     0.0    0.0    0.0    0.1   20.0    0.1    0.0    0.0     0.0   0
#> ETA[7]     0.0    0.0    0.0    0.1    0.1   30.0    0.0    0.0     0.0   0
#> ETA[8]     0.0    0.0    0.0    0.0    0.0    0.0   40.0    0.1     0.1   0
#> ETA[9]     0.0    0.0    0.0    0.0    0.0    0.0    0.1   20.0     0.1   0
#> ETA[10]    0.0    0.0    0.0    0.0    0.0    0.0    0.1    0.1    30.0   0
#> et5        0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0     0.0   6
```
