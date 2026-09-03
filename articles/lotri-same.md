# Repeated blocks with same() in lotri

## What `same()` is for

NONMEM can declare an omega block and then repeat it:

    $OMEGA BLOCK(2)
     0.1
     0.01 0.2
    $OMEGA BLOCK(2) SAME
    $OMEGA BLOCK(2) SAME

Those three blocks are not three covariance matrices. They are *one*
estimated covariance stamped three times. That is how inter-occasion
variability is parameterized: every occasion draws its own random
effects, but all occasions share one covariance – and sharing one
covariance is what lets the per-occasion random effects be
**correlated**.

`same()` is the `lotri` spelling of that:

``` r

iov <- lotri({
  iov.cl1 + iov.v1 ~ c(0.1,
                       0.01, 0.2)
  iov.cl2 + iov.v2 ~ same()
  iov.cl3 + iov.v3 ~ same()
})

iov
#>         iov.cl1 iov.v1 iov.cl2 iov.v2 iov.cl3 iov.v3
#> iov.cl1    0.10   0.01    0.00   0.00    0.00   0.00
#> iov.v1     0.01   0.20    0.00   0.00    0.00   0.00
#> iov.cl2    0.00   0.00    0.10   0.01    0.00   0.00
#> iov.v2     0.00   0.00    0.01   0.20    0.00   0.00
#> iov.cl3    0.00   0.00    0.00   0.00    0.10   0.01
#> iov.v3     0.00   0.00    0.00   0.00    0.01   0.20
#> 
#> This matrix repeats blocks with `same()`:
#>   iov.cl2, iov.v2 repeat iov.cl1, iov.v1
#>   iov.cl3, iov.v3 repeat iov.cl1, iov.v1
```

Three 2x2 blocks, one estimated 2x2:

``` r

dim(iov)
#> [1] 6 6
```

## The rules

`same()` repeats the immediately preceding **block**, under new names,
and takes no arguments. A second `same()` repeats that same original
block rather than the previous copy, the way NONMEM chains `SAME`.

It repeats the block *as declared*. A structural zero in the covariance
does not split the block in two:

``` r

lotri({
  a + b ~ c(1,
            0, 1)
  c1 + d1 ~ same()
})
#>    a b c1 d1
#> a  1 0  0  0
#> b  0 1  0  0
#> c1 0 0  1  0
#> d1 0 0  0  1
#> 
#> This matrix repeats blocks with `same()`:
#>   c1, d1 repeat a, b
```

A repeated block inherits the fixed flags of the block it repeats, works
under a condition, and works for a single parameter:

``` r

lotri({
  eta.ka ~ 0.6
  iov.cl1 + iov.v1 ~ c(0.1,
                       0.01, 0.2) | occ
  iov.cl2 + iov.v2 ~ same() | occ
})
#> $id
#>        eta.ka
#> eta.ka    0.6
#> 
#> $occ
#>         iov.cl1 iov.v1 iov.cl2 iov.v2
#> iov.cl1    0.10   0.01    0.00   0.00
#> iov.v1     0.01   0.20    0.00   0.00
#> iov.cl2    0.00   0.00    0.10   0.01
#> iov.v2     0.00   0.00    0.01   0.20
#> 
#> This matrix repeats blocks with `same()`:
#>   iov.cl2, iov.v2 repeat iov.cl1, iov.v1
```

A prior cannot be put on a repeated block. A copy is not a parameter of
its own – it *is* the block it mirrors – so a prior on it would either
duplicate the master’s prior or silently contradict it:

``` r

lotri({
  a + b ~ c(1,
            0.1, 2)
  c1 + d1 ~ same()
  prior(c1) ~ dnorm(0, 1)
})
#> Error:
#> ! 'c1' repeats 'a' with 'same()', so it cannot carry its own prior; put the prior on 'a'
```

Put it on the block that is actually estimated
(`prior(a) ~ dnorm(0, 1)`).

`same()` looks back only within one
[`{}`](https://rdrr.io/r/base/Paren.html) block, and only at its own
level of variability. Each extra argument to
[`lotri()`](https://nlmixr2.github.io/lotri/reference/lotri.md) is
parsed by its own call, so this has nothing to repeat:

``` r

lotri(a + b ~ c(1, 0.1, 2), c1 + d1 ~ same())
#> Error:
#> ! 'same()' has no block to repeat; it must follow a matrix block in the same '{}' block
```

Write the two lines in one `lotri({})` block instead.

`same()` is refused with `rcm=TRUE` and with a `cov` function. Both
reorder or adjust the whole matrix, which would move a repeated block
away from the block it is supposed to repeat; erroring is better than
returning a matrix whose claimed repetition is no longer true.

## How it is stored: the `condition` column

This is the part other packages need to know.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) does
**not** gain a column for `same()`. The repetition rides in the existing
`condition` column, which names the element that is mirrored:

``` r

df <- as.data.frame(iov)
df[, c("neta1", "neta2", "name", "est", "condition")]
#>   neta1 neta2             name  est              condition
#> 1     1     1          iov.cl1 0.10                     id
#> 2     2     1 (iov.cl1,iov.v1) 0.01                     id
#> 3     2     2           iov.v1 0.20                     id
#> 4     3     3          iov.cl2 0.10        id:same:iov.cl1
#> 5     4     3 (iov.cl2,iov.v2) 0.01 id:same:iov.cl1:iov.v1
#> 6     4     4           iov.v2 0.20         id:same:iov.v1
#> 7     5     5          iov.cl3 0.10        id:same:iov.cl1
#> 8     6     5 (iov.cl3,iov.v3) 0.01 id:same:iov.cl1:iov.v1
#> 9     6     6           iov.v3 0.20         id:same:iov.v1
```

The format is

    <baseCondition>:same:<masterEta>                 # diagonal row
    <baseCondition>:same:<masterEta1>:<masterEta2>   # covariance row

Two things are worth spelling out.

**The master is named, not indexed.** `neta1`/`neta2` are renumbered
whenever parameters are added, dropped or reordered, so an index would
go stale silently. Names are unique within one of these data frames and
survive reordering.

**Covariance rows are carried too.** The off-diagonal row above points
at the master’s off-diagonal, so the *correlation* is shared, not just
the variances.

A repeated row keeps its master’s `est` and `fix`. A consumer that knows
nothing about the suffix therefore still reads a numerically correct
matrix – it just thinks the model has more free parameters than it does.

## Reading the column

Do not compare `condition` directly. A test like `condition == "id"`
misses every repeated row, and `condition != "id"` wrongly treats one as
a different level of variability. Use these instead:

``` r

lotriBaseCondition(df$condition)
#> [1] "id" "id" "id" "id" "id" "id" "id" "id" "id"

lotriIsSame(df$condition)
#> [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
```

[`lotriSameMap()`](https://nlmixr2.github.io/lotri/reference/lotriBaseCondition.md)
gives, for each eta, the index of the eta it mirrors (`0` when it is an
ordinary or master eta). This is the object an estimator wants, since it
says which parameters are actually free:

``` r

lotriSameMap(df)
#> [1] 0 0 1 2 1 2
```

When an edit structurally changes a block – deleting one of its
parameters, say – the linkage no longer describes anything real.
[`lotriSameBreak()`](https://nlmixr2.github.io/lotri/reference/lotriBaseCondition.md)
drops it, turning the copies into ordinary independent blocks and
leaving every other family alone:

``` r

broken <- lotriSameBreak(df, "iov.v2")
broken$condition
#> [1] "id" "id" "id" "id" "id" "id" "id" "id" "id"
```

## Round trips

`same()` survives every direction:

``` r

identical(as.data.frame(as.lotri(df)), df)
#> [1] TRUE

identical(as.data.frame(eval(as.expression(iov))), df)
#> [1] TRUE

as.expression(iov)
#> lotri({
#>     iov.cl1 ~ 0.1
#>     iov.v1 ~ c(0.01, 0.2)
#>     iov.cl2 + iov.v2 ~ same()
#>     iov.cl3 + iov.v3 ~ same()
#> })
```

and the linkage is carried across
[`lotriMat()`](https://nlmixr2.github.io/lotri/reference/lotriMat.md),
so combining blocks does not silently turn a repeated block back into
independently estimated parameters:

``` r

attr(lotriMat(lotriMatInv(iov)), "lotriSame")
#> [1] 0 0 2 2 4 4
```

## `same()` versus `cnd(same = n)`

There are two spellings of the same NONMEM idea at different
granularities, and they compose.

- `~ same()` repeats **one block** inside a matrix, under names you
  choose. This is `$OMEGA BLOCK(n) SAME`.
- `cnd(same = n)` repeats a **whole nesting level** `n` times, and is
  what
  [`lotriSep()`](https://nlmixr2.github.io/lotri/reference/lotriSep.md)
  sets up for `rxode2`’s nested simulation.

Used together, a two-parameter correlated occasion block declared with
`same()` can itself be stamped once per occasion:

``` r

nested <- lotri(lotri({
  a + b ~ c(1,
            0.1, 2)
  c1 + d1 ~ same()
}) | occ(same = 3L))

dim(lotriMat(nested, format = "ETA[%d]", start = 1L))
#> [1] 12 12
```
