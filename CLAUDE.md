# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Overview

**lotri** provides a domain specific language for building symmetric,
block diagonal matrices – the omega and sigma matrices of the nlmixr2
ecosystem. It is mixed R and C/C++: the DSL is parsed in R
(`R/lotri.R`), while block concatenation and the nesting/repeat
machinery live in C (`src/lotriLstToMat.*`, `src/matlist.*`,
`src/lotriNest.*`).

## Build and Development Commands

``` sh
R CMD INSTALL .                       # install
Rscript -e 'devtools::document()'     # regenerate man/ and NAMESPACE
Rscript -e 'devtools::test()'         # run the test suite
R CMD build . && R CMD check --as-cran lotri_*.tar.gz
```

Set `NOT_CRAN=true` when running the suite locally, or the randomized
property tests are skipped.

After changing anything in `src/`, remove the stale objects
(`rm -f src/*.o src/*.so`) before reinstalling; `make` will otherwise
report “Nothing to be done” and quietly keep the old C.

## Key conventions

- **Never use `:::` in a test file.** testthat sources test files inside
  the package’s own namespace, so both exported and non-exported
  (`.foo`) internal functions are directly callable by name –
  `lotri:::.lotriSameSplit(...)` should just be `.lotriSameSplit(...)`,
  and `lotri:::.lotriGetMatrixFromEnv(...)` should just be
  `.lotriGetMatrixFromEnv(...)`. `:::` is unnecessary and CodeFactor’s
  `lintr-undesirable_operator_linter` flags it on every PR that
  introduces one.

- **A new matrix attribute must not be a prefix of, or prefixed by, an
  existing one.** [`attr()`](https://rdrr.io/r/base/attr.html) partial
  matches by default, so adding `lotriSame` alongside
  `lotriLabels`/`lotriFix`/`lotriPriors` made `attr(x, "lotri")`
  ambiguous. Read the condition property list with
  `attr(x, "lotri", exact=TRUE)`.

- **Setting any `lotri*` attribute requires the `lotriFix` class.**
  Without `class(x) <- c("lotriFix", class(x))`,
  [`print()`](https://rdrr.io/r/base/print.html),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`as.expression()`](https://rdrr.io/r/base/expression.html) dispatch
  to the default methods and the attribute is silently invisible.

- **The parser’s row counters are load bearing.** `env$eta1` means
  different things in the plus form (the block’s base) and the line form
  (the block’s first row, with `env$lastN` counting its rows). A branch
  that opens or extends a block must settle a preceding line-form block
  with `.resetLastN()` first, or it writes on top of it. Several silent
  wrong-matrix bugs have come from exactly this.

- **Check a candidate parser bug against the base commit before treating
  it as a regression.** Install the unmodified package to a separate
  library (`R CMD INSTALL -l /tmp/mainlib ~/src/lotri`) and compare; a
  good deal of surprising behaviour is long standing.
