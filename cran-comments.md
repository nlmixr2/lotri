# lotri 1.0.5

## Why this submission

This release clears the NOTE lotri 1.0.4 shows on five r-devel check
flavours (debian-clang, debian-gcc, fedora-clang, fedora-gcc and
windows):

```
Found calls to structure() using deprecated special names
```

The test suite now uses `dim`, `dimnames` and `names` rather than
`.Dim`, `.Dimnames` and `.Names`; no call to `structure()` with a
deprecated special name remains in the package.

It also carries the C tidy-ups that cleared the earlier clang 23 check
WARNING: `src/nearPD.cpp` and `src/rcm.cpp` now name the `<algorithm>`,
`<cmath>` and `<limits>` headers they had been relying on
transitively, a `printf` format string was corrected, and the
dependency on 'armadillo4r' was raised to (>= 15.4.2), whose bundled
'Armadillo' headers were the actual source of that WARNING.

Development continued on lotri between 1.0.4 and now, so the version
that carries these fixes also carries the features listed below.

## Other changes in this version

* A `same()` keyword in the DSL, which is NONMEM's `$OMEGA BLOCK(n)
  SAME` -- one estimated covariance block shared by several named
  blocks, as inter-occasion variability is parameterized.
* Prior distribution specification in a `lotri({})` block.
* A number of parser bug fixes.

See NEWS.md for the full list.

## Test environments

* local: Ubuntu 24.04, R 4.6.1 -- `R CMD check --as-cran`
* GitHub Actions (`R-CMD-check`): ubuntu-latest (devel, release,
  oldrel-1), macOS-latest (release), windows-latest (release) -- all
  passing on the submitted commit.

## R CMD check results

0 errors | 0 warnings | 2 notes

Both notes are artifacts of the local machine rather than the package:

* `checking compilation flags used ... NOTE`
  `Compilation used the following non-portable flag(s):
  '-mno-omit-leaf-frame-pointer'` -- this flag comes from the Debian/
  Ubuntu R build's default `CFLAGS`, not from the package; `src/` has
  no `Makevars` flags of its own.

* `checking HTML version of manual ... NOTE` -- `tidy` is not
  installed on the local machine, so HTML validation was skipped.

## Reverse dependencies

lotri has 8 reverse dependencies on CRAN: rxode2, nlmixr2, nlmixr2est,
nlmixr2extra, babelmixr2, nonmem2rx, monolix2rx and posologyr.  All of
them are part of the nlmixr2 ecosystem, and I maintain them.

I expect all 8 to fail their checks against this version.  Only rxode2
and nlmixr2est break directly; the other six depend on those two and
fail through them.

To be clear about the cause: this is not a consequence of the WARNING
fix itself.  It is a consequence of that fix arriving on a newer lotri
that has more features in it than the version currently on CRAN --
rxode2 and nlmixr2est need updating to support those features.

Updated versions of rxode2 and nlmixr2est that support these changes
will be submitted to follow this one, which resolves the other six as
well.
