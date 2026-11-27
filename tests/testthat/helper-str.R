# R-devel (4.7) changed `str()` output: a space now follows `=` and
# `:` before a nested `List`/`'data.frame'` header, and a classed
# object's header no longer starts with a space.  Print `str()` in the
# R-devel form on every R version so snapshots are stable.
.strStable <- function(x) {
  out <- utils::capture.output(utils::str(x))
  out <- gsub("([=:])(List of|'data.frame')", "\\1 \\2", out)
  out[1] <- sub("^ ", "", out[1])
  cat(out, sep = "\n")
}
