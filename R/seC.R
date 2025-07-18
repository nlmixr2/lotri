.seCArithmeticOperators <- function(x) {
  if (identical(x[[1]], quote(`^`)) ||
        identical(x[[1]], quote(`**`))) {
    return(paste0("pow(", .seC(x[[2]]), ", ", .seC(x[[3]]), ")"))
  }
  if (length(x) == 3) {
    paste0(
      .seC(x[[2]]),
      as.character(x[[1]]),
      .seC(x[[3]]))
  } else {
    ## Unary Operators
    return(paste(
      as.character(x[[1]]),
      .seC(x[[2]])
    ))
  }
}


.seC <- function(x) {
  if (is.name(x) || is.atomic(x)) {
    if (is.numeric(x)) return(paste(x))
    return(gsub("^t([0-9]+)$","_t[\\1]", as.character(x)))
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`(`))) {
      return(paste0("(", .seC(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`*`)) ||
                 identical(x[[1]], quote(`^`)) ||
                 identical(x[[1]], quote(`+`)) ||
                 identical(x[[1]], quote(`-`)) ||
                 identical(x[[1]], quote(`/`))) {
      return(.seCArithmeticOperators(x))
    } else {
      return(paste0(as.character(x[[1]]), "(",
                    paste(vapply(seq(2, length(x)),
                                 function(x) {
                                   .seC(x)
                                 }, character(1), USE.NAMES=FALSE),
                          collapse=", "), ")"))
    }

  }
}

seC <- function(x) {
  x <- as.character(x)
  .seC(str2lang(x))
}
