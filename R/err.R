##' print errors from lotri parsing of ({}) expressions
##'
##' @param .env  environment that has the elements `.hasErr`, `.err`, and `.lines`
##' @return nothing
##' @author Matthew Fidler
##' @noRd
.printErr <- function(.env) {
  if (.env$.hasErr) {
    message("\033[1mlotri syntax error:\n================================================================================\033[0m")
    for (i in seq_along(.env$.err)) {
      if (!is.null(.env$.err[[i]])) {
        message(crayon::bold("lotri error:"))
        .err <- paste(paste("  ", strsplit(.env$.err[[i]], "\n")[[1]]), collapse="\n")
        message(.err)
      }
      message(.env$.lines[i])
    }
    message("\033[1m================================================================================\033[0m")
    ## message(paste(.env$.lines, collapse="\n"))
    stop("lotri syntax errors above", call. = FALSE)
  }
}
