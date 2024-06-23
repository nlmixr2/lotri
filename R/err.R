##' print errors from lotri parsing of ({}) expressions
##'
##' @param .env  environment that has the elements `.hasErr`, `.err`, and `.lines`
##' @return nothing
##' @author Matthew Fidler
##' @noRd
.printErr <- function(.env) {
  if (.env$.hasErr) {
    messageText <-
      "\033[1mlotri syntax error:\n=================================================================================\033[0m"
    for (i in seq_along(.env$.err)) {
      if (!is.null(.env$.err[[i]])) {
        messageText <-
          c(
            messageText,
            crayon::bold("lotri error:"),
            paste(paste("  ", strsplit(.env$.err[[i]], "\n")[[1]]), collapse="\n")
          )
      }
      messageText <- c(messageText, .env$.lines[i])
    }
    messageText <-
      c(
        messageText,
        "\033[1m=================================================================================\033[0m"
      )
    message(paste(messageText, collapse="\n"))
    stop("lotri syntax errors above", call. = FALSE)
  }
}
