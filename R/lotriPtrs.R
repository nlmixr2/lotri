#' This function is used to get the pointers to the C objects that are used in the lotri package.
#'
#'
#' @return A list of function pointers
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#'
#' .lotriPointers()
.lotriPointers <- function() {
  .Call(`_getLotriPointers`, PACKAGE = "lotri")
}
