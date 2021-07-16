#' This function updates the namespace for loading into other rproj files.
#' @author Sucheen Sundaram
#' @import devtools
#' @import roxygen2
#' @export

update_load <- function() {
  devtools::document()
  roxygen2::roxygenise()
}
