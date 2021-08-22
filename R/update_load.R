#' This function updates the namespace for loading into other rproj files.
#' @author Sucheen Sundaram
#' @import devtools
#' @import roxygen2
#' @export

update_load <- function(package = "Frost2021Package", install = FALSE) {
  path <- paste("/Users/sucheen/Documents/Cal Poly SLO/Academics/Summer 2021/", package, sep = "")
  devtools::document(path)
  roxygen2::roxygenise(path)
  git(package = package, message = "Namespace")
  if(install) {
    devtools::install_github("kingsuching/Frost2021Package", force = TRUE)
    q()
  }
}
