#' This function updates the namespace for loading into other rproj files.
#' @author Sucheen Sundaram
#' @import devtools
#' @import roxygen2
#' @param pkg
#' @param install
#' @export

update_load <- function(pkg = "Frost2021Package", install = FALSE) {
  path <- paste("/Users/sucheen/Documents/Cal Poly SLO/Academics/Summer 2021/", pkg, sep = "")
  devtools::document(path)
  roxygen2::roxygenise(path)
  Frost2021Package::git(repository = pkg, message = "Namespace")
  if(install) {
    devtools::install_github("kingsuching/Frost2021Package", force = TRUE)
    q()
  }
}
