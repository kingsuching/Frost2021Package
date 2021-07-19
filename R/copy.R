#' This function copies and moves files
#' @param filename, the file to be copied
#' @param path, the destination path
#' @export

copy <- function(filename, path = "/Users/sucheen/Documents/Cal Poly SLO/Academics/Summer 2021/Frost2021Package/R/") {
  file.copy(filename, to = path)
}
