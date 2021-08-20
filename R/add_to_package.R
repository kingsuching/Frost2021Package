#' This function adds code to the project R package.
#' @import rvest
#' @import stringr
#' @import devtools
#' @param file, the file to be added
#' @export

add_to_package <- function(file) {
  Frost2021Package::copy(file)
  system("cd ..; cd Frost2021Package; git add .")
  Frost2021Package::git(message = paste("Added", file))
}
