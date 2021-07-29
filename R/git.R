#' This function does the git commit and push process.
#' @param repository, the R package
#' @param message, commit message
#' @export

git <- function(repository = "Frost2021Package", message = "Working on it") {
  system(paste("cd ..; cd ", repository, "; git commit -am \"", message, "\"", "; git push", sep = ""))
}
