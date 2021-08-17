#' This function adds files to the git tracking list to solve the untracked files problem.
#' @param files, the files
#' @param repository, the repository
#' @param message, commit message
#' @export

add_and_commit <- function(files, repository = "Frost2021Package", message = "Working on it", commit = TRUE) {
  if(repository == "Frost2021Package") {
    system(paste("cd ..; cd", repository, "; cd R"))
  }else{
    system(paste("cd ..; cd", repository))
  }
  for(i in files) {
    system(paste("git add", i))
  }
  if(commit) {
    git(repository, message)
  }
}
