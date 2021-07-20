#' This function calculates a similarity score between two strings.
#' @param a, string A
#' @param b, string B
#' @return similarity score
#' @export

similarity_score <- function(a, b) {
  score_a <- 0
  score_b <- 0
  for(i in 1:length(a)) {
    if(a[i] %in% b) {
      score_a <- score_a + (1/length(b))
    }
  }
  for(i in 1:length(b)) {
    if(b[i] %in% a) {
      score_b <- score_b + (1/length(a))
    }
  }
  return(mean(score_a, score_b)*100)
}
