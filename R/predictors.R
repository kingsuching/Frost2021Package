#' Generates the predictors
#'
#' @import tidyverse
#' @import dplyr
#' @import broom
#' @param df dataframe
#' @return Sum of squares for the variable that explains the most variation in the dataset
#' @export predictors

predictors <- function(df, response) {
  df <- df %>% select_if(is.numeric)
  old <- response
  response <- str_replace_all(response, " ", ".")
  names(df)[names(df) == old] == response
  final <- c()
  names(df) <- make.names(names(df))
  for(i in names(df)) {
    if(i != response) {
      f <- paste(response, "~", i)
      m <- lm(formula = f, data = df) %>% anova() %>% pluck("Sum Sq")
      final <- append(final, m[1])
    }
  }
  max_index <- index(final, max(final))
  value <- names(df)[max_index]
  return(c(value, max(final)))
}
