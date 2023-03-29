#' @title qt_zscore
#'
#' @description
#' Calculates the Z-Score of the last value inside a numeric vector
#'
#'
#' @param x A numeric vector
#'
#' @return A numeric value
#'
#' @export
#'
qt_zscore <- function(x) {

  current_value <- tail(x, 1)


  mu <- tail(x,-1) %>%
    mean()

  sigma <- tail(x,-1) %>%
    sd()

  zscore <- (current_value - mu) / sigma

  return(zscore)


}
