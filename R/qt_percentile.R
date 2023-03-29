
#' @title qt_percentile
#'
#' @description
#' Calculates the percentile of the last value isnide a numeric vector
#'
#'
#' @param x A numeric vector
#'
#' @return A numeric value
#'
#' @export
#'
#'

qt_percentile <- function(x) {

  x %>%
    cume_dist() %>%
    last(1)


}
