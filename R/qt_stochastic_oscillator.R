#' @title qt_stochastic_oscillator
#'
#' @description
#' Returns the stochastic oscillator calculation of the last value of a numeric vector
#'
#'
#' @param x A numeric vector
#'
#' @return A numeric value
#'
#' @export
#'
#'

qt_stochastic_oscillator <- function(x) {

  low <- min(x)

  high <- max(x)

  close <- tail(x,1)

  oscillator <- (close - low)/(high - low)

  return(oscillator)

}
