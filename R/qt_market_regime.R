#' @title qt_market_regime
#'
#' @param x A numeric vector of daily returns
#' @param days Period of days to calculate
#'
#' @return A string of the current market regime
#'
#' @export
#'


qt_market_regime <- function(x, days = 100 ) {

  mu <-  mean(x)

  sig <- sd(x)

  sqn <- (mu/sig) *sqrt(days)

  case_when(
    sqn < -.7 ~ "Bear Volatilie",
    sqn < 0   ~ "Bear Volatilie",
    sqn < .70 ~ "Neutral",
    sqn < 1.47 ~ "Bull Quiet",
    sqn > 1.47 ~ "Bull Volatile",
  )




}
