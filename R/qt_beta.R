#' @title qt_beta
#' @description
#' Calculates the beta of two price time series.
#'
#'
#' @param asset_1 Independant Variable
#' @param asset_2 Dependant Variable
#'
#' @return
#'

qt_beta <- function(asset_1, asset_2) {

  data <- tibble(yaxis = asset_2,
                 xaxis = asset_1) %>%
    mutate(across(.fns = ~log(.x / lag(.x)))
    ) %>%
    na.omit()


  fit <- lm(yaxis ~ xaxis, data)

  tidy(fit) %>%
    filter(term == "xaxis") %>%
    select(estimate) %>%
    pull()



}
