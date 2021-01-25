# Helper functions

#' Safely calculate a rate
#'
#' @param x A continuous variable acting as a numerator.
#' @param y A continuous variable acting as a denominator
#'
#' @return A vector of rates
#' @export
#'
#' @examples
#' rate(c(1, 2, NA, 4), c(5, NA, 6, 7))
#' 
rate <- function(x, y) {
  i <- !(is.na(x) & is.na(y))
  sum(x[i], na.rm=TRUE) / sum(y[i], na.rm=TRUE)
}

#' Generate a tidy dataframe that includes rates for selected variables
#'
#' @param grp A string of categorical grouping variable names
#' @param valuevars A string of continuous variables for which a rate will be
#'   calculated relative to population
#' @param query A list of filters, where the list element name is the name of
#'   the variable and the element value is a string of values who's membership
#'   should be included
#' @param data A dataframe for which we are filtering and calculating rate in
#'
#' @return A grouped dataframe
#' @export
#' @import dplyr rlang
#'
#' @examples
#' ob <- readr::read_csv(here::here("data", "processed", "obesity-combo.csv"))
#' grp <- c("sex", "country")
#' valuevars <- c("obese", "unemployed")
#' query <- list("region" = c("South Asia", "Europe & Central Asia"),
#'               "year" = c(2014, 2015, 2016))
#' make_rate_data(grp, valuevars, query, ob)
make_rate_data <- function(grp, valuevars, query, data=ob) {
  data %>% 
    group_by(!!!syms(grp)) %>% 
    filter(region %in% query$region, 
           year %in% query$year,
           income %in% query$income,
           country %in% query$country) %>% 
    summarize(across(c(!!!syms(valuevars)), list(rate = ~rate(., pop))))
}
