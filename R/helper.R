# Helper functions

#' Safely Calculate a Rate
#'
#' Calculates sum(x) / sum(y) for values for non-missing values in (x, y).
#'
#' @param x A numeric variable acting as a numerator.
#' @param y A numeric variable acting as a denominator.
#'
#' @return A scalar rate.
#' @export
#'
#' @examples
#' x <- c(1, 2, NA, 4)
#' y <- c(5, NA, 6, 7)
#' rate(x, y)
rate <- function(x, y) {
  i <- !(is.na(x) | is.na(y))
  sum(x[i]) / sum(y[i])
}

#' Generate a Rate Data for Plotting
#'
#' @param grp A character string of categorical grouping variable names.
#' @param fltr A named list of filters, where each list element name is the
#'   name of the variable and the element value is a string of values whose
#'   membership is to be evaluated (see examples).
#' @param vals A character string of continuous variables for which a rate will be
#'   calculated relative to population. Defaults to "obese."
#' @param data The obesity dataframe.
#'
#' @return A dataframe with rates calculated for each value in vals.
#' @export
#' @import dplyr rlang
#'
#' @examples
#' ob <- readr::read_csv(here::here("data", "processed", "obesity-combo.csv"))
#' grp <- c("sex", "country")
#' vals <- c("obese", "unemployed")
#' fltr <- list(
#'   region = c("South Asia", "Europe & Central Asia"),
#'   year = c(2014, 2015, 2016)
#' )
#' make_rate_data(grp, query, vals)
make_rate_data <- function(grp, fltr, vals = "obese", data = ob) {
  fltr <- purrr::discard(fltr, is.null)
  data %>%
    filter(across(all_of(names(fltr)), ~ . %in% fltr[[cur_column()]])) %>%
    group_by(!!!syms(grp)) %>%
    summarise(across(all_of(vals), list(rate = ~ rate(., pop))),
      .groups = "drop"
    )
}


#' Remap the Sex Input Variable
#'
#' Remaps to c("Male", "Female") if user selects "Both"
#'
#' @param x A scalar string indicating the radio button selection.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' remap_sex("Male")
#' remap_sex("Both")
#' remap_sex()
remap_sex <- function(x = NULL) {
  stopifnot(length(x) == 1 & x %in% c("Male", "Female", "Both"))
  if (length(x) == 0 || x == "Both") {
    return(c("Female", "Male"))
  } else {
    return(x)
  }
}


#' List of Custom CSS Specs
#'
#' @return A named list of lists containing CSS specifications
#' @export
custom_css <- function() {
  css <- list()
  
  # Input parameter box
  css$box <- list(
    "border" = "1px solid #d3d3d3",
    "border-radius" = "10px",
    "background-color" = "rgba(220, 220, 220, 0.5)"
  )
  
  # Drop-down choices
  css$dd <- list("font-size" = "smaller")
  
  # Footnote text
  css$sources <- list("font-size" = "xx-small")
  
  # Return CSS
  css
}
