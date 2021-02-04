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
#' @param vals A character string of continuous variables for which a rate will
#'   be calculated relative to population. Defaults to "obese."
#'
#' @return A dataframe with rates calculated for each value in vals.
#' @export
#' @import dplyr rlang
#' @importFrom rlang .data
#'
#' @examples
#' grp <- c("sex", "country")
#' vals <- c("obese", "unemployed")
#' fltr <- list(
#'   region = c("South Asia", "Europe & Central Asia"),
#'   year = c(2014, 2015, 2016)
#' )
#' make_rate_data(grp, fltr, vals)
make_rate_data <- function(grp, fltr, vals = "obese") {

  fltr <- purrr::discard(fltr, is.null)
  obesityexplorer::ob %>%
    filter(across(all_of(names(fltr)), ~ . %in% fltr[[cur_column()]])) %>%
    group_by(!!!syms(grp)) %>%
    summarise(across(all_of(vals), list(rate = ~ rate(., .data$pop))),
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

#' Create proper label for tooltips and plots
#'
#' @param x scalar string
#'
#' @return A character vector of re-maped labels.
#' @export
#'
#' @examples
#' create_label("obese_rate")
#' create_label(c("obese_rate", "income"))
create_label <- function(x) {
  case_when(
    x == "obese_rate" ~ "Obesity Rate",
    x == "smoke_rate" ~ "Smoking Rate",
    x == "smoke" ~ "Smoking Rate",
    x == "income" ~ "Income Level",
    x == "primedu" ~ "Primary Education Rate",
    x == "region" ~ "Region",
    x == "unemployed" ~ "Unemployment Rate",
    x == "country" ~ "Country",
    x == "none" ~ "",
    x == "sex" ~ "Sex",
    is.null(x) ~ "",
    TRUE ~ x
  )
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

#' The Obesity Data
#'
#' A dataset containing the obesity dataset for the years 1960-2019. The
#' original dataset has been joined with World Bank indicator data. Data are
#' partitioned into mutually exclusive strata (rows) so that they may be
#' aggregated according to user needs.
#'
#' @format A data frame with 23,138 rows and 18 variables: \describe{
#'   \item{country}{The country name.} \item{year}{The year of stratum.}
#'   \item{sex}{The sex of individuals in the stratum.} \item{iso2c}{The ISO
#'   2-letter country code.} \item{iso3c}{The ISO 3-letter country code.}
#'   \item{region}{The geogrpahic region of the country.} \item{capital}{The
#'   capital city of the country.} \item{longitude}{The longitude of the
#'   capital city.} \item{latitude}{The latitude of the capital city.}
#'   \item{income}{The name of the country income group.} \item{lending}{The
#'   name of the IMF country lending category.} \item{lifexp}{The country life
#'   expectancy for the stratum.} \item{pop}{The population count of the
#'   stratum (row).} \item{primedu}{The number of individuals in the stratum
#'   (row) who have completed basic primary education} \item{smoke}{The number
#'   of individuals in the stratum (row) who smoke.} \item{unemployed}{The
#'   number of individuals in the stratum (row) who are unemployed.}
#'   \item{obese}{The number of individuals in the stratum (row) who are
#'   obese.} \item{none}{A placeholder column with level "All" for dashboarding
#'   purposes only.} }
#' @source
#' \describe{
#'   \item{WHO Obesity Data}{\url{https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-obesity-among-adults-bmi-=-30-(age-standardized-estimate)-(-)}}
#'   \item{World Bank Indicators}{\url{https://data.worldbank.org/indicator}}
#'   }
#'
"ob"

#' Dictionary of Country Names
#'
#' A mapping of country IDs and names across datasets.
#'
#' @format A data frame with 262 rows and 5 variables:
#' \describe{
#'   \item{id}{The country ID in Altair's geojson template.}
#'   \item{altair}{The country name in Altair (Python).}
#'   \item{obesity}{The country name in the obesity dataset.}
#'   \item{world_bank}{The country name in World Bank dataset.}
#'   \item{iso3c}{The ISO 3-letter country code.}
#' }
"cydict"
