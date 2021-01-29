#' Create a Barplot of Country Obesity Rankings
#'
#' @param .region The region input callback (character vector)
#' @param .year The year input callback (integer vector)
#' @param .income The income group callback (character vector)
#' @param .sex The sex group callback (scalar character)
#' @param n a scalar representing the number of countries to chart.
#'
#' @return A plotly object.
#' @export
make_bar_plot <- function(.region, .year, .income, .sex, n = 10) {
  # Generate a filtering string
  fltr <- list(region = .region, year = .year, income = .income,
               sex = remap_sex(.sex))
  
  # Subset and aggregate data
  df <- make_rate_data("country", fltr)
  
  # Plot
  p <- df %>%
    arrange(desc(obese_rate)) %>%
    head(n) %>%
    mutate(across(country, fct_reorder, .x = obese_rate)) %>%
    ggplot(aes(x = obese_rate,
               y = country,
               fill = obese_rate,
               text = country)) +
    geom_col() +
    scale_fill_viridis_c() +
    labs(title = "Top 10 Countries",
         subtitle = .year, 
         x = "Obesity Rate(%)", 
         fill = "Obesity") +
    theme(axis.title.y = element_blank()) +
    theme_classic() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  ggplotly(p)
}

#' Create a Choropleth Map of Obesity Rates
#'
#' @param .region The region input callback (character vector)
#' @param .year The year input callback (integer vector)
#' @param .income The income group callback (character vector)
#' @param .sex The sex group callback (scalar character)
#' @param cydict A two-column dataframe containing country names and their
#'   3-letter ISO codes.
#'
#' @return A plotly object.
#' @export
make_choropleth_plot <- function(.region = NULL, .year = NULL, .income = NULL,
                                 .sex = NULL, cydict = cydict) {
  # Generate a filtering string
  fltr <- list(region = .region, year = .year, income = .income,
               sex = remap_sex(.sex))
  
  # Subset and aggregate data
  df <- make_rate_data("country", fltr) %>%
    left_join(select(cydict, country = world_bank, iso3c), 
              by = "country")
  
  # Plot
  plot_ly(df, type='choropleth', locations=~iso3c, z=~obese_rate,
          text = ~country)
}

#' Create a Scatter Map of Obesity Rates vs. Other Variables
#'
#'
#' @return A plotly object.
#' @export
make_scatter_plot <- function() {
  NULL
}

#' Create a Time Series of Obesity Rates
#'
#'
#' @return A plotly object.
#' @export
make_ts_plot <- function() {
  NULL
}
