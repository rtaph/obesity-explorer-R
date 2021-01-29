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
#' @param .year The year input callback (integer vector)
#' @param .year_range The year range input callback (integer vector)
#' @param .sex The sex group callback (scalar character)
#' @param .highlight_country The countries we want to highlight (character vector)
#'
#' @return A plotly object.
#' @export
make_ts_plot <- function(.year = 2010, .sex = NULL, .highlight_country = 'Canada', .year_range = list(1975, 2016)) {

  all_years <- seq(.year_range[[1]], .year_range[[2]])
  # Generate a filtering string
  fltr <- list(year = all_years, sex = remap_sex(.sex))
  
  # Subset and aggregate data
  df <- make_rate_data(c("country", "year"), fltr)
  
  # Get data for highlighted country
  highlight <- df %>% 
    filter(country %in% .highlight_country) %>% 
    mutate(across(country, factor, levels = .highlight_country))
    
  
  # Create subtitle
  sub <- paste0(as.character(min(all_years)), "-", as.character(max(all_years)))
  
  # Make time series plot
  ts_plot <- df %>% 
    filter(!country %in% .highlight_country) %>% # Remove highighted countries
    ggplot() +
      aes(x = year,
          y = obese_rate,
          group = country) +
    geom_line(aes(text = paste0("Country: ", country, 
                                "\nObesity Rate: ", scales::percent(obese_rate, accuracy = 1.1),
                                "\nYear: ", year)), color = 'grey80', alpha = 0.5) + # Add lines
    geom_point(data = highlight %>% filter(year == max(all_years)), # Add end points
               aes(x = as.integer(year),
                   y = obese_rate),
               size = 1,
               color = "black",
               pch=21) +
    guides(fill=FALSE) +        # Remove legend for points
    geom_line(data = highlight, # Add highlighted countries
              aes(x = year,
                  y = obese_rate,
                  color = country,
                  text = paste0("Country: ", country, 
                                "\nObesity Rate: ", scales::percent(obese_rate, accuracy = 1.1),
                                "\nYear: ", year))) +
    geom_vline(xintercept=.year, linetype="dotted") + # Add vertical line
    scale_x_continuous(limits = c(min(all_years), max(all_years)), 
                       expand = c(0, 0),
                       breaks = seq(1975, 2020, by=5)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Year", 
         y = "Obesity Rate", 
         color = "Country",
         title=paste0("World Obesity (", sub, ")"),
         subtitle=sub) +
    theme_bw()
  
  ggplotly(ts_plot, tooltip = c("text"))
}
