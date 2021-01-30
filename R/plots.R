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
  fltr <- list(
    region = .region, year = .year, income = .income,
    sex = remap_sex(.sex)
  )

  # Subset and aggregate data
  df <- make_rate_data("country", fltr)

  # Plot
  p <- df %>%
    arrange(desc(obese_rate)) %>%
    head(n) %>%
    mutate(across(country, fct_reorder, .x = obese_rate)) %>%
    ggplot(aes(
      x = obese_rate,
      y = country,
      fill = obese_rate,
      text = paste(
        "Country:", country,
        "\nObesity Rate: ", scales::percent(obese_rate, 1.1),
        "\nYear: ", .year
      )
    )) +
    geom_col() +
    scale_fill_viridis_c(limits = c(min(df$obese_rate, na.rm = TRUE), max(df$obese_rate)), oob = scales::squish, labels = scales::percent_format(1)) +
    labs(
      title = str_glue("Top 10 Countries ({.year})"),
      x = "Obesity Rate(%)",
      y = NULL,
      fill = "Obesity"
    ) +
    theme(axis.title.y = element_blank()) +
    theme_classic() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  ggplotly(p, tooltip = c("text"))
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
  fltr <- list(
    region = .region, year = .year, income = .income,
    sex = remap_sex(.sex)
  )

  # Subset and aggregate data
  df <- make_rate_data("country", fltr) %>%
    left_join(select(cydict, country = world_bank, iso3c),
      by = "country"
    ) %>%
    mutate(text_tooltip = paste(
      "Country:", country,
      "\nObesity Rate: ", scales::percent(obese_rate, 1.1),
      "\nYear: ", .year
    ), across(obese_rate, ~ . * 100))

  # Plot
  plot_ly(df,
    type = "choropleth", locations = ~iso3c, z = ~obese_rate,
    text = ~text_tooltip, hoverinfo = "text"
  ) %>%
    colorbar(
      limits = c(min(df$obese_rate, na.rm = TRUE), max(df$obese_rate)),
      value = "percent",
      title = "<b> Obesity Rate </b>",
      ticksuffix = "%"
    )
}

#' Create a Scatter Map of Obesity Rates vs. Other Variables
#' @param .region The region input callback (character vector)
#' @param .year The year input callback (integer vector)
#' @param .income The income group callback (character vector)
#' @param .sex The sex group callback (scalar character)
#' @param .regressor The regressor to be used in the scatter plot (character vector)
#' @param .grouper The attribute to be used for grouping the data in the scatter plot (character vector)
#' @return A plotly object.
#' @export
make_scatter_plot <- function(.region = NULL, .year = NULL, .income = NULL, .sex = NULL, .regressor = "smoke", .grouper = "sex") {
  # Generate a filtering string
  fltr <- list(
    region = .region, year = .year, income = .income,
    sex = remap_sex(.sex)
  )
  # Subset and aggregate data
  chosen_rate <- as.character(str_glue("{.regressor}_rate"))
  df <- make_rate_data(c(.grouper, "country"), fltr, vals = c(.regressor, "obese")) %>%
    mutate(rate = !!sym(chosen_rate))

  # Plot
  p <- df %>% ggplot(
    aes(
      x = rate,
      y = obese_rate,
      color = !!sym(.grouper)
    )
  ) +
    geom_point(aes(text = str_glue("Country: {country}
                                    Obesity Rate: {scales::percent(obese_rate, 0.1)}
                                    {create_label(.regressor)}: {scales::percent(rate, 0.1)}"))) +
    geom_smooth(se = FALSE, method = "lm") +
    labs(
      title = str_glue("Obesity Rate vs {create_label(.regressor)}"),
      x = str_glue("{create_label(.regressor)}"),
      y = "Obesity Rate",
      color = create_label(.grouper)
    ) +
    scale_x_continuous(labels = scales::percent_format(1)) +
    scale_y_continuous(labels = scales::percent_format(1)) +
    ggthemes::scale_color_tableau()
  ggplotly(p, tooltip = "text")
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
make_ts_plot <- function(.year = 2010, .sex = NULL, .highlight_country = "Canada", .year_range = list(1975, 2016)) {
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
    aes(
      x = year,
      y = obese_rate,
      group = country
    ) +
    geom_line(aes(text = paste(
      "Country:", country,
      "\nObesity Rate: ", scales::percent(obese_rate, 1.1),
      "\nYear: ", year
    )),
    color = "grey80",
    alpha = 0.5
    ) + # Add lines
    geom_point(
      data = highlight %>% filter(year == max(all_years)), # Add end points
      aes(
        x = as.integer(year),
        y = obese_rate,
        text = paste(
          "Country:", country,
          "\nObesity Rate: ", scales::percent(obese_rate, 1.1),
          "\nYear: ", year
        )
      ),
      size = 1,
      color = "black",
      pch = 21
    ) +
    guides(fill = FALSE) + # Remove legend for points
    geom_line(
      data = highlight, # Add highlighted countries
      aes(
        x = year,
        y = obese_rate,
        color = country,
        text = paste(
          "Country:", country,
          "\nObesity Rate: ", scales::percent(obese_rate, 1.1),
          "\nYear: ", year
        )
      )
    ) +
    geom_vline(xintercept = .year, linetype = "dotted") + # Add vertical line
    scale_x_continuous(
      limits = c(min(all_years), max(all_years)),
      expand = c(0, 0),
      breaks = seq(1975, 2020, by = 5)
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      x = "Year",
      y = "Obesity Rate",
      color = "Country",
      title = paste0("World Obesity (", sub, ")"),
      subtitle = sub
    ) +
    theme_bw()

  ggplotly(ts_plot, tooltip = c("text"))
}
