#' Create a Barplot of Country Obesity Rankings
#'
#' @param .region The region input callback (character vector)
#' @param .year The year input callback (integer vector)
#' @param .income The income group callback (character vector)
#' @param .sex The sex group callback (scalar character)
#' @param .ascending a logical indicating the selection for top or bottom for
#'   the country rankings. Also accepts a character. Defaults to TRUE.
#' @param .n a scalar representing the number of countries to chart.
#'
#'
#' @import ggplot2
#' @importFrom plotly ggplotly layout
#' @importFrom utils head
#' @return A plotly object.
#' @export
#'
#' @examples
#' make_bar_plot()
#' make_bar_plot(.n = 5)
#' make_bar_plot(.n = 5, .ascending = FALSE)
#' make_bar_plot(.n = 5, .ascending = "FALSE")
make_bar_plot <- function(.region = NULL, .year = 2016, .income = NULL,
                          .sex = NULL, .ascending = TRUE, .n = 10) {
  .ascending <- as.logical(.ascending)

  # Generate a filtering string
  fltr <- list(
    region = .region, year = .year, income = .income,
    sex = remap_sex(.sex)
  )

  # Subset and aggregate data
  df <- make_rate_data("country", fltr)

  bar_plot_direction <- if_else(.ascending, "Top", "Bottom")

  slicer <- ifelse(.ascending, slice_max, slice_min)

  # Plot
  p <- df %>%
    arrange(desc(.data$obese_rate)) %>%
    slicer(.data$obese_rate, n = .n) %>%
    mutate(across(.data$country, ~ fct_reorder(., .data$obese_rate))) %>%
    ggplot(aes(
      x = .data$obese_rate,
      y = .data$country,
      fill = .data$obese_rate,
      text = paste(
        "Country:", .data$country,
        "\nObesity Rate: ", scales::percent(.data$obese_rate, 1.1),
        "\nYear: ", .year
      )
    )) +
    geom_col() +
    scale_fill_viridis_c(
      limits = c(0, 0.5), oob = scales::squish,
      labels = scales::percent_format(1),
      breaks = seq(0, 0.4, 0.2)
    ) +
    labs(
      title = str_glue("{bar_plot_direction} {.n} Countries ({.year})"),
      x = "Obesity Rate (%)",
      y = NULL,
      fill = "Obesity Rate"
    ) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), 
                       limits = c(0, 0.5))
  ggplotly(p, tooltip = "text", height = 300) %>%
    layout(font = custom_css()$plotly)
}

#' Create a Choropleth Map of Obesity Rates
#'
#' @param .region The region input callback (character vector)
#' @param .year The year input callback (integer vector)
#' @param .income The income group callback (character vector)
#' @param .sex The sex group callback (scalar character)
#'
#' @import ggplot2
#' @importFrom plotly plot_ly colorbar
#' @return A plotly object.
#' @export
#'
#' @examples
#' make_choropleth_plot()
make_choropleth_plot <- function(.region = NULL, .year = 2016, .income = NULL,
                                 .sex = NULL) {
  # Generate a filtering string
  fltr <- list(
    region = .region, year = .year, income = .income,
    sex = remap_sex(.sex)
  )

  # Subset and aggregate data
  df <- make_rate_data("country", fltr) %>%
    left_join(select(obesityexplorer::cydict,
      country = .data$world_bank,
      .data$iso3c
    ),
    by = "country"
    ) %>%
    mutate(text_tooltip = paste(
      "Country:", .data$country,
      "\nObesity Rate: ", scales::percent(.data$obese_rate, 1.1),
      "\nYear: ", .year
    )) %>%
    mutate(across(.data$obese_rate, ~ . * 100))

  # Margin settings
  m <- list(
    l = 70,
    r = 1,
    b = 1,
    t = 50,
    pad = 4
  )

  # Plot
  plot_ly(stats::na.omit(df),
    type = "choropleth", locations = ~iso3c, z = ~obese_rate,
    text = ~text_tooltip, hoverinfo = "text"
  ) %>%
    colorbar(
      limits = c(0, 50),
      value = "percent",
      title = "Obesity Rate",
      ticksuffix = "%",
      x = 1,
      y = 0.8
    ) %>%
    layout(
      margin = m, height = 300, title = list(
        text = paste0("World Obesity (", as.character(.year), ")"),
        y = 0.9
      ), geo = list(
        landcolor = "lightgray",
        showcountries = TRUE,
        showland = TRUE,
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = "geoMercator"),
        lataxis = list(range = list(-55, 90))
      ),
      font = custom_css()$plotly
    )
}

#' Create a Scatter Map of Obesity Rates vs. Other Variables
#' @param .region The region input callback (character vector)
#' @param .year The year input callback (integer vector)
#' @param .income The income group callback (character vector)
#' @param .sex The sex group callback (scalar character)
#' @param .regressor The regressor to be used in the scatter plot (character
#'   vector)
#' @param .grouper The attribute to be used for grouping the data in the
#'   scatter plot (character vector)
#' @return A plotly object.
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom stringr str_glue
#' @export
#'
#' @examples
#' make_scatter_plot()
make_scatter_plot <- function(.region = NULL, .year = NULL, .income = NULL,
                              .sex = NULL, .regressor = "literacy",
                              .grouper = "sex") {
  # Generate a filtering string
  fltr <- list(
    region = .region, year = .year, income = .income,
    sex = remap_sex(.sex)
  )
  # Subset and aggregate data
  chosen_rate <- as.character(str_glue("{.regressor}_rate"))
  df <- make_rate_data(c(.grouper, "country"), fltr,
    vals = c(.regressor, "obese")
  ) %>%
    mutate(rate = !!sym(chosen_rate))

  # Plot
  p <- df %>% ggplot(
    aes(
      x = .data$rate,
      y = .data$obese_rate,
      color = !!sym(.grouper)
    )
  ) +
    geom_point(aes(
      text = str_glue(
        "Country: {country}
         Obesity Rate: {scales::percent(obese_rate, 0.1)}
         {create_label(.regressor)}: {scales::percent(rate, 0.1)}"
      )
    )) +
    geom_smooth(se = FALSE, method = "lm", formula = y ~ x, size = 0.75) +
    labs(
      x = str_glue("{create_label(.regressor)} (%)"),
      y = "Obesity Rate (%)",
      color = create_label(.grouper)
    ) +
    scale_x_continuous(labels = scales::percent_format(1)) +
    scale_y_continuous(labels = scales::percent_format(1)) +
    ggthemes::scale_color_tableau() +
    theme_bw()
  ggplotly(p, tooltip = "text") %>%
    layout(
      title = list(
        text = str_glue("Obesity Rate vs {create_label(.regressor)} ({.year})"),
        xanchor = "center",
        x = 0.5,
        y = 40,
        yanchor = "bottom",
        yref = "paper"
      ),
      margin = list(t = 70),
      font = custom_css()$plotly
    )
}

#' Create a Time Series of Obesity Rates
#'
#' @param .year The year input callback (integer vector)
#' @param .year_range The year range input callback (integer vector)
#' @param .sex The sex group callback (scalar character)
#' @param .highlight_country The countries we want to highlight (character
#'   vector)
#' @param .income The income group callback (character vector)
#' @param .region The region input callback (character vector)
#'
#' @return A plotly object.
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom forcats fct_reorder
#' @export
#'
#' @examples
#' make_ts_plot()
make_ts_plot <- function(.year = 2010, .sex = NULL,
                         .highlight_country = "Canada",
                         .year_range = list(1975, 2016),
                         .income = NULL,
                         .region = NULL) {
  all_years <- seq(.year_range[[1]], .year_range[[2]])

  # Generate a filtering string
  fltr <- list(
    year = all_years,
    sex = remap_sex(.sex),
    income = .income,
    region = .region
  )

  # Subset and aggregate data
  df <- make_rate_data(c("country", "year"), fltr) %>%
    mutate(text = paste(
      "Country:", .data$country,
      "\nObesity Rate: ", scales::percent(.data$obese_rate, 1.1),
      "\nYear: ", .data$year
    ))

  # Get data for highlighted country
  highlight <- df %>%
    filter(.data$country %in% c(.highlight_country)) %>%
    mutate(across(.data$country, factor, levels = .highlight_country))


  # Create subtitle
  sub <- paste0(
    as.character(min(all_years)), "-",
    as.character(max(all_years))
  )

  # Make time series plot
  ts_plot <- df %>%
    filter(!.data$country %in% .highlight_country) %>%
    ggplot(aes(
      x = .data$year,
      y = .data$obese_rate,
      group = .data$country
    )) +
    geom_line(aes(text = .data$text),
      color = "grey80", na.rm = TRUE,
      alpha = 0.5
    ) +
    geom_line(
      data = highlight, # Add highlighted countries
      aes(
        x = .data$year,
        y = .data$obese_rate,
        color = .data$country,
        text = .data$text
      )
    ) +
    geom_vline(aes(group = factor("Selected Year")),
      xintercept = .year,
      linetype = "dotted",
      show.legend = TRUE
    ) + # Add vertical line
    scale_x_continuous(
      limits = c(min(all_years), max(all_years)),
      expand = c(0, 0),
      breaks = seq(1975, 2020, by = 5)
    ) +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(1)) +
    labs(
      x = "Year",
      y = "Obesity Rate",
      color = "Country"
    ) +
    theme_bw()

  ggplotly(ts_plot, tooltip = "text") %>%
    layout(
      title = list(
        text = paste0(
          "World Obesity (", sub, ")",
          "<br>",
          "<sup>",
          str_glue("Year Selected: {.year}"),
          "</sup>"
        ),
        xanchor = "center",
        x = 0.5,
        y = 40,
        yanchor = "bottom",
        yref = "paper"
      ),
      margin = list(t = 70),
      font = custom_css()$plotly
    )
}
