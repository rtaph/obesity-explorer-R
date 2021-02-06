# Obesity-explorer application

# Load libraries
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(devtools)

# Load custom functions and data
devtools::load_all(".")

# define year range
a <- as.character(c(1975:2016))

# Load CSS Styles
css <- custom_css()

# Define app layout
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$title("Obesity Explorer")
app$layout(
  dbcContainer(
    list(
      htmlH1("Obesity Dashboard",
        style = css$header
      ),
      dccMarkdown(header),
      dbcRow(
        list(
          dbcCol( # INPUT CONTROLS
            id = "options-panel",
            style = css$box,
            md = 4,
            list(
              htmlBr(),
              dbcLabel("Filter Sex:"),
              dccRadioItems(
                id = "input_sex",
                options = list(
                  list(label = "Male", value = "Male"),
                  list(label = "Female", value = "Female"),
                  list(label = "Both", value = "Both")
                ),
                css$dd,
                labelStyle = list("display" = "inline-block"),
                inputStyle = css$radio_buttons,
                value = "Both"
              ),
              htmlBr(),
              dbcLabel("Filter Year:"),
              dccSlider(
                id = "input_year",
                min = 1975,
                max = 2016,
                step = 1,
                value = 2016,
                included = FALSE,
                marks = as.list(set_names((a[seq(1, length(a), 5)])))
              ),
              htmlBr(),
              dbcLabel("Filter Region:"),
              dccDropdown(
                id = "input_region",
                options = map(
                  unique(ob$region),
                  ~ list(label = ., value = .)
                ),
                value = unique(ob$region),
                clearable = FALSE,
                style = css$dd,
                multi = TRUE
              ),
              htmlBr(),
              dbcLabel("Filter Income Group:"),
              dccDropdown(
                id = "input_income",
                options = map(unique(ob$income), ~ list(label = ., value = .)),
                value = unique(ob$income),
                style = css$dd,
                clearable = FALSE,
                multi = TRUE
              ),
              htmlHr(),
              dccMarkdown(footer, style = css$sources)
            )
          ),
          htmlBr(),
          dbcCol( # PLOTTING PANEL
            list(
              dccTabs(id = "tabs", children = list(
                dccTab(label = "Country Standings", children = list(
                  htmlDiv(
                    list(
                      dbcCard(
                        list(
                          dbcCardBody(
                            dccGraph(
                              id = "choropleth_plot",
                              config = list("displayModeBar" = FALSE)
                            )
                          )
                        )
                      ),
                      dbcCard(
                        list(
                          dbcCardBody(
                            list(
                              dccGraph(
                                id = "bar_plot",
                                config = list("displayModeBar" = FALSE)
                              ),
                              htmlHr(),
                              dbcRow(
                                justify = "center",
                                list(
                                  dbcCol(
                                    list(
                                      dbcLabel("Select Number:"),
                                      dbcInput(
                                        id = "input_n",
                                        type = "number",
                                        placeholder = "number",
                                        style = list(width = "150px"),
                                        value = 10,
                                        min = 5,
                                        max = 20
                                      )
                                    ),
                                    md = 3
                                  ),
                                  dbcCol(
                                    list(
                                      dbcLabel("Select Ranking:"),
                                      dccRadioItems(
                                        id = "input_rank",
                                        options = list(
                                          list(label = "Top", value = "TRUE"),
                                          list(label = "Bottom", value = "FALSE")
                                        ),
                                        css$dd,
                                        labelStyle = list("display" = "inline-block"),
                                        inputStyle = css$radio_buttons,
                                        value = "TRUE"
                                      )
                                    ), 
                                    md = 3
                                  )
                                )
                              )
                      
                            )

                          )
                        )
                      )
                    )
                  )
                )),
                dccTab(label = "Trends", children = list(
                  htmlDiv(
                    list(
                      dbcCard(
                        list(
                          dbcCardBody(
                            list(
                              dccGraph(
                                id = "ts_plot",
                                config = list("displayModeBar" = FALSE)
                              ),
                              htmlHr(),
                              dbcLabel("Select Year Range:"),
                              dccRangeSlider(
                                id = "input_year_range",
                                min = 1975,
                                max = 2016,
                                step = 1,
                                value = c(1975, 2016),
                                marks = as.list(set_names((a[seq(1, length(a), 5)])))
                              ),
                              htmlBr(),
                              dbcLabel("Highlight Countries:"),
                              dccDropdown(
                                id = "input_highlight_country",
                                options = map(
                                  unique(ob$country),
                                  ~ list(label = ., value = .)
                                ),
                                value = "Canada",
                                clearable = TRUE,
                                searchable = TRUE,
                                multi = TRUE
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )),
                dccTab(label = "Associations", children = list(
                  htmlDiv(
                    list(
                      dbcCard(
                        list(
                          dbcCardBody(
                            list(
                              dccGraph(
                                id = "scatter_plot",
                                config = list("displayModeBar" = FALSE)
                              ),
                              htmlHr(),
                              dbcLabel("Select Coloring Variable: "),
                              dccDropdown(
                                id = "input_grouper",
                                options = list(
                                  list(label = "Income level (country)", value = "income"),
                                  list(label = "Sex", value = "sex"),
                                  list(label = "Region", value = "region"),
                                  list(label = "No grouping", value = "none")
                                ),
                                value = "none",
                                style = css$dd,
                                clearable = FALSE,
                                multi = FALSE
                              ),
                              htmlBr(),
                              dbcLabel("Select X-Axis Variable: "),
                              dccDropdown(
                                id = "input_regressor",
                                options = list(
                                  list(label = "Smoking Rate", value = "smoke"),
                                  list(
                                    label = "Literacy Rate",
                                    value = "literacy"
                                  ),
                                  list(
                                    label = "Unemployment Rate",
                                    value = "unemployed"
                                  )
                                ),
                                value = "unemployed",
                                clearable = FALSE,
                                style = css$dd,
                                multi = FALSE
                              ),
                              htmlDiv(id = "load"),
                              htmlBr()
                            )
                          )
                        )
                      )
                    )
                  )
                ))
              ))
            ),
          )
        ),
        style = css$no_left_pad
      ),
      htmlBr()
    )
  )
)


app$callback(
  output("bar_plot", "figure"),
  list(
    input("input_region", "value"),
    input("input_year", "value"),
    input("input_income", "value"),
    input("input_sex", "value"),
    input("input_rank", "value"),
    input("input_n", "value")
  ),
  memoise::memoize(make_bar_plot)
)

app$callback(
  output("choropleth_plot", "figure"),
  list(
    input("input_region", "value"),
    input("input_year", "value"),
    input("input_income", "value"),
    input("input_sex", "value")
  ),
  memoise::memoize(make_choropleth_plot)
)

app$callback(
  output("ts_plot", "figure"),
  list(
    input("input_year", "value"),
    input("input_sex", "value"),
    input("input_highlight_country", "value"),
    input("input_year_range", "value"),
    input("input_income", "value"),
    input("input_region", "value")
  ),
  memoise::memoize(make_ts_plot)
)

app$callback(
  output("scatter_plot", "figure"),
  list(
    input("input_region", "value"),
    input("input_year", "value"),
    input("input_income", "value"),
    input("input_sex", "value"),
    input("input_regressor", "value"),
    input("input_grouper", "value")
  ),
  memoise::memoize(make_scatter_plot)
)

app$callback(
  output("load", "children"),
  list(
    input("input_regressor", "value")
  ),
  function(selection) {
    if (selection == "smoke") {
      text <- smoke_txt
    } else {
      text <- NULL
    }
    htmlDiv(list(dccMarkdown(text, style = css$caveat)))
  }
)

if (Sys.getenv("DYNO") == "") {
  app$run_server(
    debug = TRUE,
    dev_tools_hot_reload = TRUE
  )
} else {
  app$run_server(host = "0.0.0.0")
}
