
expect_plotly <- purrr::partial(checkmate::expect_class, classes = "plotly")

test_that("Plotting functions must not error", {
  suppressWarnings({
    expect_error(make_bar_plot(), NA)
    expect_error(make_choropleth_plot(), NA)
    expect_error(make_ts_plot(), NA)
    expect_error(make_scatter_plot(), NA)
  })
})

test_that("Plotting functions must return a plotly object", {
  suppressWarnings({
    expect_plotly(make_bar_plot())
    expect_plotly(make_choropleth_plot())
    expect_plotly(make_ts_plot())
    expect_plotly(make_scatter_plot())
  })
})
