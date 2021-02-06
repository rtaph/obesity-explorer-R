# Unit tests for helper functions

test_that("remap_sex() must work", {
  expect_equal(remap_sex("Male"), "Male")
  expect_equal(remap_sex("Female"), "Female")
  expect_equal(remap_sex("Both"), c("Female", "Male"))
  expect_equal(remap_sex(), c("Female", "Male"))
})

test_that("make_rate_data() must return a dataframe", {
  # Parameters
  grp <- c("sex", "country")
  vals <- c("obese", "unemployed")
  fltr <- list(
    region = c("South Asia", "Europe & Central Asia"),
    year = c(2014, 2015, 2016)
  )

  res <- make_rate_data(grp, fltr, vals)
  dtypes <- c("character", "numeric")
  checkmate::expect_data_frame(res, types = dtypes)
})

test_that("create_label() must work", {
  expect_equal(create_label("obese_rate"), "Obesity Rate")
  expect_equal(create_label("smoke_rate"), "Smoking Rate")
  expect_equal(create_label("smoke"), "Smoking Rate")
  expect_equal(create_label("income"), "Income Level")
  expect_equal(create_label("literacy"), "Adult Literacy Rate")
  expect_equal(create_label("primedu"), "Primary Education Rate")
  expect_equal(create_label("region"), "Region")
  expect_equal(create_label("unemployed"), "Unemployment Rate")
  expect_equal(create_label("country"), "Country")
  expect_equal(create_label("none"), "")
  expect_equal(create_label("sex"), "Sex")
})

test_that("make_bar_plot() must accept .ascending as logical or character", {
  expect_error(make_bar_plot(.ascending = TRUE), NA)
  expect_error(make_bar_plot(.ascending = "TRUE"), NA)
})
