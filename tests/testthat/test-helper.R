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
