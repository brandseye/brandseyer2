context("test-filters.R")
library(brandseyer2)

#-------------------------------------------------------

test_that("Can add a pickedup restriction to a filter", {
  time <- lubridate::ymd_hm("2018/03/03 13:45", tz = "Africa/Johannesburg")
  filter <- "test"

  expect_equal(brandseyer2:::add_pickedup(filter, "Africa/Johannesburg", time), "test and (pickedUp before '2018-03-03 13:45')")
  expect_equal(brandseyer2:::add_pickedup(filter, "UTC", time), "test and (pickedUp before '2018-03-03 11:45')")
})
