context("test-account.R")
library(brandseyer2)

v3_data <- list(
  storage = "V3",
  name = "Test Account",
  code = "BETE01AA"
)

v3_account <- structure(
  list(data = v3_data),
  class = c("brandseyer2.account", "brandseyer2.account.v3"))

# -----------------------------------------------

test_that("Can read account code", {
  expect_equal(account_code(v3_account), "BETE01AA")
})

test_that("Can read account name", {
  expect_equal(account_name(v3_account), "Test Account")
})

test_that("Can read account api format", {
  expect_equal(account_api_version(v3_account), "V3")
})
