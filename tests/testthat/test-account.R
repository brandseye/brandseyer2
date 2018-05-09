context("test-account.R")
library(brandseyer2)

v3_data <- list(
  storage = "V3",
  name = "Test Account",
  code = "BETE01AA"
)

# -----------------------------------------------

test_that("Can create an account structure", {
  v3_account <<- create_account(v3_data)
  classes <- class(v3_account)
  expect_equal(classes[1], "brandseyer2.account.v3")
  expect_equal(classes[2], "brandseyer2.account")
})

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
