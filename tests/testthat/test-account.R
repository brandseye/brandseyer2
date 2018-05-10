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

test_that("Can fetch account tags", {
  tags <- account("TEST01AA") %>%
    account_tags()

  expect_equal(nrow(tags), 5)
  expect_warning(tags$account)
})

test_that("Can fetch tags for multiple accounts", {
  tags <- accounts(c("TEST01AA", "TEST02AA")) %>%
    account_tags()

  expect_equal(nrow(tags), 10)
  expect_equal(length(tags$account), 10)
})

test_that("Can read an account's manager", {
  manager <- account("TEST01AA") %>%
    account_manager()

  expect_equal(manager$name, "Your Manager")
  expect_equal(manager$email, "noreply@brandseye.com")
})
