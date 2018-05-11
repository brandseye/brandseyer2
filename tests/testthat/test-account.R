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

  tag1 <- tags %>% filter(id == 1)
  expect_equal(tag1$name, "tag1")
  expect_equal(tag1$namespace, "tag")
  expect_equal(tag1$deleted, FALSE)
})

test_that("Can fetch tags for multiple accounts", {
  tags <- accounts(c("TEST01AA", "TEST02AA")) %>%
    account_tags()

  expect_equal(nrow(tags), 10)
  expect_equal(length(tags$account), 10)
})

test_that("Can fetch topics for an account", {
  topics <- account("TEST01AA") %>%
    account_topics()

  expect_equal(nrow(topics), 3)

  parent <- topics[1, ]
  expect_equal(parent$is_parent, TRUE)
  expect_equal(length(unlist(parent$children)), 2)
})

test_that("Can fetch topics for multiple accounts", {
  topics <- accounts(c("TEST01AA", "TEST02AA")) %>%
    account_topics()

  expect_equal(nrow(topics), 6)
  expect_equal(length(topics$account), 6)
})

test_that("Can read an account's manager", {
  manager <- account("TEST01AA") %>%
    account_manager()

  expect_equal(manager$name, "Your Manager")
  expect_equal(manager$email, "noreply@brandseye.com")
})

test_that("Can read brand information for an account", {
  brands <- account("TEST01AA") %>%
    account_brands()

  expect_equal(nrow(brands), 3)
  expect_equal(brands$id, 1:3)
})

test_that("Can read phrase information for an account", {
  phrases <- account("TEST01AA") %>%
    account_phrases()

  expect_equal(nrow(phrases), 5)
  expect_equal(phrases$phrase.id, 1:5)
  expect_equal(phrases$brand.id, c(1, 1, 2, 2, 3))
})
