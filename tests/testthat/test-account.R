context("test-account.R")
library(brandseyer2)

v3_data <- list(
  storage = "V3",
  name = "Test Account",
  code = "BETE01AA",
  accountType = "INTERNAL"
)

# -----------------------------------------------

test_that("Can create an account structure", {
  v3_account <<- create_account(v3_data)
  expect_s3_class(v3_account, "brandseyer2.account.v3")
  expect_s3_class(v3_account, "brandseyer2.account")

  acs <- account("TEST01AA")
  expect_s3_class(acs, "brandseyer2.account")

  acs <- account("TEST01AA", "TEST02AA")
  expect_equal(length(acs), 2)

  acs <- tibble::tibble(account = c("TEST01AA", "TEST02AA")) %>%
    account()
  expect_equal(length(acs), 2)

  expect_error(tibble::tibble(bob = c("TEST01AA", "TEST02AA")) %>% account(),
               regexp = "No `account` column")

})

# -----------------------------------------------

test_that("Can detect if something is an account object", {
  expect_equal(is_account(v3_account), TRUE)
  expect_equal(is_account(account("TEST01AA")), TRUE)
  expect_equal(is_account(1), FALSE)
})

# -----------------------------------------------

test_that("Can read account code", {
  expect_equal(account_code(v3_account), "BETE01AA")
})

test_that("Can read account type", {
  expect_equal(account_type(v3_account), "INTERNAL")
  expect_equal(account_type(account("TEST01AA")), "PAID")
})

test_that("Can read account name", {
  expect_equal(account_name(v3_account), "Test Account")
})

test_that("Can read account api format", {
  expect_equal(account_api_version(v3_account), "V3")
})

test_that("Can read an account's timezone", {
  expect_equal(account("TEST01AA") %>% account_timezone(), "Africa/Johannesburg")
  expect_equal(account("TEST02AA") %>% account_timezone(), "UTC")
})

test_that("Can fetch account tags", {
  tags <- account("TEST01AA") %>%
    tags()

  expect_equal(nrow(tags), 6)
  expect_warning(tags$account)

  tag1 <- tags %>% filter(id == 1)
  expect_equal(tag1$name, "tag1")
  expect_equal(tag1$namespace, "tag")
  expect_equal(tag1$deleted, FALSE)
})

test_that("Can fetch tags for multiple accounts", {
  tags <- accounts(c("TEST01AA", "TEST02AA")) %>%
    tags()

  expect_equal(nrow(tags), 11)
  expect_equal(length(tags$account), 11)
})

test_that("Can annotate tags with parents", {
  tag_list <- account("TEST01AA") %>%
    tags() %>%
    with_tag_parents(1001)

  expect_equal(tag_list$parent, c(NA, NA, 1001, 10, 10, NA))

  tag_list <- account("TEST01AA") %>% tags()
  expect_error(with_tag_parents(tag_list), regexp = "parent_id")
  expect_error(with_tag_parents(tag_list, 98284374), regexp = "not present")
})

test_that("Can fetch topics for an account", {
  topics <- account("TEST01AA") %>%
    topics()

  expect_equal(nrow(topics), 3)

  parent <- topics[1, ]
  expect_equal(parent$is_parent, TRUE)
  expect_equal(length(unlist(parent$children)), 2)
})

test_that("Can fetch topics for multiple accounts", {
  topics <- accounts(c("TEST01AA", "TEST02AA")) %>%
    topics()

  expect_equal(nrow(topics), 6)
  expect_equal(length(topics$account), 6)
})

test_that("Can fetch topic_tree for an account", {
  tree <- account("TEST01AA") %>%
    topic_trees()

  expect_equal(nrow(tree), 1)
  expect_equal(tree[[1, "namespace"]], "topic_tree")
})

test_that("Can read an account's manager", {
  manager <- account("TEST01AA") %>%
    account_manager()

  expect_equal(manager$name, "Your Manager")
  expect_equal(manager$email, "noreply@brandseye.com")
})

test_that("Can read brand information for an account", {
  brands <- account("TEST01AA") %>%
    brands()

  expect_equal(nrow(brands), 5)
  expect_equal(brands$id, c(1, 2, 5, 6, 3))
})

test_that("Can read brands from more than one account", {
  brands <- account("TEST01AA", "TEST02AA") %>%
    brands()

  expect_equal(nrow(brands), 6)
  expect_equal(brands$id, c(1, 2, 5, 6, 3, 100))
})

test_that("Can read root brand info for accounts", {
  rb <- account("TEST01AA") %>% root_brands()
  expect_equal(nrow(rb), 2)

  rb <- account(c("TEST01AA", "TEST02AA")) %>% root_brands()
  expect_equal(nrow(rb), 3)

  rb <- tibble::tribble(
    ~id, ~parent, ~deleted, ~archived,
    1,   NA,      FALSE,    NA,
    2,   NA,      TRUE,     NA,
    3,   1,       FALSE,    NA,
    4,   NA,      FALSE,    lubridate::ymd_hm("2018/01/01 00:00")
  )

  expect_equal(nrow(rb %>% root_brands()), 1)
  expect_equal(nrow(rb %>% root_brands(includeDeleted = TRUE)), 2)
  expect_equal(nrow(rb %>% root_brands(includeDeleted = TRUE, includeArchived = TRUE)), 3)
  expect_equal(nrow(data.frame() %>% root_brands()), 0)
})

test_that("Can read phrase information for an account", {
  phrases <- account("TEST01AA") %>%
    phrases()

  expect_equal(nrow(phrases), 9)
  expect_equal(phrases$phrase.id, c(102, 102, 101, 101, 5, 3, 2, 4, 1))
  expect_equal(phrases$brand.id, c(5, 6, 5, 6, 3, 2, 1, 2, 1))
})

test_that("Can read phrase information for multiple accounts", {
  phrases <- account(c("TEST01AA", "TEST02AA")) %>%
    phrases()

  expect_equal(nrow(phrases), 11)
  expect_equal(phrases$phrase.id, c(102, 102, 101, 101, 5, 3, 2, 4, 1, 102, 101))
  expect_equal(phrases$brand.id, c(5, 6, 5, 6, 3, 2, 1, 2, 1, 100, 100))
})
