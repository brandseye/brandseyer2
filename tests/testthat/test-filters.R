context("test-filters.R")
library(brandseyer2)

v4account <- create_account(
  list(
    storage = "V4",
    name = "Test Account",
    code = "TEST03AA",
    accountType = "INTERNAL",
    timezone = "Africa/Johannesburg",
    brands = list(
      list(
        id = as.integer(10),
        name = "Your Brand Here"
      )
    )
  ))

#-------------------------------------------------------

test_that("Can add a pickedup restriction to a filter", {
  time <- lubridate::ymd_hm("2018/03/03 13:45", tz = "Africa/Johannesburg")
  filter <- "test"

  expect_equal(brandseyer2:::add_pickedup(filter, "Africa/Johannesburg", time), "test and (pickedUp before '2018-03-03 13:45')")
  expect_equal(brandseyer2:::add_pickedup(filter, "UTC", time), "test and (pickedUp before '2018-03-03 11:45')")
})


test_that("Can create a query object from an account", {
  q <- to_query(account("TEST01AA"))

  expect_equal(q$accounts, c("TEST01AA"))
  expect_equal(map_int(q$brands, "id"), c(2, 1))
  expect_equal(q$timezone, "Africa/Johannesburg")
  expect_equal(q$grouping, NULL)
  expect_equal(q$comparison, NULL)
  expect_equal(q$fields, NULL)
  expect_equal(q$ordering, NULL)
  expect_equal(is_query(q), TRUE)
  expect_equal(is_query("bob"), FALSE)

  expect_equal(map_int(get_query_brands(q), "id"), c(2, 1))
})

test_that("Query for a V3 account is null", {
  expect_warning(to_query(account("TEST02AA")), regexp = "V4")
  q <- suppressWarnings(to_query(account("TEST02AA")))
  expect_equal(q$accounts, NULL)
  expect_equal(q$brands, NULL)
  expect_equal(q$timezone, NULL)
})

test_that("Can filter a query", {
  q <- to_query(account("TEST01AA")) %>% filter_mentions("test")
  expect_equal(q$filter, "test")

  q <- account("TEST01AA") %>% filter_mentions("test2")
  expect_equal(q$filter, "test2")

  q <- list(v4account, account("TEST01AA")) %>% filter_mentions("test3")
  expect_equal(q$filter, "test3")
})

test_that("Can group a query", {
  q <- to_query(account("TEST01AA")) %>% group_mentions_by(bob, bobette)
  expect_equal(q$grouping, c("bob", "bobette"))

  q <- account("TEST01AA") %>% group_mentions_by(alice)
  expect_equal(q$grouping, "alice")

  q <- list(v4account, account("TEST01AA")) %>% group_mentions_by(published, tag)
  expect_equal(q$grouping, c("published", "tag"))
})

test_that("Can order a query", {
  q <- to_query(account("TEST01AA")) %>% with_order(bob, bobette)
  expect_equal(q$ordering, c("bob", "bobette"))

  q <- account("TEST01AA") %>% with_order(alice)
  expect_equal(q$ordering, "alice")

  q <- list(v4account, account("TEST01AA")) %>% with_order(published, tag)
  expect_equal(q$ordering, c("published", "tag"))
})

test_that("Can select fields", {
  q <- to_query(account("TEST01AA")) %>% with_fields(bob, bobette)
  expect_equal(q$fields, c("bob", "bobette"))

  q <- account("TEST01AA") %>% with_fields(alice)
  expect_equal(q$fields, "alice")

  q <- list(v4account, account("TEST01AA")) %>% with_fields(published, tag)
  expect_equal(q$fields, c("published", "tag"))
})

test_that("Can add subfilters", {
  q <- to_query(account("TEST01AA")) %>% compare_mentions(one = "one", two = "two", "three")
  expect_equal(q$comparison, list(one = "one", two = "two", "three"))

  q <- account("TEST01AA") %>% compare_mentions(one = "1", two = "2", "3")
  expect_equal(q$comparison, list(one = "1", two = "2", "3"))

  q <- list(v4account, account("TEST01AA")) %>% compare_mentions(one = "4", two = "5", "6")
  expect_equal(q$comparison, list(one = "4", two = "5", "6"))
})

test_that("Can add an account", {
  q <- to_query(v4account) %>% with_account("TEST01AA")
  expect_equal(q$accounts, c("TEST03AA", "TEST01AA"))

  q <- v4account %>% with_account("TEST01AA")
  expect_equal(q$accounts, c("TEST03AA", "TEST01AA"))

  q <- to_query(v4account) %>% with_account(account("TEST01AA"))
  expect_equal(q$accounts, c("TEST03AA", "TEST01AA"))

  expect_equal(get_query_accounts(q) %>% as.vector(), c("TEST03AA", "TEST01AA"))
})

test_that("Filtering with a V3 account removes it", {
  expect_warning(account("TEST01AA", "TEST02AA") %>% filter_mentions("today"),
                 regexp = "V4")
  q <- suppressWarnings(account("TEST01AA", "TEST02AA") %>% filter_mentions("today"))
  expect_equal(q$accounts, "TEST01AA")
})

test_that("Can merge queries", {
  q1 <- account("TEST01AA") %>%
    filter_mentions("today") %>%
    compare_mentions(one = "one", two = "two") %>%
    group_mentions_by(published1) %>%
    with_order(order1) %>%
    with_fields(field1)

  q2 <- to_query(v4account) %>%
    filter_mentions("tomorrow") %>%
    compare_mentions(three = "three") %>%
    group_mentions_by(published2) %>%
    with_order(order2) %>%
    with_fields(field2)

  q <- brandseyer2:::merge_query(q1, q2)

  expect_equal(q$accounts, c("TEST01AA", "TEST03AA"))
  expect_equal(map_int(q$brands, "id"), c(2L, 1L, 10L))
  expect_equal(q$timezone, c("Africa/Johannesburg", "Africa/Johannesburg"))
  expect_equal(q$grouping, "published1")
  expect_equal(q$comparison, list(one = "one", two = "two"))
  expect_equal(q$fields, "field1")
  expect_equal(q$ordering, "order1")
})
