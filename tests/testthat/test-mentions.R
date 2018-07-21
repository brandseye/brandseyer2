context("test-mentions.R")
library(brandseyer2)

test_that("Can parse a minimal list of mention data", {
  data <- list(
    list(id = "one",
         sentiment = as.integer(0),
         published = "2017-11-21T21:37:35.000+0000",
         pickedUp = "2017-11-22T21:37:35.000+0000",
         updated = "2017-11-23T21:37:35.000+0000",
         brands = list(
           list(id = as.integer(1), name = "one"),
           list(id = as.integer(2), name = "two")
         ),
         tags = list(
           list(id = as.integer(1), name = "one")
         ),
         mediaLinks = list(
           list(url = "http://bob.com", mimeType = "application/json")
         ),
         socialNetwork = list(
           id = "TWITTER",
           label = "Twitter"
         )
    ),
    list(id = "two",
         sentiment = as.integer(1),
         tags = list()
    )
  )

  m <- brandseyer2:::list_to_v4_mentions(data)
  expect_equal(nrow(m), 2)
  expect_equal(m[[1, "id"]] , "one")
  expect_equal(m[[1, "brands"]], c(1, 2))
  expect_equal(m[[1, "sentiment"]], 0)
  expect_equal(m[[1, "tags"]], c(1))
  expect_equal(nrow(m[[1, "mediaLinks"]]), 1)
  expect_equal(m[[1, "mediaLinks"]]$url, "http://bob.com")
  expect_equal(m[[1, "mediaLinks"]]$mimeType, "application/json")
  expect_equal(m[[1, "socialNetwork"]], "TWITTER")
  expect(m[[1, "published"]] == lubridate::ymd_hms(20171121213735), "Wrong published time")
  expect(m[[1, "pickedUp"]] == lubridate::ymd_hms(20171122213735), "Wrong pickedup time")
  expect(m[[1, "updated"]] == lubridate::ymd_hms(20171123213735), "Wrong updated time")

  expect_equal(m[[2, "id"]], "two")
  expect_equal(m[[2, "tags"]], NULL)
  expect_equal(m[[2, "mediaLinks"]], tibble::tibble(url = NA, mimeType = NA))
})

test_that("Always needs a filter to get mentions", {
  expect_error(account("TEST01AA") %>% mentions(), "*filter*")
  expect_error(account("TEST01AA") %>% mentions(filter = ""), "*filter*")
})

test_that("Can read mention data for an account", {
  m <- account("TEST01AA") %>%
    mentions("an arbitrary filter")

  expect_is(attr(m, "account"), "brandseyer2.account")
  expect_gt(nrow(m), 0)
})

test_that("Can handle reading no data", {
  result <- brandseyer2:::list_to_v4_mentions(list())

  expect_equal(result, tibble::tibble())
})
