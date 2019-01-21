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

  m <- brandseyer2:::list_to_v4_mentions(data, TRUE)
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

test_that("Only mash can see certain fields", {
  data <- list(
    list(id = "one",
         sentiment = 0L,
         published = "2017-11-21T21:37:35.000+0000",
         site = 'twitter.com',
         extract = "extract",
         postExtract = 'extract',
         postExtractHtml = 'extract',
         extractHtml = 'extract',
         combinedHtml = 'extract',
         content = 'extract',
         contentHtml = 'extract'
    ),
    list(id = "two",
         sentiment = 1L,
         site = 'facebook.com',
         extract = "extract",
         postExtract = 'extract',
         postExtractHtml = 'extract',
         extractHtml = 'extract',
         combinedHtml = 'extract',
         content = 'extract',
         contentHtml = 'extract'
    )
  )

  admin <- brandseyer2:::list_to_v4_mentions(data, TRUE)
  expect_equal(admin$extract, c("extract", "extract"))
  expect_equal(admin$postExtract, c("extract", "extract"))
  expect_equal(admin$postExtractHtml, c("extract", "extract"))
  expect_equal(admin$extractHtml, c("extract", "extract"))
  expect_equal(admin$combinedHtml, c("extract", "extract"))
  expect_equal(admin$content, c("extract", "extract"))
  expect_equal(admin$contentHtml, c("extract", "extract"))

  regular <- brandseyer2:::list_to_v4_mentions(data, FALSE)
  expect_equal(regular$extract, c(NA, "extract"))
  expect_equal(regular$postExtract, c(NA, "extract"))
  expect_equal(regular$postExtractHtml, c(NA, "extract"))
  expect_equal(regular$extractHtml, c(NA, "extract"))
  expect_equal(regular$combinedHtml, c(NA, "extract"))
  expect_equal(regular$content, c(NA, "extract"))
  expect_equal(regular$contentHtml, c(NA, "extract"))
})

test_that("if site not included, no data returned", {
  data <- list(
    list(id = "one",
         sentiment = 0L,
         published = "2017-11-21T21:37:35.000+0000",
         extract = "extract"
    )
  )

  admin <- brandseyer2:::list_to_v4_mentions(data, TRUE)
  expect_equal(admin$extract, "extract")

  regular <- suppressWarnings(brandseyer2:::list_to_v4_mentions(data, FALSE))
  expect_equal(regular$extract, NA)

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
  result <- brandseyer2:::list_to_v4_mentions(list(), TRUE)

  expect_equal(result, tibble::tibble())
})

test_that("Error reported for reaching v3 accounts", {
  data <- structure(list(id = 1), class = "brandseyer2.account.v3")

  expect_error(mentions(data, "published inthelast week"), regexp = "only supports V4 accounts")
})

test_that("Can annotate mentions with tags", {
  m <- tibble::tribble(
    ~id,     ~tags,
    "1-1",   c(1L, 2L),
    "1-2",   NULL,
    "1-3",   1L
  )

  attr(m, "account") <- account("TEST01AA")

  t <- m %>% tags()

  expect_equal(nrow(t), 3)
  expect_equal(t$id, c("1-1", "1-1", "1-3"))
  expect_equal(t$tag.id, c(1, 2, 1))
  expect_equal(names(t),
               c("id", "tag.id", "name", "namespace",
                 "description", "deleted", "forClassification", "children", "is_parent"))
})

test_that("Can annotate mentions with topics", {
  m <- tibble::tribble(
    ~id,     ~tags,
    "1-1",   c(11L, 12L),  # topics
    "1-2",   NULL,
    "1-3",   1L            # regular tag
  )

  attr(m, "account") <- account("TEST01AA")

  t <- m %>% topics()
  t_full <- m %>% topics(na.rm = FALSE)

  expect_equal(nrow(t), 2)
  expect_equal(t$id, c("1-1", "1-1"))
  expect_equal(t$topic.id, c(11, 12))
  expect_equal(names(t),
               c("id", "topic.id", "name", "namespace",
                 "description", "deleted", "forClassification", "children", "is_parent"))

  expect_equal(nrow(t_full), 4)
  expect_equal(t_full$id, c("1-1", "1-1", "1-2", "1-3"))
  expect_equal(t_full$topic.id, c(11, 12, NA, NA))
})
