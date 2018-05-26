context("test-mentions.R")
library(brandseyer2)

test_that("Can parse a minimal list of mention data", {
  data <- list(
    list(id = "one",
         sentiment = as.integer(0),
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
         ))
  )

  m <- brandseyer2:::list_to_v4_mentions(data)
  expect_equal(nrow(m), 1)
  expect_equal(m[[1, "id"]] , "one")
  expect_equal(m[[1, "brands"]], c(1, 2))
  expect_equal(m[[1, "sentiment"]], 0)
  expect_equal(m[[1, "tags"]], c(1))
  expect_equal(nrow(m[[1, "mediaLinks"]]), 1)
  expect_equal(m[[1, "mediaLinks"]]$url, "http://bob.com")
  expect_equal(m[[1, "mediaLinks"]]$mimeType, "application/json")
  expect_equal(m[[1, "socialNetwork"]], "TWITTER")
})
