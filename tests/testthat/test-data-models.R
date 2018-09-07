context("test-data-models")
library(brandseyer2)

test_that("Can read currencies", {
  mockr::with_mock(
    read_api = function(endpoint) {
      jsonlite::fromJSON(
        '[{
          "id": "USD",
          "label": "US Dollar"
         }]', simplifyVector = FALSE
      )
    }, {
      d <- data_model_currencies()
      expect_equal(nrow(d), 1)
      expect_equal(d$id, 'USD')
      expect_equal(d$name, 'US Dollar')
    }
  )
})

test_that("Can read languages", {
  mockr::with_mock(
    read_api = function(endpoint) {
      jsonlite::fromJSON(
        '[{
           "id": "ab",
           "name": "Abkhazian"
         }]', simplifyVector = FALSE
      )
    }, {
      d <- data_model_languages()
      expect_equal(nrow(d), 1)
      expect_equal(d$id, 'ab')
      expect_equal(d$name, 'Abkhazian')
    }
  )
})

test_that("Can read categories", {
  mockr::with_mock(
    read_api = function(endpoint) {
      jsonlite::fromJSON(
        '[{
           "id": "CONSUMER",
           "label": "Consumer"
         }]', simplifyVector = FALSE
      )
    }, {
      d <- data_model_categories()
      expect_equal(nrow(d), 1)
      expect_equal(d$id, 'CONSUMER')
      expect_equal(d$name, 'Consumer')
    }
  )
})

test_that("Can read countries", {
  mockr::with_mock(
    read_api = function(endpoint) {
      jsonlite::fromJSON(
        '[{
           "id": "AF",
           "name": "Afghanistan"
         }]', simplifyVector = FALSE
      )
    }, {
      d <- data_model_countries()
      expect_equal(nrow(d), 1)
      expect_equal(d$id, 'AF')
      expect_equal(d$name, 'Afghanistan')
    }
  )
})

