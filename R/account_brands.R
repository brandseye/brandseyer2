# Copyright (c) 2018, Brandseye PTY (LTD)
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' @describeIn brands
#'
#' Read brands for only a single account
#'
#' @param short Only show the most important brand information.
#'
#' @export
#' @examples
#'
#' # Fetch brand information
#' account("TEST01AA") %>%
#'   brands()
#'
#' # Find deleted parent brands
#' account("TEST01AA") %>%
#'   brands() %>%
#'   dplyr::filter(is.na(parent), deleted)
#'
#' # Fetch phrases without using `phrases``
#' account("TEST01AA") %>%
#'   brands(short = FALSE) %>%
#'   dplyr::select(id, phrases) %>%
#'   dplyr::rename(brand.id = id) %>%
#'   tidyr::unnest(phrases) %>%
#'   dplyr::rename(phrase.id = id)
brands.brandseyer2.account <- function(x, ..., short = TRUE) {

  # For devtools::check
  id <- NULL; name <- NULL; deleted <- NULL; archived <- NULL; parent <- NULL;

  # Brands are stored in a recursive tree, so we need a recursive function.
  recurse <- function(brands, parent = NA) {
    brand_id <- c()
    brand_parent <- c()
    brand_name <- c()
    brand_description <- c()
    brand_tier <- c()
    brand_deleted <- c()
    brand_schema <- c()
    brand_filter <- c()
    brand_tt_id <- c()
    brand_sentiment_rate <- c()
    brand_topic_rate <- c()
    brand_archived <- c()
    brand_phrases <- list()


    for (brand in brands) {
      phrases <- NULL

      if (!short) {
        phrase_id <- c()
        phrase_query <- c()
        phrase_inactive <- c()
        phrase_deleted <- c()

        for (phrase in brand$phrases) {
          phrase_id <- c(phrase_id, phrase$id)
          phrase_query <- c(phrase_query, phrase$q)
          phrase_inactive <- c(phrase_inactive, phrase$inactive %||% FALSE)
          phrase_deleted <- c(phrase_deleted, phrase$deleted %||% FALSE)
        }

        phrases <-  if (is.null(phrase_id))
          tibble::tibble()
        else {
          tibble::tibble(id = phrase_id,
                         query = phrase_query,
                         inactive = phrase_inactive,
                         deleted = phrase_deleted)
        }
      }


      brand_id <- c(brand_id, brand$id)
      brand_parent <- c(brand_parent, parent)
      brand_name <- c(brand_name, brand$name)
      if (!short) brand_description <- c(brand_description, purrr::pluck(brand, "description", "en") %||% NA)
      if (!short) brand_tier <- c(brand_tier, brand$tier %||% NA)
      brand_deleted <- c(brand_deleted, brand$deleted %||% FALSE)
      if (!short) brand_schema <- c(brand_schema, brand$schema %||% NA)
      if (!short) brand_filter <- c(brand_filter, brand$mentionFilter %||% NA)
      if (!short) brand_tt_id <- c(brand_tt_id, brand$topicTreeId %||% NA)
      if (!short) brand_sentiment_rate <- c(brand_sentiment_rate, brand$crowdSamplePercentage %||% NA)
      if (!short) brand_topic_rate <-  c(brand_topic_rate, brand$crowdTopicPercentage %||% NA)
      brand_archived <- c(brand_archived, lubridate::ymd_hms(brand$archived %||% NA))
      if (!short) brand_phrases <- c(brand_phrases, list(phrases))
    }

    parents <- if (is.null(brand_id))
      tibble()
    else {
      tibble(
        id = brand_id,
        parent = brand_parent,
        name = brand_name,
        description = if (!short) brand_description else NA,
        tier = if (!short) brand_tier else NA,
        deleted = brand_deleted,
        schema = if (!short) brand_schema else NA,
        filter = if (!short) brand_filter else NA,
        topic_tree_id = if (!short) brand_tt_id else NA,
        sentiment_rate = if (!short) brand_sentiment_rate else NA,
        topic_rate = if (!short) brand_topic_rate else NA,
        archived = brand_archived,
        phrases = if (!short) brand_phrases else NA
      )
    }

    children <- brands %>%
      map(~recurse(.x$children, parent = .x$id))

    bind_rows(parents, children)
  }

  data <- recurse(x$brands)

  if (short) {
    data <- data %>%
      select(id, name, parent, deleted, archived)
  }


  data
}

#' @describeIn brands
#'
#' Create a table of brands for the list of accounts given
#'
#' @export
#'
#' @examples
#'
#' accounts(c("TEST01AA", "TEST02AA")) %>%
#'   brands()
brands.list <- function(x, ..., short = TRUE) {
  x %>%
    map_df(~ .x %>%
             brands(short = short) %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}

