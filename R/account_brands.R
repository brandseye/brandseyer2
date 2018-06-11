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
#'   brands() %>%
#'   dplyr::select(id, phrases) %>%
#'   dplyr::rename(brand.id = id) %>%
#'   tidyr::unnest(phrases) %>%
#'   dplyr::rename(phrase.id = id)
brands.brandseyer2.account <- function(x, ...) {

  # Brands are stored in a recursive tree, so we need a recursive function.
  recurse <- function(brands, parent = NA) {
    parents <- brands %>%
      map_df(function(brand) {

        # Gather the phrases together
        phrases <- brand$phrases %>%
          map_df(function(phrase) {
            tibble(
              id = phrase$id,
              query = phrase$q,
              inactive = phrase$inactive %||% FALSE,
              deleted = phrase$deleted %||% FALSE
            )
          })

        tibble(
          id = brand$id,
          parent = parent,
          name = brand$name,
          tier = brand$tier %||% NA,
          deleted = brand$deleted %||% FALSE,
          schema = brand$schema %||% NA,
          filter = brand$mentionFilter %||% NA,
          topic_tree_id = brand$topicTreeId %||% NA,
          sentimentRate = brand$crowdSamplePercentage %||% NA,
          topicRate = brand$crowdTopicPercentage %||% NA,
          archived = lubridate::ymd_hms(brand$archived %||% NA),
          phrases = list(phrases)
        )
      })

    children <- brands %>%
      map(~recurse(.x$children, parent = .x$id))

    bind_rows(parents, children)
  }

  recurse(x$brands)
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
brands.list <- function(x, ...) {
  x %>%
    map_df(~ .x %>%
             brands() %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}

