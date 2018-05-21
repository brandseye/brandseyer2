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

#' Fetch account brands
#'
#' Fetches brand information, returned as a tibble, for the given
#' account. It includes brand names, associated phrases, and so on.
#'
#' @param accounts One or more account objects.
#'
#' @return A tibble of brand information
#' @export
#'
#' @author Constance Neeser
#'
#' @seealso \code{\link{root_brands}} for filtering to only the root brands of an account.
#'
#' @examples
#'
#' \dontrun{
#' # Fetch brand information
#' account("TEST01AA") %>%
#'   account_brands()
#'
#' # Find deleted parent brands
#' account("TEST01AA") %>%
#'   account_brands() %>%
#'   dplyr::filter(is.na(parent), deleted)
#'
#' # Fetch phrases without using account_phrases
#' account("TEST01AA") %>%
#'   account_brands() %>%
#'   dplyr::select(id, phrases) %>%
#'   dplyr::rename(brand.id = id) %>%
#'   tidyr::unnest(phrases) %>%
#'   dplyr::rename(phrase.id = id)
#' }
account_brands <- function(accounts) {
  UseMethod("account_brands")
}

#' @describeIn account_brands
#'
#' Read brands for only a single account
#'
#' @export
account_brands.brandseyer2.account <- function(accounts) {

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
          phrases = list(phrases)
        )
      })

    children <- brands %>%
      map(~recurse(.x$children, parent = .x$id))

    bind_rows(parents, children)
  }

  recurse(accounts$brands)
}

#' @describeIn account_brands
#'
#' Create a table of brands for the list of accounts given
#'
#' @export
#'
#' @examples
#'
#' accounts(c("TEST01AA", "TEST02AA")) %>%
#'   account_brands()
account_brands.list <- function(accounts) {
  accounts %>%
    map_df(~ .x %>%
             account_brands() %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}

