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

#' Fetch account phrases
#'
#' Fetches phrase information, returned as a tibble, for the given
#' account. All of this information is included in \code{\link{account_brands}}
#'
#' @param accounts One or more account objects.
#'
#' @return A tibble of phrase information.
#' @export
#'
account_phrases <- function(accounts) {
  UseMethod("account_phrases")
}

#' @describeIn account_phrases
#'
#' Read phrases for only a single account
#'
#' @export
account_phrases.brandseyer2.account <- function(accounts) {
  # Handle devtools::check notes
  phrases <- NULL
  brand.id <- NULL
  phrase.id <- NULL

  brands <- accounts %>% account_brands()
  if (nrow(brands) == 0) return(tibble())

  brands %>%
    select(id, phrases) %>%
    rename(brand.id = id) %>%
    unnest(phrases) %>%
    rename(phrase.id = id) %>%
    select(phrase.id, everything())
}


#' @describeIn account_phrases
#'
#' Create a table of phrases for the list of accounts given
#'
#' @export
#'
#' @examples
#'
#' accounts(c("TEST01AA", "TEST02AA")) %>%
#'   account_phrases()
account_phrases.list <- function(accounts) {
  accounts %>%
    map_df(~ .x %>%
             account_phrases() %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}
