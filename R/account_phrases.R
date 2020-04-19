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

#' @describeIn phrases
#'
#' Read phrases for only a single account
#'
#' @export
#'
#' @examples
#'
#' # Fetch phrases for a single account
#' account("TEST01AA") %>%
#'   phrases()
phrases.brandseyer2.account <- function(x, ...) {
  # Handle devtools::check notes
  phrases <- NULL
  brand.id <- NULL
  phrase.id <- NULL
  deleted <- NULL
  inactive <- NULL
  query <- NULL

  brands <- x %>% brands(short = FALSE)
  if (nrow(brands) == 0) return(tibble())

  brands %>%
    select(id, phrases) %>%
    rename(brand.id = id) %>%
    unnest_legacy(phrases = map(phrases, ~ .x %||% NA)) %>%
    rename(phrase.id = id) %>%
    select(phrase.id, everything()) %>%
    arrange(deleted, inactive, query)
}


#' @describeIn phrases
#'
#' Create a table of phrases for the list of accounts given
#'
#' @export
#'
#' @examples
#'
#' accounts(c("TEST01AA", "TEST02AA")) %>%
#'   phrases()
phrases.list <- function(x, ...) {
  x %>%
    map_df(~ .x %>%
             phrases() %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}
