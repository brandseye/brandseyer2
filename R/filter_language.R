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

#----------------------------------------------------------
# Filtering mentions

filter_mentions <- function(.account, filter) {
  UseMethod("filter_mentions")
}

filter_mentions.brandseyer2.account <- function(.account, filter) {
  assert_that(is.string(filter))

  q <- to_query(.account)
  q$filter <- filter
  q
}

filter_mentions.list <- function(.account, filter) {
  assert_that(is.string(filter))

  .account %>%
    map(~ .x %>% filter_mentions(filter)) %>%
    purrr::reduce(merge_query)
}

# Possibly bad, as doesn't check if an account is V4.
filter_mentions.character <- function(.account, filter) {
  assert_that(is.string(filter))

  query(accounts = .account, filter = filter)
}

filter_mentions.brandseyer2.query <- function(.account, filter) {
  assert_that(is.string(filter))

  query <- copy_query(.account)
  query$filter <- filter
  query
}

#----------------------------------------------------------
# Comparing mentions

compare_mentions <- function(.account, ...) {
  UseMethod("compare_mentions")
}

compare_mentions.brandseyer2.account <- function(.account, ...) {
  .account %>%
    account_code() %>%
    compare_mentions(...)
}

compare_mentions.list <- function(.account, ...) {

  .account %>%
    account_code() %>%
    compare_mentions(...)
}

compare_mentions.character <- function(.account, ...) {
  comparison <- list(...)
  assert_that(length(comparison) >= 1, msg = "No comparison filters supplied")
  assert_that(all(map_lgl(comparison, is.string)), msg = "Comparison filters must be strings")

  q <- to_query(.account)
  q$comparison <- comparison
  q
}

compare_mentions.brandseyer2.query <- function(.account, ...) {
  comparison <- list(...)
  assert_that(length(comparison) >= 1, msg = "No comparison filters supplied")
  assert_that(all(map_lgl(comparison, is.string)), msg = "Comparison filters must be strings")

  query <- copy_query(.account)
  query$comparison <- comparison
  query
}

#----------------------------------------------------------
# Grouping mentions

group_mentions_by <- function(.account, ...) {
  UseMethod("group_mentions_by")
}

#----------------------------------------------------------
# Selecting extra fields

with_fields <- function(.account, ...) {
  UseMethod("with_fields")
}
