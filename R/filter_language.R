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
    filter_and_warn_v4() %>%
    map(~ .x %>% filter_mentions(filter)) %>%
    purrr::reduce(merge_query)
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
  comparison <- list(...)

  compare_mentions_impl(to_query(.account), comparison)
}

compare_mentions.list <- function(.account, ...) {
  comparison <- list(...)

  .account %>%
    filter_and_warn_v4() %>%
    map(to_query) %>%
    purrr::reduce(merge_query) %>%
    compare_mentions_impl(comparison)
}

compare_mentions.character <- function(.account, ...) {
  stop("sdf")
}

compare_mentions.brandseyer2.query <- function(.account, ...) {
  comparison <- list(...)
  compare_mentions_impl(.account, comparison)
}

compare_mentions_impl <- function(query, comparison) {
  assert_that(length(comparison) >= 1, msg = "No comparison filters supplied")
  assert_that(all(map_lgl(comparison, is.string)), msg = "Comparison filters must be strings")

  query <- copy_query(query)
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

#----------------------------------------------------------
# Utilities

filter_and_warn_v4 <- function(accounts) {
  bad_version <- keep(accounts, ~ account_api_version(.x) != 'V4')
  bad_brand <- keep(accounts, ~ nrow(root_brands(.x)) == 0)

  if (rlang::is_empty(bad_version) && rlang::is_empty(bad_brand))
    return(accounts)

  if (!rlang::is_empty(bad_version)) {
    message <- bad_version %>%
      map(account_code) %>%
      stringr::str_flatten(collapse = ", ") %>%
      { glue::glue("The following accounts are not V4: {.}. Ignoring them.")}



    if (rlang::is_string(message))
      rlang::warn(message)
  }


  if (!rlang::is_empty(bad_brand)) {
    message <- bad_brand %>%
      map(account_code) %>%
      stringr::str_flatten(collapse = ", ") %>%
      { glue::glue("The following accounts have no root brands: {.}. Ignoring them.")}



    if (rlang::is_string(message))
      rlang::warn(message)
  }

  keep(accounts, ~ account_api_version(.x) == 'V4' && nrow(root_brands(.x)) != 0)
}
