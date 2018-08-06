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
# Adding brands

#' Add accounts to a query.
#'
#' [with_account()] lets you add additional accounts to a [query()]. It's
#' root brands and timezone are added, as appropriate. This is
#' part of the [query()] language.
#'
#' @param .query   A [query()] or [account()] to which to add this account.
#' @param account  The [account()] to add.
#'
#' @return A [query()] object
#' @export
#'
#' @seealso
#'
#' Other verbs for the query language: [group_mentions_by()],
#' [compare_mentions()], [with_fields()], [with_order()]
#'
#' [query()] is a way to manually create queries.
#'
#' @examples
#'
#' \dontrun{
#' account("TEST01AA") %>%
#'   filter_mentions("published inthelast week") %>%
#'   with_account("TEST03AA")
#'
#' account("TEST01AA") %>%
#'   filter_mentions("published inthelast week") %>%
#'   with_account(account("TEST03AA"))
#' }
with_account <- function(.query, account) {
  UseMethod("with_account")
}

#' @export
with_account.brandseyer2.query <- function(.query, account) {
  with_account_impl(account, .query)
}

#' @export
with_account.brandseyer2.account <- function(.query, account) {
  with_account_impl(account, to_query(.query))
}

with_account_impl <- function(.account, .query) {
  UseMethod("with_account_impl")
}

with_account_impl.character <- function(.account, .query) {
  account(.account) %>%
    with_account_impl(.query)
}

with_account_impl.brandseyer2.account <- function(.account, .query) {
  merge_query(.query, to_query(.account))
}

with_account_impl.list <- function(.account, .query) {
  .account %>%
    map(to_query) %>%
    reduce(merge_query)
}


#----------------------------------------------------------
# Selecting brands

with_brands <- function(.account, selector) {
  UseMethod("with_brands")
}

with_brands.brandseyer2.account <- function(.account, selector) {
  with_brands(list(.account), selector)
}

with_brands.list <- function(.account, selector) {
  b <- with_brands_impl(selector, account(.account))
  brands <- filter_brand_from_df(account_code(.account), b)

  if (interactive() && nrow(b) > 0) {
    message(paste("Selecting brands:", map(brands, purrr::partial(format, colour = FALSE))))
  }

  if (interactive() && nrow(b) == 0) {
    rlang::warn(glue::glue("No brands matched brand selector {selector}"))
  }

  q <- map(.account, to_query) %>% reduce(merge_query)
  q$brands <- brands
  q
}

with_brands_impl <- function(selector, account) {
  UseMethod("with_brands_impl")
}

with_brands_impl.numeric <- function(selector, account) {
  account %>%
    brands() %>%
    filter(id == selector)
}

with_brands_impl.character <- function(selector, account) {
  # For devtools::check
  name <- NULL;

  account %>%
    brands() %>%
    filter(grepl(selector, name))
}

#----------------------------------------------------------
# Filtering mentions


#' Filter for only specific mentions
#'
#' `filter_mentions()` causes counting and mention operations
#' to occur on only those mentions matching the filter. This is
#' part of the [query()] language.
#'
#' @param .account An account or [query()] object.
#' @param filter A filter string.
#'
#' @return A query object
#' @export
#'
#' @seealso
#'
#' Other verbs for the query language: [group_mentions_by()],
#' [compare_mentions()], [with_fields()], [with_order()], [with_account()].
#'
#' [query()] is a way to manually create queries.
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   filter_mentions("published inthelast week")
filter_mentions <- function(.account, filter) {
  UseMethod("filter_mentions")
}

#' @export
filter_mentions.brandseyer2.account <- function(.account, filter) {
  assert_that(is.string(filter))

  q <- to_query(.account)
  q$filter <- filter
  q
}

#' @export
filter_mentions.list <- function(.account, filter) {
  assert_that(is.string(filter))

  .account %>%
    filter_and_warn_v4() %>%
    map(~ .x %>% filter_mentions(filter)) %>%
    purrr::reduce(merge_query)
}

#' @export
filter_mentions.brandseyer2.query <- function(.account, filter) {
  assert_that(is.string(filter))

  query <- copy_query(.account)
  query$filter <- filter
  query
}

#----------------------------------------------------------
# Comparing mentions

#' Compare mentions using sub-filters when counting them
#'
#' [compare_mentions()] lets you group mentions together using
#' sub-filters when counting them using [count_mentions()]. This
#' is part of the [query()] language.
#'
#' @param .account An account or [query()] object.
#' @param ... A named set of sub-filters, such as
#'            `positive = sentiment = 1`. See the examples.
#'
#' @return A [query()] object.
#' @export
#'
#' @seealso
#'
#' Other verbs for the query language: [group_mentions_by()],
#' [filter_mentions()], [with_fields()], [with_order()], [with_account()].
#'
#' [query()] is a way to manually create queries.
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   compare_mentions(enterprise = "media is enterprise",
#'                    consume = "media is consumer")
compare_mentions <- function(.account, ...) {
  UseMethod("compare_mentions")
}

#' @export
compare_mentions.brandseyer2.account <- function(.account, ...) {
  comparison <- list(...)

  compare_mentions_impl(to_query(.account), comparison)
}

#' @export
compare_mentions.list <- function(.account, ...) {
  comparison <- list(...)

  .account %>%
    filter_and_warn_v4() %>%
    map(to_query) %>%
    purrr::reduce(merge_query) %>%
    compare_mentions_impl(comparison)
}

#' @export
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

#' Group mentions by shared criteria
#'
#' [group_mentions_by()] allows you to specify how to group
#' mentions when counting them, such as by day, sentiment, etc.
#' This is part of the [query()] language.
#'
#' @param .account An account or [query()] object.
#' @param ... A list of fields to group by.
#' @param .envir An optional environment to perform substitutions in.
#'
#' @return A [query()] object.
#' @export
#' @seealso
#'
#' Other verbs for the query language: [group_mentions_by()],
#' [compare_mentions()], [with_fields()], [with_order()], [with_account()].
#'
#' [query()] is a way to manually create queries.
#'
#' @examples
#'
#' account("TEST01AA", "TEST02AA") %>%
#'   filter_mentions("published inthelast week") %>%
#'   group_mentions_by(published, sentiment)
group_mentions_by <- function(.account, ..., .envir) {
  UseMethod("group_mentions_by")
}

#' @export
group_mentions_by.brandseyer2.account <- function(.account, ..., .envir = parent.frame()) {
  groupBy <- get_name_list(deparse(substitute(c(...))), env = .envir)

  .account %>%
    to_query %>%
    group_mentions_by_impl(groupBy)
}

#' @export
group_mentions_by.list <- function(.account, ..., .envir = parent.frame()) {
  groupBy <- get_name_list(deparse(substitute(c(...))), env = .envir)
  .account %>%
    filter_and_warn_v4() %>%
    map(to_query) %>%
    purrr::reduce(merge_query) %>%
    group_mentions_by_impl(groupBy)
}

#' @export
group_mentions_by.brandseyer2.query <- function(.account, ..., .envir = parent.frame()) {
  groupBy <- get_name_list(deparse(substitute(c(...))), env = .envir)
  group_mentions_by_impl(.account, groupBy)
}

group_mentions_by_impl <- function(query, groupBy) {
  query <- copy_query(query)
  query$grouping <- groupBy
  query
}

#----------------------------------------------------------
# Selecting extra fields

#' Select extra data when fetching or counting mentions
#'
#' [with_fields()] allows you to fetch additional data fields on mentions,
#' or to count additional fields when counting mentions. This is part of the
#' [query()] language.
#'
#' @param .account An account or [query()] object.
#' @param ... The fields to order by.
#' @param .envir An environment to perform substitutions in.
#'
#' @return A [query()] object
#' @export
#'
#' @seealso
#'
#' Other verbs for the query language: [group_mentions_by()],
#' [compare_mentions()], [filter_mentions()], [with_order()], [with_account()].
#'
#' [query()] is a way to manually create queries.
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   filter_mentions("published inthelast week") %>%
#'   with_fields(mentionCount, totalOTS)
with_fields <- function(.account, ..., .envir) {
  UseMethod("with_fields")
}

#' @export
with_fields.brandseyer2.account <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)

  .account %>%
    to_query %>%
    with_fields_impl(fields)
}

#' @export
with_fields.list <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)
  .account %>%
    filter_and_warn_v4() %>%
    map(to_query) %>%
    purrr::reduce(merge_query) %>%
    with_fields_impl(fields)
}

#' @export
with_fields.brandseyer2.query <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)
  with_fields_impl(.account, fields)
}

with_fields_impl <- function(query, fields) {
  query <- copy_query(query)
  query$fields <- fields
  query
}

#----------------------------------------------------------
# ordering fields

#' Order results from fetching or counting mentions
#'
#' [with_order()] allows you to specify the order of returned
#' mentions or the order of counted mentions. This does not
#' preclude you from reordering mentions using [dplyr::arrange()].
#' This is part of the [query()] language.
#'
#' @param .account An account or [query()] object.
#' @param ... The fields to order by.
#' @param .envir The environment to perform substitutions in.
#'
#' @return A [query()] object.
#' @export
#'
#' @seealso
#'
#' Other verbs for the query language: [group_mentions_by()],
#' [compare_mentions()], [filter_mentions()], [filter_mentions()], [with_account()].
#'
#' [query()] is a way to manually create queries.
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   filter_mentions("published inthelast week") %>%
#'   group_mentions_by(mentionCount, totalOTS) %>%
#'   with_order(totalOTS)
with_order <- function(.account, ..., .envir) {
  UseMethod("with_order")
}

#' @export
with_order.brandseyer2.account <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)

  .account %>%
    to_query %>%
    with_order_impl(fields)
}

#' @export
with_order.list <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)
  .account %>%
    filter_and_warn_v4() %>%
    map(to_query) %>%
    purrr::reduce(merge_query) %>%
    with_order_impl(fields)
}

#' @export
with_order.brandseyer2.query <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)
  with_order_impl(.account, fields)
}

with_order_impl <- function(query, fields) {
  query <- copy_query(query)
  query$ordering <- fields
  query
}

#-------------------------------------------------------------
# Utilities

# This filters out accounts that we can't query on,
# and provides nice, grouped, warning messages.
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
