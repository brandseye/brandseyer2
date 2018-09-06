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
#' [compare_mentions()], [with_mention_fields()], [with_mention_order()]
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

# Messages won't be show non-interactive. Can be suppressed
# when interactive.
with_brands <- function(.account, selector) {
  UseMethod("with_brands")
}

with_brands.brandseyer2.account <- function(.account, selector) {
  with_brands(list(.account), selector)
}

with_brands.brandseyer2.query <- function(.account, selector) {
  get_query_accounts(.account) %>%
    account() %>%
    with_brands(selector)
}

with_brands.list <- function(.account, selector) {
  b <- with_brands_impl(selector, account(.account))
  brands <- filter_brand_from_df(account_code(.account), b)

  # Provide information about what changed.
  if (interactive() && nrow(b) > 0) {
    m <- glue("Selecting brands: ",
              map_chr(brands, partial(format, colour = FALSE)) %>%
                stringr::str_flatten(", "))

    if (nchar(m) > cli::console_width()) {
      acs <- map(brands, ~ .x$code) %>% unique() %>% length()
      m <- glue("Selected {length(brands)} brands across {acs} accounts")
    }

    message(m)
  }

  if (interactive() && nrow(b) == 0) {
    rlang::warn(glue("No brands matched brand selector {selector}"))
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
  selector %<>%
    stringr::str_replace_all("\\s+", "\\\\s*") %>%
    stringr::str_replace_all("\\(", "\\\\(") %>%
    stringr::str_replace_all("\\)", "\\\\)")

  account %>%
    brands() %>%
    filter(grepl(selector, name, ignore.case = TRUE))
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
#' [compare_mentions()], [with_mention_fields()], [with_mention_order()], [with_account()].
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
#' [filter_mentions()], [with_mention_fields()], [with_mention_order()], [with_account()].
#'
#' [query()] is a way to manually create queries.
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   compare_mentions(enterprise = "media is enterprise",
#'                    consumer = "media is consumer")
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

#' @rdname compare_mentions
#'
#' @param comparisons A named list of comparison subfilters.
#'
#' @details [compare_mentions_raw()] allows you to construct a list
#' of comparison filters programmatically, and apply them to the query.
#'
#' @export
#'
#' @examples
#' account("TEST01AA") %>%
#'   compare_mentions_raw(list(enterprise = "media is ENTERPRISE",
#'                             consumer = "media is CONSUMER"))
compare_mentions_raw <- function(.account, comparisons) {
  UseMethod("compare_mentions_raw")
}

#' @export
compare_mentions_raw.brandseyer2.account <- function(.account, comparisons) {
  to_query(.account) %>%
    compare_mentions_impl(comparisons)
}

#' @export
compare_mentions_raw.list <- function(.account, comparisons) {
  .account %>%
    filter_and_warn_v4() %>%
    map(to_query) %>%
    purrr::reduce(merge_query) %>%
    compare_mentions_impl(comparisons)
}

#' @export
compare_mentions_raw.brandseyer2.query <- function(.account, comparisons) {
  compare_mentions_impl(.account, comparisons)
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
#' [compare_mentions()], [with_mention_fields()], [with_mention_order()], [with_account()].
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
#' [with_mention_fields()] allows you to fetch additional data fields on mentions,
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
#' [compare_mentions()], [filter_mentions()], [with_mention_order()], [with_account()].
#'
#' [query()] is a way to manually create queries.
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   filter_mentions("published inthelast week") %>%
#'   with_mention_fields(mentionCount, totalOTS)
with_mention_fields <- function(.account, ..., .envir) {
  UseMethod("with_mention_fields")
}

#' @export
with_mention_fields.brandseyer2.account <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)

  .account %>%
    to_query %>%
    with_mention_fields_impl(fields)
}

#' @export
with_mention_fields.list <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)
  .account %>%
    filter_and_warn_v4() %>%
    map(to_query) %>%
    purrr::reduce(merge_query) %>%
    with_mention_fields_impl(fields)
}

#' @export
with_mention_fields.brandseyer2.query <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)
  with_mention_fields_impl(.account, fields)
}

with_mention_fields_impl <- function(query, fields) {
  query <- copy_query(query)
  query$fields <- fields
  query
}

#----------------------------------------------------------
# ordering fields

#' Order results from fetching or counting mentions
#'
#' [with_mention_order()] allows you to specify the order of returned
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
#'   with_mention_order(totalOTS)
with_mention_order <- function(.account, ..., .envir) {
  UseMethod("with_mention_order")
}

#' @export
with_mention_order.brandseyer2.account <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)

  .account %>%
    to_query %>%
    with_mention_order_impl(fields)
}

#' @export
with_mention_order.list <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)
  .account %>%
    filter_and_warn_v4() %>%
    map(to_query) %>%
    purrr::reduce(merge_query) %>%
    with_mention_order_impl(fields)
}

#' @export
with_mention_order.brandseyer2.query <- function(.account, ..., .envir = parent.frame()) {
  fields <- get_name_list(deparse(substitute(c(...))), env = .envir)
  with_mention_order_impl(.account, fields)
}

with_mention_order_impl <- function(query, fields) {
  query <- copy_query(query)
  query$ordering <- fields
  query
}

#-------------------------------------------------------------
# Utilities

# This filters out accounts that we can't query on,
# and provides nice, grouped, warning messages.
filter_and_warn_v4 <- function(accounts) {
  bad_version <- c()
  good <- list()
  for (account in accounts) {
    if (account_api_version(account) != 'V4')
      bad_version <- c(bad_version, account_code(account))
    else
      good <- c(list(account), good)
  }

  if (!rlang::is_empty(bad_version)) {
    message <- bad_version %>%
      stringr::str_flatten(collapse = ", ") %>%
      { glue("The following accounts are not V4: {.}. Ignoring them.")}

    if (nchar(message) > cli::console_width())
      message <- bad_version %>%
        utils::head(2) %>%
        stringr::str_flatten(collapse = ", ") %>%
        { glue("The following accounts are not V4: {.} and {length(bad_version) - 2} others. Ignoring them.")}

    rlang::warn(message)
  }



  good
}
