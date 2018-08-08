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

#' Create a query for mentions or counts
#'
#' [query()] creates a query to be used for fetching mentions, or
#' for counting them. You probably want to avoid using this directly,
#' and use one of the query verbs instead, such as [filter_mentions()].
#' [to_query()] is a convenient way to create a query object from
#' other things, such as an [account()].
#'
#' The query object is accepted by [count_mentions()] and [mentions()].
#' The advantage of using query and its various verbs is that it handles
#' adding root brands to your filter for you, as well as filtering out
#' mentions using older versions of the API, or who have no brands altogether.
#'
#' Of special note, V4 accounts will not be added to a query.
#'
#' @section Query verbs:
#'
#' Various functions can be chained together using the pipe operator as 'verbs'
#' in a query sentence.
#'
#' - [filter_mentions()] lets you choose what mentions to count or fetch.
#' - [group_mentions_by()] lets you group mentions when counting.
#' - [compare_mentions()] lets you count and compare mentions from subfilters.
#' - [with_fields()] lets you select extra information fields, such as OTS, or engagement.
#' - [with_order()] lets you order the results.
#'
#' @section Accessors:
#'
#' [get_query_brands()] returns a list of [filter_brand()] objects. This
#' prints nicely, for examining all brands in a query.
#' [get_query_accounts()] returns a vector of account codes. This prints
#' nicely, for examining all accounts involved in a query.
#'
#' @param accounts   A character vector of accounts to use.
#' @param brands     A list of brands to query against.
#' @param timezones  The timezones to report dates in when counting mentions.
#' @param filter     The filter to use when fetching or counting mentions.
#' @param comparison A list of subfilters to use when counting mentions.
#' @param fields     Extra fields to return when fetching or counting mentions.
#' @param grouping   How to group mentions when counting them.
#' @param ordering   The order to return results in.
#'
#' @return A query object
#' @export
#'
#' @seealso
#'
#' You can test if an object is a [query()] object using [is_query()].
#' [to_count_filter()] will allow you to convert a [query()] to
#' a vector of filters appropriate for use on [count_mentions()].
#' [to_mention_filter()] is similar, but for the [mentions()] function.
#'
#' @examples
#'
#' query(accounts = c("TEST01AA", "TEST02AA"),
#'       filter = "published inthelast week",
#'       timezone = "UTC",
#'       brands = list(filter_brand(1), filter_brand(2))
#' )
query <- function(accounts = NULL,
                  brands = NULL,
                  timezones = NULL,
                  filter = NULL,
                  comparison = NULL,
                  fields = NULL,
                  grouping = NULL,
                  ordering = NULL) {

  structure(list(accounts = accounts,
                 brands = brands,
                 timezones = timezones,
                 filter = filter,
                 comparison = comparison,
                 fields = fields,
                 grouping = grouping,
                 ordering = ordering),
            class = "brandseyer2.query")
}

copy_query <- function(q) {
  query(
    accounts = q$accounts,
    brands = q$brands,
    timezones = q$timezones,
    filter = q$filter,
    comparison = q$comparison,
    fields = q$fields,
    grouping = q$grouping,
    ordering = q$ordering
  )
}

merge_query <- function(lhs, rhs) {
  query(
    accounts = c(lhs$accounts, rhs$accounts),
    brands = c(lhs$brands, rhs$brands),
    timezones = c(lhs$timezones, rhs$timezones),
    filter = lhs$filter,
    comparison = lhs$comparison,
    fields = lhs$fields,
    grouping = lhs$grouping,
    ordering = lhs$ordering
  )
}


#' @export
print.brandseyer2.query <- function(x, ...) {
  cat(format(x), "\n")
}

#' @export
format.brandseyer2.query <- function(x, ...) {
  lines <- list("BrandsEye Query")
  width <- cli::console_width()

  if (!is.null(x$accounts)) {
    title_str <- "  accounts:\t"
    account_str <- stringr::str_flatten(x$accounts, collapse = ', ')
    if (stringr::str_length(account_str) + stringr::str_length(title_str) > width)
      account_str <- glue::glue("{stringr::str_flatten(x$accounts[1:2], collapse = ', ')} and {length(x$accounts) - 2} other accounts")

    account_str <- paste0(crayon::silver(title_str), account_str)
    lines <- c(lines, account_str)
  }
  if (!rlang::is_empty(x$brands)) {
    title_str <- "  brands:\t"
    brand_str <- x$brands %>% map_chr(~ format(.x, colour = FALSE)) %>% stringr::str_flatten(collapse = ", ")
    if (stringr::str_length(brand_str) + stringr::str_length(title_str) > width) {
      brand_str <- glue::glue("{length(x$brands)} brands selected")
    } else {
      brand_str <- x$brands %>% map_chr(~ format(.x)) %>% stringr::str_flatten(collapse = ", ")
    }

    brand_str = paste0(crayon::silver(title_str), brand_str)
    lines <- c(lines, brand_str)
  }
  if (!is.null(x$timezones)) {
    lines <- c(lines, glue::glue("  {crayon::silver('timezones:')}\t{stringr::str_flatten(unique(x$timezones), collapse = ', ')}"))
  }
  if (!is.null(x$filter)) {
    lines <- c(lines, glue::glue("  {crayon::silver('filter:')}\t{x$filter}"))
  }
  if (!is.null(x$comparison)) {
    lines <- c(lines, glue::glue("  {crayon::silver('comparison:')}\t{stringr::str_flatten(x$comparison, collapse = ', ')}"))
  }
  if (!is.null(x$fields)) {
    lines <- c(lines, glue::glue("  {crayon::silver('fields:')}\t{stringr::str_flatten(x$fields, collapse = ', ')}"))
  }
  if (!is.null(x$grouping)) {
    lines <- c(lines, glue::glue("  {crayon::silver('grouping:')}\t{stringr::str_flatten(x$grouping, collapse = ', ')}"))
  }
  if (!is.null(x$ordering)) {
    lines <- c(lines, glue::glue("  {crayon::silver('ordering:')}\t{stringr::str_flatten(x$ordering, collapse = ', ')}"))
  }

  paste(lines, collapse = "\n")
}


#' See brands in a query
#'
#' Returns a list of brands in a query. It is useful for
#' interactively displaying all the brands in a large query.
#'
#' @param query A [query()] object.
#'
#' @return A list of brands.
#' @export
#'
#' @examples
#'
#' q <- account("TEST01AA") %>%
#'   filter_mentions("published in thelast week")
#' get_query_brands(q)
#'
get_query_brands <- function(query) {
  l <- rep(query$brands %||% list(), 1)
  class(l) <- c("brandseyer2.query.brand_list", class(l))
  l
}

#' @export
format.brandseyer2.query.brand_list <- function(x, ...) {
  if (length(x) == 0) return(paste(clisymbols::symbol$bullet, "No brands"))
  map(x, ~ glue::glue("{clisymbols::symbol$bullet} {format(.x)}")) %>%
    stringr::str_flatten(collapse = "\n")
}

#' @export
print.brandseyer2.query.brand_list <- function(x, ...) {
  cat(format(x))
}

#' See the accounts in a query.
#'
#' Returns a vector of account codes in a query. Ideal for
#' printing all accounts being queried in large queries.
#'
#' @param query A [query()] object.
#'
#' @return A vector of account codes.
#' @export
#'
#' @examples
#'
#' q <- account("TEST01AA") %>%
#'   filter_mentions("published inthelast week")
#' get_query_accounts(q)
#'
get_query_accounts <- function(query) {
  l <- rep(query$accounts %||% list(), 1)
  class(l) <- c("brandseyer2.query.account_list", class(l))
  l
}

#' @export
format.brandseyer2.query.account_list <- function(x, ...) {
  if (length(x) == 0) return(paste(clisymbols::symbol$bullet, "No accounts"))
  map(x, ~ glue::glue("{clisymbols::symbol$bullet} {.x}")) %>%
    stringr::str_flatten(collapse = "\n")
}

#' @export
print.brandseyer2.query.account_list <- function(x, ...) {
  cat(format(x))
}
