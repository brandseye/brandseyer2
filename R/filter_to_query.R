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

#' Create a query object from another object
#'
#' Creates a [query()] object from another object. This
#' can be a convenient way to create queries.
#'
#' @param x An object to turn in to a [query()] object.
#'
#' @return A query object
#' @export
#'
#' @examples
#'
#' to_query(account("TEST01AA"))
to_query <- function(x) {
  UseMethod("to_query")
}

#' @export
to_query.brandseyer2.account <- function(x) {
  if (account_api_version(x) != "V4") {
    rlang::warn(glue::glue("Account {account_code(x)} isn't V4. Ignoring."))
    return(query())
  }

  brands <- filter_brand_from_df(account_code(x), x %>% root_brands())

  query(accounts = account_code(x),
        brands = brands,
        timezones = account_timezone(x))
}

#' @export
to_query.brandseyer2.query <- function(x) x
