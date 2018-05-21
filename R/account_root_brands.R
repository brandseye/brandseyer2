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

#' Find root brands in an account.
#'
#' Root brands are the brands around which we collect mentions, and
#' determine sentiment. Sentiment is always towards the brand.
#'
#' @param x The object to find root brands for.
#' @param includeDeleted Whether to include deleted root brands.
#'
#' @return A tibble containing rows only for the root brands of an account. Otherwise,
#'         like the table returned in \code{\link{account_brands}}.
#' @export
#'
#' @author Constance Neeser
#'
#' @seealso \code{\link{account_brands}} for returning all brands in an account.
#'
#' @examples
#'
#' # Get root brands for an account
#' account("TEST01AA") %>%
#'   root_brands()
#'
#' # Get root brands from a tibble or data.frame
#' account("TEST01AA") %>%
#'   account_brands() %>%
#'   root_brands()
root_brands <- function(x, includeDeleted) {
  UseMethod("root_brands")
}

#' @describeIn root_brands
#'
#' Find root brands from a \code{tibble} or \code{data.frame}. The data.frame
#' should have  \code{id}, \code{parent}, and \code{deleted} columns, just as
#' the tibble returned from \code{\link{account_brands}} does.
#'
#' @export
root_brands.data.frame <- function(x, includeDeleted = FALSE) {
  assert_that(x %has_name% "id", msg = "data.frame has no `id` column for brand IDs")
  assert_that(x %has_name% "parent", msg = "data.frame has no `parent` column")
  assert_that(x %has_name% "deleted", msg = "data.frame has no `deleted` column")

  # For devtools::check
  deleted <- NULL; parent <- NULL

  x %>%
    filter(is.na(parent), includeDeleted | !deleted) %>%
    tibble::as_tibble()
}

#' @describeIn root_brands
#'
#' Returns root brand information from an account object.
#'
#' @export
root_brands.brandseyer2.account <- function(x, includeDeleted = FALSE) {
  x %>%
    account_brands() %>%
    root_brands(includeDeleted = includeDeleted)
}

#' @describeIn root_brands
#'
#' Returns root brand information from a list of account objects.
#'
#' @export
root_brands.list <- function(x, includeDeleted = FALSE) {
  x %>%
    account_brands() %>%
    root_brands(includeDeleted = includeDeleted)
}
