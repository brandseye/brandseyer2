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


#' Fetch mentions from an account
#'
#' This will give you access to mentions in an account.
#'
#' @note This function is mostly meant to read mentions from
#' newer BrandsEye accounts. Older accounts can still be read
#' using this function if you have the old \code{brandseyer}
#' library installed.
#'
#' @param x An account object
#' @param filter A query to match mentions against. See the filter vignette for details.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A tibble of mentions.
#' @export
mentions <- function(x, filter, ...) {
  UseMethod("mentions")
}

#' @describeIn mentions
#'
#' Reads V4 accounts. Returns a tibble of mentions.
#'
#' @export
mentions.brandseyer2.account.v4 <- function(x, filter, ...) {
  assertthat::assert_that(assertthat::is.string(filter))
  assertthat::assert_that(nchar(filter) > 0,
                          msg = "filter cannot be an empty character vector")

  query <- list(filter = filter)
  read_api(endpoint = paste0("v4/accounts/", account_code(x), "/mentions"),
           query = query)
}

#' @describeIn mentions
#'
#' Reads older, V3 accounts. To use this function, please ensure that
#' the older \code{brandseyer} library is installed as well. Returns
#' old \code{brandseyer} data structures, as though the \code{brandseyer::account_mentions}
#' function had been called.
#'
#' @param limit The maximum number of mentions to be returned
#' @param offset Mentions are returned in an order. Offset says how many of the
#'   first mentions should be skipped.
#' @param include A character vector of extra information to include in the mentions
#' @param select A character vector of the mention fields to be returned.
#' @param all Set to true if you would like to return all mentions from the account.
#'            This overides the \code{limit} parameter.

#'
#' @export
mentions.brandseyer2.account.v3 <- function(x, filter, ...,
                                            limit = 30, offset = 0,
                                            include, select,
                                            all = FALSE) {
  if (!is_installed("brandseyer")) {
    abort("brandseyer2 only supports V4 accounts. Please install `brandseyer`")
  }
  brandseyer::account_mentions(account_code(x), filter = filter,
                               limit = 30, offset = 0,
                               include = include, select = select, all = all)
}
