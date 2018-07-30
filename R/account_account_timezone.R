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

#' Get an account's timezone
#'
#' Returns a vector of timezones for the given accounts.
#' An account's data is reported in a particular timezone, set for that account.
#' All dates provided in filters are assumed to be given in that timezone.
#'
#' @param account An account object, or list of account objects.
#'
#' @return A character vector of timezone that account data is reported in.
#' @export
#' @author Constance Neeser
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   account_timezone()
#'
#' account("TEST01AA", "TEST02AA") %>%
#'   account_timezone()
account_timezone <- function(account) {
  UseMethod("account_timezone")
}

#' @export
account_timezone.brandseyer2.account <- function(account) {
  account$timezone
}

#' @export
account_timezone.list <- function(account) {
  map_chr(account, account_timezone)
}
