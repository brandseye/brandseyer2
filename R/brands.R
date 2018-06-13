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

#' Fetch brand information
#'
#' Fetches brand information for accounts and for mention data.
#' This is returned returned as a tibble. For account objects,
#' this returns the brands that make up the account. For mention data,
#' this returns the brands that are associated with the mention.
#'
#' @param x An object to fetch brand data for, such as an [account()]
#'          or tibble of mention data.
#' @param ... Additional parameters for functions.
#'
#' @return A tibble of brand information
#' @export
#'
#' @author Constance Neeser
#'
#' @seealso [root_brands()] for filtering to only the root brands of an account.
brands <- function(x, ...) {
  UseMethod("brands")
}
