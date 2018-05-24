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

#' Fetch tags for accounts and mention data
#'
#' Fetches tag information, returned as a tibble, for the given
#' account. This can be done for both account information, as well as
#' mentions.
#'
#' @param x An object to find tag information for, such as an \code{\link{account}}
#'          or a tibble of mention data.
#' @param ... Extra arguments passed to functions.
#'
#' @return A tibble of tag information. Includes the name, namespace, and description of the
#' tags. Note that topics are stored in the 'topics' namespace.
#' @export
#'
#' @seealso \code{\link{account}}() to fetch account data.
#' @seealso \code{\link{topics}}() to see just a list of topics.
#' @seealso \code{\link{mentions}}() to query mention data.
#'
#' @author Constance Neeser
tags <- function(x, ...) {
  UseMethod("tags")
}
