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

#' Checking for errors
#'
#' Pass the results from httr calls to the API to examine errors
#' and report on them in a standard way.
#'
#' @param data Data returned from an `httr` call.
#'
#' @author Constance Neeser
check_errors <- function(data) {
  if (httr::status_code(data) == 401) stop(brandseye_auth_error())
  if (httr::status_code(data) != 200) {
    message = jsonlite::fromJSON(httr::content(data, "text"))$error
    stop(brandseye_api_error(message))
  }

  invisible()
}

brandseye_auth_error <- function() {
  condition(c("brandseye_auth_error", "error"),
            message = "BrandsEye Authentication Error: You have bad authentication details. Try authenticating again.")
}

brandseye_api_error <- function(message) {
  condition(c("brandseye_api_error", "error"),
            message = paste("BrandsEye API Error:", message),
            text = message)
}
