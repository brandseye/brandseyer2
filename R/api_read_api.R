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

#' Read arbitrary data from our api.
#'
#' This reads arbitrary data from our api, including mention
#' data.
#'
#' @param endpoint The endpoint that you would like to access
#' @param query An httr query list
#'
#' @return the json content
#'
#' @author Constance Neeser
read_api <- function(endpoint, query = list()) {
  assert_that(is.string(endpoint))
  auth = whoami(raise_error = TRUE)

  url = paste0("https://api.brandseye.com/", endpoint)
  data <- httr::GET(url, httr::authenticate("API_KEY", auth$key), query = query)
  check_errors(data)
  return(httr::content(data))
}
