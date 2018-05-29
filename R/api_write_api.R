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

#' Write arbitrary data to our api.
#'
#' This reads arbitrary data from our api, including mention
#' data.
#'
#' @param endpoint The endpoint that you would like to access
#' @param method A string indicating the kind of write action. Possibly 'PUT' or 'POST'
#' @param json An object to serialise as json.
#' @param query An httr query list
#'
#' @return the json content
#'
#' @author Constance Neeser
write_api <- function(endpoint, method = "PUT", json = NULL, query = list()) {
  assert_that(is.string(endpoint))
  assert_that(method %in% c("PUT"))
  auth = whoami(raise_error = TRUE)

  url = paste0("https://api.brandseye.com/", endpoint)
  data <- httr::PUT(url,
                    httr::authenticate("API_KEY", auth$key),
                    query = query,
                    body = json, encode="json")
  check_errors(data)
  invisible()
}
