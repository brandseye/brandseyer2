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

#' List all accounts available to you.
#'
#' These accounts are provided as a tibble. The tibble can be
#' passed to some other functions, such as \code{\link{account}},
#' to return information from across all accounts available to you.
#'
#' @return A tibble of accounts available to you.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Fetch the tags from all accounts available to you.
#' account_list() %>%
#'   account_tags()
#' }
account_list <- function() {
  accounts <- read_mash("accounts") %>%
    map_df(~tibble(
      account = .x$code,
      name = .x$name,
      status =.x$status,
      onlyAdmin = .x$onlyAdminLogin %||% FALSE,
      findNewMentions = .x$findNewMentions %||% FALSE,
      offline = .x$offline %||% FALSE
    ))

  class(accounts) <- c("brandseyer2.account_list", class(accounts))
  accounts
}
