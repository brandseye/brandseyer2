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
#' Provides a tibble of all BrandsEye accounts that are available to you. By default,
#' this will show only active accounts.
#'
#' By default this returns only active accounts. You can use the
#' `includeInactive` parameter to change this behaviour
#'
#' The returned tibble can be passed to some other functions, such as [account()],
#' to return information from across all accounts available to you.
#'
#' @param includeInactive Set to `TRUE` if you would like inactive accounts to also be returned.
#'
#' @return A tibble of accounts available to you.
#' @export
account_list <- function(includeInactive = FALSE) {
  query <- list(includeInactive = ifelse(includeInactive, "true", "false"))
  read_mash("accounts", query = query) %>%
    map_df(~tibble(
      account = .x$code,
      name = .x$name,
      status =.x$status,
      onlyAdmin = .x$onlyAdminLogin %||% FALSE,
      findNewMentions = .x$findNewMentions %||% FALSE,
      offline = .x$offline %||% FALSE,
      inactive = .x$inactive %||% FALSE,
      type = .x$accountType %||% NA
    ))
}
