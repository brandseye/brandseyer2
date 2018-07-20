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

#' Read account logs
#'
#' Returns a tibble of logs for the account, in the last period stated.
#'
#' @param x An [account()] object or list of them.
#' @param from A date indicating from when to read dates.
#'
#' @return A tibble of log information, including the user that perform the action, and how many
#'         times the action was performed.
#' @export
logs <- function(x, from) {
  UseMethod("logs")
}

#' @describeIn logs
#'
#' For [account()] objects.
#'
#' @export
#' @examples
#'
#' \dontrun{
#' # Read logs for an account
#' account("TEST01AA") %>%
#'   logs()
#' }
#'
logs.brandseyer2.account <- function(x, from = (Sys.Date() %m-% months(1))) {
  assert_that(lubridate::is.Date(from))
  assert_that(from <= Sys.Date(), msg = "`from` cannot be after today")

  getData <- function(offset) read_mash(paste0("accounts/", account_code(x), "/activity"),
                                        query = list(limit = 100, offset = offset))

  packData <- function(data) {
    data %>%
      map_df(~ tibble(
        userId = .x$userId,
        userName = .x$userName,
        date = lubridate::ymd_hms(.x$date),
        description = .x$description,
        repeatCount = .x$repeatCount %||% NA
      ))
  }

  results <- getData(0) %>%
    purrr::pluck("data") %>%
    packData()

  lastDate <- utils::tail(results$date, n = 1)

  offset <- 0
  while (lastDate > from) {
    offset <- offset + 100
    r <- getData(offset) %>%
      purrr::pluck("data") %>%
      packData()
    lastDate <- utils::tail(results$date, n = 1)
    results <- dplyr::bind_rows(results, r)
  }


  results %>%
    filter(date >= from)
}

#' @describeIn logs
#'
#' For a list of accounts
#'
#' @export
#' @examples
#'
#' \dontrun{
#' # Read for a list of accounts
#' account("TEST01AA", "TEST02AA") %>%
#'   logs()
#' }
logs.list <- function(x, from = (Sys.Date() %m-% months(1))) {
  x %>%
    map_df(~ logs(.x, from = from) %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}
