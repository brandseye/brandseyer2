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
#' Logs can be read from various different kinds of object. An [account()]
#' object is the most basic, although logs can also be read from a list of
#' account objects (in which case the returned tibble will include an account code column),
#' or from a tibble containing an account code column, such as that returned
#' by [account_list()].
#'
#' @param x               An object to read logs from.
#' @param from            A date indicating from when to read dates.
#' @param .show.progress  A logical indicating whether to show a progress bar or not.
#'                        By default, this only shows a progress bar in interactive environments.
#'                        Also, progress bars will only be shown if 3 or more accounts are
#'                        being examined.
#'
#' @return A tibble of log information, including the user that perform the action, and how many
#'         times the action was performed.
#' @export
logs <- function(x, from, .show.progress) {
  UseMethod("logs")
}

#' @describeIn logs
#'
#' Reach for an account code character vector.
#'
#' @export
#' @examples
#'
#' \dontrun{
#' logs("TEST01AA")
#' }
#'
logs.character <- function(x,
                           from = (Sys.Date() %m-% months(1)),
                           .show.progress = interactive()) {
  assert_that(lubridate::is.Date(from))
  assert_that(from <= Sys.Date(), msg = "`from` cannot be after today")

  if (length(x) > 1) {
    pb <- list(tick = function(...) {})
    if (length(x) >= 3 && .show.progress) {
      pb <- progress::progress_bar$new(
        format = "  logs for :code [:bar] :percent eta: :eta",
        total = length(x)
      )
    }

    pb$tick(tokens = list(code = "starting"), len = 0)

    return(
      map_df(x, function(code) {
        on.exit(pb$tick(tokens = list(code = code)))
        logs(code, from = from, .show.progress = .show.progress) %>%
               mutate(account = code) %>%
               select(account, everything())
      })
    )
  }

  getData <- function(offset) read_mash(paste0("accounts/", x, "/activity"),
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

  if (nrow(results) == 0) return(results)

  lastDate <- utils::tail(results$date, n = 1)

  offset <- 0
  while (lastDate > from) {
    offset <- offset + 100
    r <- getData(offset) %>%
      purrr::pluck("data") %>%
      packData()
    if (nrow(r) == 0) break
    lastDate <- utils::tail(results$date, n = 1)
    results <- dplyr::bind_rows(results, r)
  }


  results %>%
    filter(date >= from)
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
logs.brandseyer2.account <- function(x,
                                     from = (Sys.Date() %m-% months(1)),
                                     .show.progress = interactive()) {
  account_code(x) %>% logs(from = from, .show.progress = .show.progress)
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
logs.list <- function(x,
                      from = (Sys.Date() %m-% months(1)),
                      .show.progress = interactive()) {
  x %>%
    map_df(~ logs(.x, from = from, .show.progress = .show.progress) %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}

#' @describeIn logs
#'
#' Read for a data frame, such as returned by [account_list()]
#'
#' @export
#' @examples
#' \dontrun{
#' account_list() %>%
#'   logs()
#' }
logs.data.frame <- function(x,
                            from = (Sys.Date() %m-% months(1)),
                            .show.progress = interactive()) {
  assert_that(x %has_name% 'account',
              msg = "No account code column present in `x`")

  x$account %>%
    logs(from = from, .show.progress = .show.progress)
}
