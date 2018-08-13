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

#' Finding retrosends for accounts
#'
#' [logs_retrosent()] can be used to find retrosends that have occurred
#' for a particular [account()] or list of accounts. If you want
#' to specify a date range, use [logs()] and pipe its results
#' through to [logs_retrosent()].
#'
#' @param x                A data frame of logs, or an account object (or list of account objects)
#' @param ...              Extra arguments
#' @param .show.progress   When reading logs, whether to show a progress bar or not. See [logs()].
#'
#' @return A tibble giving retrosend information, such as the crowd, priority and filter used.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' account("TEST01AA") %>%
#'   logs_retrosent()
#'
#' account_list() %>%
#'   logs() %>%
#'   logs_retrosent()
#'
#' # Pull date for a particular time range
#' account("TEST01AA") %>%
#'   logs(Sys.Date() - lubridate::weeks(1)) %>%
#'   logs_retrosent()
#'
#' # Pull data from a particular date up to today.
#' account("TEST01AA") %>%
#'   logs(lubridate::ymd("2018/08/01")) %>%
#'   logs_retrosent()
#' }
logs_retrosent <- function(x, ...) {
  UseMethod("logs_retrosent")
}

#' @describeIn logs_retrosent
#'
#' Reading from a data.frame or tibble of log information, such as that given
#' by [logs()].
#'
#' @export
logs_retrosent.data.frame <- function(x, ...) {
  # For devtools

  . <- NULL; description <- NULL; userId <- NULL; userName <- NULL;
  crowd <- NULL; priority <- NULL;

  x %>%
    filter(startsWith(description, "Sending ")) %>%
    mutate(crowd = stringr::str_extract(description, "crowd \\d+") %>%
             stringr::str_split("\\s+") %>%
             map_int(~(if (length(.x) == 2) as.integer(.x[[2]]) else NA)),
           priority = stringr::str_extract(description, "\\w+ priority") %>%
             stringr::str_split("\\s+") %>%
             map_chr(~ toupper(.x[[1]])),
           filter = stringr::str_extract(description, ":\\s+.*") %>%
             stringr::str_split_fixed("\\s+", 2) %>%
             .[, 2]) %>%
    mutate(filter = ifelse(nchar(filter) == 0, NA, filter)) %>%
    select(matches("account"), userId, userName, date, crowd, priority, filter)
}

#' @describeIn logs_retrosent
#'
#' From a vector of account codes.
#'
#' @export
logs_retrosent.character <- function(x, ..., .show.progress = interactive()) {
  x %>%
    logs(.show.progress = .show.progress) %>%
    logs_retrosent()
}

#' @describeIn logs_retrosent
#'
#' From an account object
#'
#' @export
logs_retrosent.brandseyer2.account <- function(x, ...) {
  x %>%
    logs() %>%
    logs_retrosent()
}

#' @describeIn logs_retrosent
#'
#' From a list of account objects.
#'
#' @export
logs_retrosent.list <- function (x, ..., .show.progress = interactive()) {
  x %>%
    account_code() %>%
    logs(.show.progress = .show.progress) %>%
    logs_retrosent()
}
