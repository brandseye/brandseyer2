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

#' Read account rules
#'
#' Returns a tibble of all the rules for an account. Rules are
#' applied to mentions as they are received by BrandsEye.
#'
#' Rules can be read from various different kinds of object. An [account()]
#' object is the most basic, although rules can also be read from a list of
#' account objects (in which case the returned tibble will include an account code column),
#' or from a tibble containing an account code column, such as that returned
#' by [account_list()].
#'
#' @param x               An object to read logs from.
#' @param .show.progress  A logical indicating whether to show a progress bar or not.
#'                        By default, this only shows a progress bar in interactive environments.
#'                        Also, progress bars will only be shown if 5 or more accounts are
#'                        being examined.
#'
#' @return A tibble of rule information,.
#' @export
rules <- function(x, .show.progress) {
  UseMethod("rules")
}

#' @describeIn rules
#'
#' For an [account()] object.
#'
#' @export
#' @examples
#' \dontrun{
#' account("TEST01AA") %>%
#'   rules()
#' }
rules.brandseyer2.account <- function(x, .show.progress = interactive()) {
  x %>%
    account_code() %>%
    rules(.show.progress = .show.progress)
}

#' @describeIn rules
#'
#' For a list of account objects.
#'
#' @export
#' @examples
#' \dontrun{
#' account("TEST01AA", "TEST02AA") %>%
#'   rules()
#' }
rules.list <- function(x, .show.progress = interactive()) {
  x %>%
    account_code() %>%
    rules(.show.progress = .show.progress)
}

#' @describeIn rules
#'
#' For a `data.frame` that contains an `account` column of account codes.
#'
#' @export
#' @examples
#' \dontrun{
#' account_list() %>%
#'   rules()
#' }
rules.data.frame <- function(x, .show.progress = interactive()) {
  assert_that(x %has_name% 'account',
              msg = "No account code column present in `x`")

  x$account %>%
    rules(.show.progress = .show.progress)
}

#' @describeIn rules
#'
#' For a character vector
#'
#' @export
#' @examples
#'
#' \dontrun{
#' rules("TEST01AA")
#' rules(c("TEST01AA", "TEST02AA"))
#' }
rules.character <- function(x, .show.progress = interactive()) {

  if (length(x) > 1) {
    pb <- list(tick = function(...) {})
    if (length(x) >= 5 && .show.progress) {
      pb <- progress::progress_bar$new(
        format = "  rules for :code [:bar] :percent eta: :eta",
        total = length(x)
      )
    }

    pb$tick(tokens = list(code = "starting"), len = 0)

    return(
      map_df(x, function(code) {
        on.exit(pb$tick(tokens = list(code = code)))
        rules(code) %>%
          mutate(account = code) %>%
          select(account, everything())
      })
    )
  }



  data <- read_mash(paste0("accounts/", x, "/rules"))

  macros <- map(data, function(d) {
    if (is.null(d$macro)) return(tibble())
    tibble(
      id = d$macro$id,
      name = d$macro$name,
      attributes = d$macro$attributes
    )
  })

  rules <- data %>%
    map_df(~ tibble(
      id = .x$id,
      name = .x$name,
      action = .x$action,
      filter = .x$filter,
      priority = .x$priority
    ))

  rules$macros <- macros
  rules
}
