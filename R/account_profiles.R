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

#' Read online profiles for an account
#'
#' Returns a tibble of online profiles for the account.
#'
#' Online profiles can be read from various different kinds of object. An [account()]
#' object is the most basic, although profiles can also be read from a list of
#' account objects (in which case the returned tibble will include an account code column),
#' or from a tibble containing an account code column, such as that returned
#' by [account_list()].
#'
#' @param x               An object to read profiles from.
#' @param .show.progress  A logical indicating whether to show a progress bar or not.
#'                        By default, this only shows a progress bar in interactive environments.
#'                        Also, progress bars will only be shown if 3 or more accounts are
#'                        being examined.
#'
#' @return A tibble of log information, including the user that perform the action, and how many
#'         times the action was performed.
#' @export
profiles <- function(x, .show.progress) {
  UseMethod("profiles")
}

#' @describeIn profiles
#'
#' Reach for an account code character vector.
#'
#' @export
#' @examples
#'
#' \dontrun{
#' # Read profiles for an account code
#' profiles("TEST01AA")
#' }
#'
profiles.character <- function(x,
                           .show.progress = interactive()) {

  if (length(x) > 1) {
    pb <- list(tick = function(...) {})
    if (length(x) >= 3 && .show.progress) {
      pb <- progress::progress_bar$new(
        format = "  profiles for :code [:bar] :percent eta: :eta",
        total = length(x)
      )
    }

    pb$tick(tokens = list(code = "starting"), len = 0)

    return(
      map_df(x, function(code) {
        on.exit(pb$tick(tokens = list(code = code)))
        profiles(code, .show.progress = .show.progress) %>%
          mutate(account = code) %>%
          select(account, everything())
      })
    )
  }

  read_mash(paste0("accounts/", x, "/online-profiles")) %>%
    map_df(~tibble(
      id = .x$id,
      handle = .x$handle,
      handleId = .x$handleId,
      name =.x$name,
      authorId = .x$authorId,
      link = .x$link,
      type = .x$type,
      media = .x$media %||% NA,
      relevant = .x$relevant %||% NA,
      pictureLink = .x$pictureLink %||% NA,
      highPriority = .x$highPriority %||% FALSE,
      authorised = .x$authorized %||% FALSE,
      directMessagesEnabled = .x$directMessagesEnabled %||% FALSE,
      lastUpdated = lubridate::ymd_hms(.x$lastUpdated),
      lastUpdatedBy = .x$lastUpdatedBy$id,
      lastUpdatedByEmail = .x$lastUpdatedBy$email
    ))

}

#' @describeIn profiles
#'
#' For [account()] objects.
#'
#' @export
#' @examples
#'
#' \dontrun{
#' # Read profiles for an account
#' account("TEST01AA") %>%
#'   profiles()
#' }
#'
profiles.brandseyer2.account <- function(x, .show.progress = interactive()) {
  account_code(x) %>% profiles(.show.progress = .show.progress)
}

#' @describeIn profiles
#'
#' For a list of accounts
#'
#' @export
#' @examples
#'
#' \dontrun{
#' # Read for a list of accounts
#' account("TEST01AA", "TEST02AA") %>%
#'   profiles()
#' }
profiles.list <- function(x, .show.progress = interactive()) {
  x %>%
    account_code() %>%
    profiles()
}

#' @describeIn profiles
#'
#' Read for a data frame, such as returned by [account_list()]
#'
#' @export
#' @examples
#' \dontrun{
#' account_list() %>%
#'   profiles()
#' }
profiles.data.frame <- function(x, .show.progress = interactive()) {
  assert_that(x %has_name% 'account',
              msg = "No account code column present in `x`")

  x$account %>%
    profiles(.show.progress = .show.progress)
}
