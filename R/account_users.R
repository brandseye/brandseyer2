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

#' Get users for an account
#'
#' Returns a tibble of users for an account, or list of accounts.
#'
#' @param x An object to get users for, such as an account object.
#'
#' @return A tibble of users.
#' @export
users <- function(x) {
  UseMethod("users")
}

#' @describeIn users
#'
#' List users for an account object.
#'
#' @export
#' @examples
#' \dontrun{
#' account("TEST01AA") %>%
#'   users()
#' }
users.brandseyer2.account <- function(x) {
  users(account_code(x))
}

#' @describeIn users
#'
#' List users for accounts given as account codes.
#'
#' @export
#' @examples
#'
#' \dontrun{
#' users("TEST01AA")
#' users(c("TEST01AA", "TEST02AA"))
#' }
users.character <- function(x) {
  if (length(x) > 1) {
    return(
      map_df(x, function(code) {
        users(code) %>%
          mutate(account = code) %>%
          select(account, everything())
      })
    )
  }

  data <- read_mash(paste0("accounts/", x, "/users"))

  data %>%
    map_df(~tibble(
      id = .x$id,
      email = .x$email,
      name = .x$name,
      firstName = .x$firstName,
      lastName = .x$lastName,
      mainUser = .x$mainUser %||% FALSE,
      accessStatus = .x$accessStatus %||% FALSE,
      permissions = .x$permissions
    ))
}

#' @describeIn users
#'
#' List users for a list of account objects.
#'
#' @export
#' @examples
#' \dontrun{
#' account("TEST01AA", "TEST02AA") %>%
#'   users()
#' }
users.list <- function(x) {
  map_df(x, users)
}

#' @describeIn users
#'
#' List users for accounts coded in the `account` column of a tibble, such as
#' provided by [account_list()].
#'
#' @export
#' @examples
#' \dontrun{
#' account_list() %>%
#'   users()
#' }
users.data.frame <- function(x) {
  assert_that(x %has_name% "account", msg = "No `account` column present")

  x$account %>% users()
}
