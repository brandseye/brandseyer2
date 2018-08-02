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

#' Access account information
#'
#' Returns an account object, representing the account that you're
#' performing actions on in brandseyer2.
#'
#' Typically you may ask for only a single account, by passing
#' a single account code as an argument. You can also pass in multiple
#' account codes, or even a vector of account codes, to be given a list
#' of account objects to act on.
#'
#' @param codes A vector of one or more account codes, or possibly
#'              a tibble (with an `account` column, such as from [account_list()])
#' @param ...   Additional account codes.
#'
#' @return An account object if one account code is given, or a list of account objects,
#'         one for each code given, in the same order as the codes were given.
#'
#' @aliases accounts
#'
#' @seealso [accounts()] is a synonym for `account`.
#' @seealso [is.account()] to test if an object is an account object.
#' @seealso [tags()] to find tags in an account.
#' @seealso [topics()] to find topics in an account.
#' @seealso [create_tags()] to add more tags to an account.
#' @seealso [topic_trees()] to find the topic trees in an account.
#' @seealso [logs()] to read the account's logs.
#' @seealso [users()] to see what users have access to the account.
#'
#' @export
account <- function(codes, ...) {
  UseMethod("account")
}

#' @describeIn account
#'
#' Fetch counts for one or more account codes given as a character vector.
#'
#' @param .show.progress A logical indicating whether a progress bar should
#'        be shown or not. By default, it will only be shown on interactive sessions.
#'        Further, no matter this parameter's value, the bar will only be shown
#'        when fetching account information for more than five accounts.
#'
#' @section Interactive use:
#'
#' When reading multiple accounts at a time, this method will by defualt
#' show a progress bar in interactive environments (such as RStudio). You
#' can control this behaviour using the `.show.progress` parameter.
#'
#' @export
#'
#' @examples
#'
#' # Read one account
#' account("TEST01AA")
#'
#' # Read two accounts
#' account("TEST01AA", "TEST02AA")
#'
#' # Read accounts from a vector
#' account(c("TEST01AA", "TEST02AA"))
account.character <- function(codes, ..., .show.progress = interactive()) {
  codes <- c(codes, ...)

  if (length(codes) > 1) {
    # Set up the progress bar.
    pb <- list(tick = function(...) {})
    if (length(codes) >= 5 && .show.progress) {
      pb <- progress::progress_bar$new(
        format = "  account :code [:bar] :percent eta: :eta",
        total = length(codes)
      )
    }

    pb$tick(tokens = list(code = "starting"), len = 0)
    return(map(codes, function(code) {
      on.exit(pb$tick(tokens = list(code = code)))
      account(code)
    }))
  }

  # See if we have any internal data that we can load
  if (startsWith(codes, "TEST") && nchar(codes) == 8) {
    if (exists(tolower(codes))) {
      result <- get(tolower(codes))
      if (!is.null(result)) return(result)
    }
  }

  read_account(codes) %>%
    create_account()
}

#' @describeIn account
#'
#' Get account information from a data.frame. The data.frame
#' should have an `account` column holding the account's code.
#' [account_list()] is a function that returns just such a data.frame.
#'
#' @export
account.data.frame <- function(codes, ..., .show.progress = interactive()) {
  assert_that(codes %has_name% "account",
              msg = "No `account` column in data.frame")

  codes$account %>%
    account(.show.progress = .show.progress)
}


#' @export
accounts <- account

# Given a data list, this will create the appropriate data structure.
create_account <- function(data) {
  storage_class <- paste0("brandseyer2.account.", tolower(data$storage))
  structure(data,
            class = c(storage_class, "brandseyer2.account"))
}

#' @export
print.brandseyer2.account <- function(x, ...) {
  cat(format(x), "\n")
}

#' @export
format.brandseyer2.account <- function(x, ...) {
  paste(
    "BrandsEye Account", "\n",
    crayon::silver(" Name:\t"), account_name(x), "\n",
    crayon::silver(" Code:\t"), account_code(x)
  )
}

