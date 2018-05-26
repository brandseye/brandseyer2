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
#' @param codes A vector of one or more account codes, or possibly
#'              a tibble, as provided by \code{\link{account_list}}
#'
#' @return An account object if one account code is given, or a list of account objects,
#'         one for each code given, in the same order as the codes were given.
#'
#' @aliases accounts
#'
#' @seealso \code{\link{accounts}} is a synonym for \code{account}.
#' @seealso \code{\link{is.account}} to test if an object is an account object.
#'
#' @export
#' @examples
#'
#' # Read two accounts, returned as a list.
#' accounts(c("TEST01AA", "TEST02AA"))
account <- function(codes) {
  UseMethod("account")
}

#' @describeIn account
#'
#' Fetch counts for one or more account codes given as a character vector.
#'
#' @export
#'
#' @examples
#'
#' # Read one account
#' account("TEST01AA")
account.character <- function(codes) {
  if (length(codes) > 1) {
    return(map(codes, account))
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
#' Get account information from the list of accounts available to from \code{\link{account_list}}
#'
#' @export
account.brandseyer2.account_list <- function(codes) {
  account(codes$account)
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
  format(paste(
    "BrandsEye Account", "\n",
    " Name: ", account_name(x), "\n",
    " Code: ", account_code(x)
  ))
}

