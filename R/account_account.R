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
#'
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Read two accounts, returned as a list.
#' accounts(c("TEST01AA", "TEST02AA"))
#' }
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
#' brandseyer2::account("TEST01AA")
account.character <- function(codes) {
  if (codes == "TEST01AA") {
    return(test01aa)
  }
  if (length(codes) == 1) {
    read_account(codes) %>%
      create_account()
  } else {
    codes %>%
      map(read_account) %>%
      map(create_account)
  }
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

# Print out readable account information.
print.brandseyer2.account <- function(account, ...) {
  cat("BrandsEye Account", "\n")
  cat("Name: ", account_name(account), "\n")
  cat("Code: ", account_code(account), "\n")
}


