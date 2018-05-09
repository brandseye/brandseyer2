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
#' @param codes A vector of one or more account codes.
#'
#' @return An account object if one account code is given, or a list of account objects,
#'         one for each code given, in the same order as the codes were given.
#'
#' @aliases accounts
#'
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Read one account
#' account("TEST01AA")
#'
#' # Read two accounts, returned as a list.
#' accounts(c("TEST01AA", "TEST02AA"))
#' }
account <- function(codes) {
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
#' An alternative name for the \code{account}
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


