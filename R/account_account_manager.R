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


#' See your account manager
#'
#' This provides the name and email address of your
#' account manager. You can contact this person if you have any questions
#' concerning your account.
#'
#' The returned object provides two fields:
#'
#' \enumerate{
#'   \item \code{$name} Holds your account manager's name
#'   \item \code{$email} Holds your account manager's email address
#' }
#'
#' @param account An account object
#'
#' @return A list containing the name and email address of your account manager.
#' @export
#'
#' @examples
#' \dontrun{
#' ac <- account("TEST01AA")
#' manager <- account_manager(ac)
#'
#' manager$name
#' manager$email
#' }
#'
account_manager <- function(account) {
  UseMethod("account_manager")
}

account_manager.brandseyer2.account <- function(account) {
  structure(
    list(name = paste(account$clientService$firstName, account$clientService$lastName),
         email = account$clientService$email),
    class = "brandseyer2.manager"
  )
}

print.brandseyer2.manager <- function(manager) {
  display <- matrix(c(manager$name, manager$email), 2, 1)
  rownames(display) <- c("name:", "email:")
  colnames(display) <- ""
  cat("BrandsEye Client Service")
  print(display, quote = FALSE)
}