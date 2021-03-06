# Copyright (c) 2015, 2018, Brandseye PTY (LTD)
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

#' Authenticate yourself with BrandsEye
#'
#' Provides a means to authenticate yourself with the BrandsEye API. This is done
#' using an API key that your account manager representative has provided for you.
#'
#' @param key Your API key. If you don't know this, your account manager can give you one.
#' @param save Set to true if you would like this key to be saved to disc and loaded
#'             automatically the next time that you use this library.
#' @return Your authentication details
#'
#' @details
#' All authentication with the BrandsEye API is done via a special, user specific
#' key. This key can be provided to you by your account manager.
#'
#' It's possible to also have your key saved to disc and automatically loaded
#' for you when you next use the library. Set the `save` parameter to `TRUE`
#' in order to do this.
#'
#' If you would like to see who you are currently logged in as, see the
#' [whoami()] function.
#'
#' @seealso [account_manager()]
#'
#' @examples
#' \dontrun{
#'
#' authenticate("adfd42345f534fgdfgd")
#'
#' }
#'
#' @export
#' @author Constance Neeser
authenticate <- function(key, save = FALSE) {
  assertthat::assert_that(is.character(key))

  # Want to see if the user has admin privelages.
  me <- getUserImpl(key)
  isAdmin = FALSE
  if (!is.null(me$admin) && !is.na(me$admin)) {
    isAdmin = me$admin
  }

  auth <- structure(list(
    name = paste(me$firstName, me$lastName),
    email = me$email,
    key = key,
    admin = isAdmin
  ), class = "brandseye.auth")

  pkg.env$authentication <- auth

  if (save)
    save_key(key)

  auth
}

#' @export
print.brandseye.auth <- function(x, ...) {
  cat(format(x), "\n")
}

#' @export
format.brandseye.auth <- function(x, ...) {
  paste0("BrandsEye Credentials\n",
         crayon::silver("  name:  "), x$name, "\n",
         crayon::silver("  email: "), x$email)
}

#' Returns your current authentication credentials.
#'
#' If you need to authenticate yourself, you can do this
#' with the [authenticate()] function. [am_i_brandseye()]
#' is a useful method for determining if you're a staff member
#' or not. The `raise_error` method makes [whoami()] an easy
#' way to determine if the user is authenticated, and to raise
#' an error if not.
#'
#' @param raise_error If this is set to `TRUE`, the function
#'                    will raise an error instead of returning NULL if you
#'                    are not authenticated.
#' @return Returns `NULL` if you are not logged in,
#'         otherwise returns your current authentication credentials.
#' @author Constance Neeser
#' @export
#'
whoami <- function(raise_error = FALSE) {
  if (raise_error && is.null(pkg.env$authentication))
    abort("You are not authenticated. Use the authenticate function to do so.")

  pkg.env$authentication
}

#' Are you authorised as a BrandsEye staff member?
#'
#' Returns `TRUE` if and only if you are a BrandsEye staff.
#'
#' @return A logical value.
#' @export
am_i_brandseye <- function() {
  credentials <- whoami()
  if (is.null(credentials)) return(FALSE);
  credentials$admin %||% FALSE
}

#' Returns the file name in which you can save your authentication information.
#' @export
authentication_filename <- function() {
  file.path(Sys.getenv("HOME"), ".brandseyerd", "authentication.json")
}


#' Saves a key to file.
#'
#' @param key The API_KEY
save_key <- function(key) {
  assertthat::assert_that(is.character(key))
  contents <- paste0('{ "key": "', key, '" }')

  message(paste("Authentication file created at", authentication_filename()))
  if (!file.exists(dirname(authentication_filename()))) {
    dir.create(dirname(authentication_filename()))
  }
  if (!is.null(contents)) {
    fileConn<-file(authentication_filename())
    writeLines(contents, fileConn)
    close(fileConn)
  }
  else {
    file.create(authentication_filename())
  }

  invisible()
}

#' Low level function to access user information.
#'
#' @param key The API key
getUserImpl <- function(key) {
  url <- paste0("https://mash.brandseye.com/rest/users/me")
  data <- httr::GET(url, httr::authenticate("API_KEY", key))
  check_errors(data)
  results <- jsonlite::fromJSON(httr::content(data, "text"))

  results
}
