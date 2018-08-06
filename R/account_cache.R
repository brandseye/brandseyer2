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

pkg.accounts <- new.env()   # account information that we've fetched.

cached_account_exists <- function(code) {
  assert_that(is.string(code))
  exists(code, envir = pkg.accounts, inherits = FALSE)
}

cache_account <- function(account) {
  assign(account_code(account), account, envir = pkg.accounts)
  account
}

cache_is_expired <- function(account, duration = lubridate::duration(1, "minutes")) {
  Sys.time() - account$accessed > lubridate::duration(1, "minutes")
}

remove_from_cache <- function(code) {
  assert_that(is.string(code))

  if (cached_account_exists(code)) {
    rm(code, envir = pkg.accounts)
  }

  account
}

get_cached_account <- function(code) {
  assert_that(is.string(code))
  get(code, envir = pkg.accounts)
}

clear_cache <- function(account) {
  message("Removing old accounts")
  ls(pkg.accounts) %>%
    map(function(code) {
      if (cached_account_exists(code)) {
        ac <- get_cached_account(code)
        if (cache_is_expired(ac))
          remove_from_cache(code)
      }
    })

  invisible()
}
