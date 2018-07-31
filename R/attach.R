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

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  # Print a welcoming message for people in interactive mode.
  # We want to make sure that we don't upset any scripts
  # relying on preset random number seeds.
  withr::with_preserve_seed({
    message <- sample(
      c(
        "Welcome to brandseyer2!",
        "Find help and docs at https://brandseye.github.io/brandseyer2",
        "Read the brandseyer2 cookbook at https://brandseye.github.io/brandseyer2/articles/cookbook.html",
        "See what brandseyer2 can do at https://brandseye.github.io/brandseyer2/reference/index.html"
      ),
      1
    )

    packageStartupMessage(message)
  })
}
