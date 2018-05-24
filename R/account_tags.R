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

#' @describeIn tags
#'
#' Get tags from an account object.
#'
#' @export
#' @examples
#'
#' # See what namespaces are in your account
#' account("TEST01AA") %>%
#'   tags() %>%
#'   dplyr::select(namespace) %>%
#'   table()
#'
#' # Find topics
#' account("TEST01AA") %>%
#'   tags() %>%
#'   dplyr::filter(namespace == "topic")
#'
tags.brandseyer2.account <- function(x, ...) {
  # Handle devtools::check notes
  children <- NULL
  is_parent <- NULL

  x$tags %>%
    map_df(function(d) {
      tibble(id = d$id,
             name = d$name,
             namespace = d$namespace,
             description = d$description %||% NA,
             deleted = d$deleted %||% FALSE
      )
    }) %>%
    mutate(children = map(x$tags, ~c(pluck(.x, "children"), recursive = TRUE)),
           is_parent = map_lgl(children, ~length(.x) > 0))
}

#' @describeIn tags
#'
#' Create a table of tags for the list of given accounts.
#'
#' @export
#'
#' @examples
#'
#' # See the tags for multiple accounts at a time.
#' accounts(c("TEST01AA", "TEST02AA")) %>%
#'   tags()
tags.list <- function(x, ...) {
  x %>%
    map_df(~ .x %>%
             tags %>%
             mutate(account = account_code(.x),
                    ac = list(.x)) %>%
             select(account, -ac, everything()))
}
