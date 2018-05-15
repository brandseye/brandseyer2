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

#' Fetch tags for an account
#'
#' Fetches tag information information, returned as a tibble, for the given
#' account.
#'
#' @param accounts One or more account objects.
#'
#' @return A tibble of tag information. Includes the name, namespace, and description of the
#' tags. Note that topics are stored in the 'topics' namespace.
#' @export
#'
#' @seealso \code{\link{account_topics}} to see just a list of topics.
#'
#' @examples
#' \dontrun{
#'
#' # See what namespaces are in your account
#' account("TEST01AA") %>%
#'   account_tags() %>%
#'   dplyr::select(namespace) %>%
#'   table()
#'
#' # Find topics
#' account("TEST01AA") %>%
#'   account_tags() %>%
#'   dplyr::filter(namespace == "topic")
#' }
#'
account_tags <- function(accounts) {
  UseMethod("account_tags")
}

#' @describeIn account_tags
#'
#' Get tags from an account object.
#'
#' @export
account_tags.brandseyer2.account <- function(accounts) {
  # Handle devtools::check notes
  children <- NULL
  is_parent <- NULL

  accounts$tags %>%
    map_df(function(d) {
      tibble(id = d$id,
             name = d$name,
             namespace = d$namespace,
             description = d$description %||% NA,
             deleted = d$deleted %||% FALSE
      )
    }) %>%
    mutate(children = map(accounts$tags, "children"),
           is_parent = map_lgl(children, ~length(.x) > 0))
}

#' @describeIn account_tags
#'
#' Create a table of tags for the list of accounts given
#'
#' @export
#'
#' @examples
#'
#' accounts(c("TEST01AA", "TEST02AA")) %>%
#'   account_tags()
account_tags.list <- function(accounts) {
  accounts %>%
    map_df(~ .x %>%
             account_tags %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}
