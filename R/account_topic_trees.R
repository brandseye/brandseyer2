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

#' List topic trees for an account
#'
#' Returns a tibble of topic trees associated with the account.
#'
#' @param x A account or list of accounts.
#' @param ... Arguments for other methods.
#'
#' @export
#'
topic_trees <- function(x, ...) {
  UseMethod("topic_trees")
}

#' @describeIn topic_trees
#'
#' Provides topic trees for [account()] objects.
#'
#' @export
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   topic_trees()
topic_trees.brandseyer2.account <- function(x, ...) {
  # For devtools::check
  namespace <- NULL; is_parent <- NULL;

  x %>%
    tags() %>%
    dplyr::filter(namespace == "topic_tree") %>%
    dplyr::select(-namespace, -is_parent)
}

#' @describeIn topic_trees
#'
#' Get topic trees for a list of accounts.
#'
#' @export
#'
#' @examples
#'
#' accounts(c("TEST01AA", "TEST02AA")) %>%
#'   topic_trees()
topic_trees.list <- function(x, ...) {
  x %>%
    map_df(~ .x %>%
             topic_trees %>%
             mutate(account = account_code(.x)) %>%
             select(account, everything()))
}
