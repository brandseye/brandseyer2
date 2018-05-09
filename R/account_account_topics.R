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

#' Fetch topics for an account
#'
#' Fetches the topics that are used in an account.
#'
#' @param account An account object.
#'
#' @return A tibble of topic information. Includes the name and topic description.
#' @export
#'
account_topics <- function(account) {
  UseMethod("account_topics")
}

account_topics.brandseyer2.account <- function(account) {
  # Taking devtools::check() notes in to discussion.
  namespace <- NULL
  children <- NULL

  account %>%
    account_tags() %>%
    dplyr::filter(namespace == "topic") %>%
    dplyr::arrange(desc(map_int(children, length)))
}
