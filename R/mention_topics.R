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

#' @describeIn topics
#'
#' Fetch topic information from a tibble of mention data.
#'
#' @param ac An optional account object from which to take topic information.
#' @param na.rm Whether to remove mentions that have no topics.
#'
#' @export
topics.data.frame <- function(x, ..., ac = attr(x, "account"), na.rm = TRUE) {
  assert_that(x %has_name% "id", msg = "data.frame has no mention id column")
  assert_that(x %has_name% "tags", msg = "data.frame has no tags / topics column")
  assert_that(is.logical(na.rm))

  ts <- x %>%
    select(id, tags) %>%
    unnest_legacy(tags = map(tags, ~ .x %||% NA))

  # For devtools::check()
  namespace <- NULL; topic.id <- NULL;

  if (!is.null(ac)) {
    ts <- ts %>%
      left_join(ac %>% topics(), by = c("tags" = "id")) %>%
      rename(topic.id = tags) %>%
      mutate(topic.id = map2_int(topic.id, namespace,
                                 ~ ifelse(!is.na(.x) && !is.na(.y), .x, NA))) %>%
      filter((namespace == "topic") | !na.rm)
  } else rlang::warn("No account information to use - see `ac` argument to `topics()`")

  ts
}
