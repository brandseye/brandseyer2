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


# returns NULL if there are no brands.


#' Converts a query to a vector of filters for counting by.
#'
#' [to_count_filter()] converts a [query()] to a vector
#' of filters appropriate for counting by. An account code
#' is specified to choose a filter for a specific account.
#' Each filter is required to perform all of the count operations
#' being requested by the query for an individual account.
#'
#' @param query A [query()] object.
#' @param code  The account we want filters for.
#'
#' @return A vector of characters.
#' @export
#'
#' @examples
#'
#' account("TEST01AA") %>%
#'   filter_mentions("published inthelast week") %>%
#'   to_count_filter("TEST01AA")
to_count_filter <- function(query, code) {
  if (!(code %in% query$accounts)) return(NULL)
  assert_that(is.string(query$filter))

  brands <- purrr::keep(query$brands, ~ .x$code == code)
  if (length(brands) == 0) return(NULL)


  brand_portion <- brands %>%
    map_int(~.x$id) %>%
    map_chr(~ glue::glue("brand isorchildof {.x}")) %>%
    stringr::str_flatten(collapse = " or ")

  filter <- glue::glue("({query$filter}) and ({brand_portion})")
  if (is.null(query$comparison)) return(filter)

  results <- query$comparison %>%
    map_chr(~ glue::glue("{filter} and ({.x})"))

  # Ensure that all comparisons have names
  names(results) %<>%
    purrr::map2_chr(query$comparison, ~ {if (.x == "") .y else .x})

  results
}
