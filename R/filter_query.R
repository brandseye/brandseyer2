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

query <- function(accounts = accounts,
                  brands = NULL,
                  timezones = NULL,
                  filter = NULL,
                  comparison = NULL,
                  fields = NULL,
                  grouping = NULL,
                  ordering = NULL) {

  structure(list(accounts = accounts,
                 brands = brands,
                 timezones = timezones,
                 filter = filter,
                 comparison = comparison,
                 fields = fields,
                 grouping = grouping,
                 ordering = ordering),
            class = "brandseyer2.query")
}

copy_query <- function(q) {
  query(
    accounts = q$accounts,
    brands = q$brands,
    timezones = q$timezones,
    filter = q$filter,
    comparison = q$comparison,
    fields = q$fields,
    grouping = q$grouping,
    ordering = q$ordering
  )
}

merge_query <- function(lhs, rhs) {
  query(
    accounts = c(lhs$accounts, rhs$accounts),
    brands = c(lhs$brands, rhs$brands),
    timezones = c(lhs$timezones, rhs$timezones),
    filter = lhs$filter,
    comparison = lhs$comparison,
    fields = lhs$fields,
    grouping = lhs$grouping,
    ordering = lhs$ordering
  )
}


print.brandseyer2.query <- function(x, ...) {
  cat(format(x), "\n")
}

format.brandseyer2.query <- function(x, ...) {
  lines <- list("BrandsEye Query")
  width <- cli::console_width()

  if (!is.null(x$accounts)) {
    title_str <- "  accounts:\t"
    account_str <- stringr::str_flatten(x$accounts, collapse = ', ')
    if (stringr::str_length(account_str) + stringr::str_length(title_str) > width)
      account_str <- glue::glue("{stringr::str_flatten(x$accounts[1:2], collapse = ', ')} and {length(x$accounts) - 2} other accounts")

    account_str <- paste0(crayon::silver(title_str), account_str)
    lines <- c(lines, account_str)
  }
  if (!rlang::is_empty(x$brands)) {
    title_str <- "  brands:\t"
    brand_str <- x$brands %>% map_chr(~ format(.x, colour = FALSE)) %>% stringr::str_flatten(collapse = ", ")
    if (stringr::str_length(brand_str) + stringr::str_length(title_str) > width) {
      brand_str <- glue::glue("{length(x$brands)} brands selected")
    } else {
      brand_str <- x$brands %>% map_chr(~ format(.x)) %>% stringr::str_flatten(collapse = ", ")
    }

    brand_str = paste0(crayon::silver(title_str), brand_str)
    lines <- c(lines, brand_str)
  }
  if (!is.null(x$timezones)) {
    lines <- c(lines, glue::glue("  {crayon::silver('timezones:')}\t{stringr::str_flatten(unique(x$timezones), collapse = ', ')}"))
  }
  if (!is.null(x$filter)) {
    lines <- c(lines, glue::glue("  {crayon::silver('filter:')}\t{x$filter}"))
  }
  if (!is.null(x$comparison)) {
    lines <- c(lines, glue::glue("  {crayon::silver('comparison:')}\t{stringr::str_flatten(x$comparison, collapse = ', ')}"))
  }
  if (!is.null(x$fields)) {
    lines <- c(lines, glue::glue("  {crayon::silver('fields:')}\t{stringr::str_flatten(x$fields, collapse = ', ')}"))
  }
  if (!is.null(x$grouping)) {
    lines <- c(lines, glue::glue("  {crayon::silver('grouping:')}\t{stringr::str_flatten(x$grouping, collapse = ', ')}"))
  }
  if (!is.null(x$ordering)) {
    lines <- c(lines, glue::glue("  {crayon::silver('ordering:')}\t{stringr::str_flatten(x$ordering, collapse = ', ')}"))
  }

  paste(lines, collapse = "\n")
}

