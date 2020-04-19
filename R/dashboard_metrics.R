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

#' See metrics in a dashboard
#'
#' This provides easy access to metric information stored in the metric
#' column of a tibble returned by [sections()] and [dashboards()]. It
#' essentially uses [tidyr::unnest_legacy()] to clean up the table.
#'
#' This provides an additional field not available in the original metric
#' information called `filter`. This indicates the filter that the metric would have
#' used to call information from the BrandsEye API, and is a combination of the
#' section and metric filters.
#'
#' @param x The tibble to pull widget information from.
#' @param m An optional filter for selecting metrics. A numeric
#'          vector allows you to choose metrics with the given IDs. A
#'          character vector allows you to choose metrics with matching captions.
#' @param ... Parameters for other methods.
#'
#' @return An unnested table of metrics.
#'
#' @seealso [dashboards()] to pull dashboard information for an account.
#' @seealso [sections()] to pull section information for an account.
#'
#' @export
metrics <- function(x, m, ...) {
  UseMethod("metrics")
}

#' @describeIn metrics
#'
#' Expand metric information for data frames.
#'
#' @param collapse A logical value indicating whether columns should be removed (collapsed)
#'                 or not.
#'
#' @export
metrics.data.frame <- function(x, m, collapse = TRUE, ...) {
  assert_that(x %has_name% "id", msg = "No dashboard `id` column in data frame.")
  assert_that(x %has_name% "section.id", msg = "No `section.id` column in data frame.")
  assert_that(x %has_name% "section.filter", msg = "No `section.filter` column in data frame.")
  assert_that(x %has_name% "metrics", msg = "No `metrics` column in data frame.")

  # for devtools::check
  section.filter <- NULL; metric.filter <- NULL; section.id <- NULL; metric.id <- NULL;
  metric.type <- NULL; colour.index <- NULL; caption <- NULL;

  results <- x %>%
    unnest_legacy(metrics) %>%
    mutate(filter = map2_chr(section.filter, metric.filter, function(s, m) {
      s <- if (is.na(s)) NULL else paste0("(", s, ")")
      m <- if (is.na(m)) NULL else m <- paste0("(", m, ")")
      stringr::str_c(c(s, m), collapse = " AND ")
    }))


  if (collapse) {
    results <- results %>%
      select(id, section.id, section.filter, metric.id, metric.filter, filter, metric.type:colour.index)
  }

  if (!missing(m)) {
    if (is.numeric(m)) {
      results <- results %>%
        filter(metric.id %in% m)
    }

    if (is.character(m)) {
      results <- results %>%
        filter(map_lgl(caption, ~ any(stringr::str_detect(tolower(.x), tolower(m)))))
    }
  }

  results
}
