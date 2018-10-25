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

#' Give section information for a dashboard
#'
#' Given a tibble of dashboard information (from, for example, `dashboards()`),
#' this adds the section information to the tibble.
#'
#' The second argument to this function, `d`, lets you search for specific
#' mentions in a similar way that [dashboards()] does for dashboards. See
#' the `dashboards()` documentation for more details.
#'
#' @param x A tibble to get section information for.
#' @param d An optional value to filter the sections on. Can be a vector
#'          of integer IDs for the wanted sections, or a vector of
#'          characters giving words to perform partial string matches
#'          on section titles with.
#' @param unnest A logical value indicating whether sections should be
#'               appended as a list of tibbles, or whether they should be unnested
#'               in to the tibble itself, similar to having called [tidyr::unnest()].
#' @param ... Parameters for other methods
#'
#' @return The original tibble, but now with an additional section column.
#' @export
#'
#' @seealso [dashboards()] to pull dashboard information for an account.
#' @seealso [metrics()] to pull metric information for sections.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Get all sections for the dashboard with ID 1
#' account("TEST01AA") %>%
#'   dashboards(1) %>%
#'   sections()
#'
#' # Get the section with ID 5 for the dashboard with ID 1
#' account("TEST01AA") %>%
#'   dashboards(1) %>%
#'   sections(5)
#'
#' # Get the sections whose title contain the string "where"
#' account("TEST01AA") %>%
#'   dashboards(1) %>%
#'   sections("where")
#' }
sections <- function(x, d, unnest, ...) {
  UseMethod("sections")
}

#' @describeIn sections
#'
#' @param .show.progress Whether to show a progress bar. By default, it will
#'                       only show a progress bar if there are more than 5 sections
#'                       being fetched, and the session is interactive.
#'
#' Get section information for a dashboard tibble.
#'
#' @export
sections.data.frame <- function(x, d, unnest = FALSE,
                                ...,
                                .show.progress = interactive()) {
  assert_that(x %has_name% "id", msg = "No dashboard `id` column present")

  ac <- attr(x, "account")
  assert_that(inherits(ac, "brandseyer2.account"),
              msg = "No account information on this data frame. See `dashboards`.")

  # For devtools::check
  title <- NULL; section.id <- NULL;

  pb <- list(tick = function(...) {})
  if (nrow(x) > 3 && .show.progress) {
    pb <- progress::progress_bar$new(
      format = "  sections for :code [:bar] :percent eta: :eta",
      total = nrow(x)
    )
  }

  pb$tick(0, tokens = list(code = account_code(ac)))

  d_missing <- missing(d)
  result <- x %>%
    mutate(sections = map(id, function(id) {
      pb$tick(tokens = list(code = account_code(ac)))
      secs <- load_sections(ac, id)
      if (d_missing) return(secs)

      if (is.character(d)) {
        return(
          secs %>%
            filter(map_lgl(title, ~ any(stringr::str_detect(tolower(.x), tolower(d)))))
        )
      }

      if (is.numeric(d)) {
        return(
          secs %>%
            filter(section.id %in% d)
        )
      }
    }))

  if (unnest) result <- result %>% unnest()

  result
}

load_sections <- function(x, d) {
  # For devtools::check
  section.id <- NULL; title <- NULL;

  read_mash(paste0("accounts/", account_code(x), "/reports/", d)) %>%
    pluck("sections") %>%
    {if (is.null(.)) list(list(id = NA, filter = NA, title = NA)) else .} %>%
    map_df(function(section) {
      section[["compare"]] <- list(as_tibble(section[["compare"]] %||% NA))
      section[["metrics"]] <- list(section$widgets %>%
                                     map_df(~tibble(metric.id = .x$id,
                                                    metric.type = .x$type,
                                                    metric.filter = .x$filter %||% NA,
                                                    width = as.integer(.x$width %||% NA),
                                                    height = as.integer(.x$height %||% NA),
                                                    chart.type = .x[['chart-type']] %||% NA,
                                                    label.type = .x[['label-type']] %||% NA,
                                                    caption = .x$caption,
                                                    coarseness = .x$coarseness %||% NA,
                                                    max.items = as.integer(.x[['max-items']] %||% NA),
                                                    colour.index = as.integer(.x[['colour-index']] %||% NA))))
      section[["widgets"]] <- NULL
      as_tibble(section)
    }) %>%
    rename(section.id = id, section.filter = filter) %>%
    select(section.id, title, everything())
}

