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

sections <- function(x, d) {
  UseMethod("sections")
}

sections.data.frame <- function(x, d) {
  assert_that(x %has_name% "id", msg = "No dashboard `id` column present")

  ac <- attr(x, "account")
  assert_that(inherits(ac, "brandseyer2.account"),
              msg = "No account information on this data frame. See `dashboards`.")

  # For devtools::check
  title <- NULL; section.id <- NULL;

  d_missing <- missing(d)
  result <- x %>%
    mutate(sections = map(id, function(id) {
      sections <- load_sections(ac, id)
      if (d_missing) return(sections)

      if (is.character(d)) {
        return(
          sections %>%
            filter(map_lgl(title, ~ any(stringr::str_detect(tolower(.x), tolower(d)))))
        )
      }

      if (is.numeric(d)) {
        return(
          sections %>%
            filter(section.id %in% d)
        )
      }
    }))

  result
}

load_sections <- function(x, d) {
  # For devtools::check
  section.id <- NULL; title <- NULL;
  read_mash(paste0("accounts/", account_code(x), "/reports/", d)) %>%
    pluck("sections") %>%
    map_df(function(section) {
      section[["widgets"]] <- list(section$widgets %>%
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
      as_tibble(section)
    }) %>%
    rename(section.id = id, section.filter = filter) %>%
    select(section.id, title, everything())
}
