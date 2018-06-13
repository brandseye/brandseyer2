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

dashboards <- function(x, d) {
  UseMethod("dashboards")
}

dashboards.brandseyer2.account <- function(x, d) {
  result <- read_mash(paste0("accounts/", account_code(x), "/reports")) %>%
    map_df(function(data) {
      lastUpdated <- data[["lastUpdatedBy"]]
      data[["lastUpdatedBy"]] <- lastUpdated$email
      as.tibble(data)
    })

  attr(result, "account") <- x

  if (missing(d)) return(result)

  # for devtools::check
  name <- NULL;

  if (is.character(d)) {
    return(
      result %>%
        filter(map_lgl(name, ~ any(stringr::str_detect(tolower(.x), tolower(d)))))
    )
  }

  if (is.numeric(d)) {
    return(
      result %>% filter(id %in% d)
    )
  }
}
