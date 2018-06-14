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

#' List the dashboards in an account
#'
#' In [Analyse](https://analyse.brandseye.com), BrandsEye's analysis tool,
#' people can set up dashboards to query our data. This function exposese those
#' dashboards as a tibble, allowing you to query sections, metrics and their
#' associated filters.
#'
#' Dashboards can be filtered in various ways. The second argument, `d`, can be
#' either an integer or list of integers, specifying the particular dashboard
#' IDs that are wanted. The second argument can also be a character vector,
#' to give partial matches against dashboard names. Case is ignored.
#'
#' @seealso [sections()] to pull section information for a dashboard.
#' @seealso [metrics()] to pull metric information for sections.
#'
#' @param x An account object to query.
#' @param d An optional value to filter the dashboards on. Can be a vector
#'          of integer IDs for the wanted dashboards, or a vector of
#'          characters giving words to perform partial string matches
#'          on dashboard names with.
#' @param ... Additional parameters for methods
#'
#' @return A tibble of dashboard data.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Get all dashboards.
#' account("TEST01AA") %>%
#'   dashboards()
#'
#' # Get the dashboard with ID 1.
#' account("TEST01AA") %>%
#'   dashboards(1)
#'
#' # Get dashboards 1 and 21.
#' account("TEST01AA") %>%
#'   dashboards(c(1, 21))
#'
#' # Get dashboards whose name contains the string 'overview'.
#' account("TEST01AA") %>%
#'   dashboards("overview")
#'
#' # Get dashboards whose name contains the string 'overview' or the string 'who'.
#' account("TEST01AA") %>%
#'   dashboards(c("overview", "who"))
#'
#' }
dashboards <- function(x, d, ...) {
  UseMethod("dashboards")
}

#' @describeIn dashboards
#'
#' Read dashboard data for an [account()] object.
#'
#' @export
#'
#' @param section An optional numeric vector of section IDs, or
#'                character vector to match against section titles. This specifies
#'                which sections to return, similarly to calling [sections()]. You
#'                can use the special string `'*'` to select all sections.
dashboards.brandseyer2.account <- function(x, d,
                                           section, ...) {
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
    result <- result %>%
      filter(map_lgl(name, ~ any(stringr::str_detect(tolower(.x), tolower(d)))))
  }

  if (is.numeric(d)) {
    result <- result %>% filter(id %in% d)
  }

  if (!missing(section)) {
    result <- result %>% sections(section, unnest = TRUE)
  }

  result
}
