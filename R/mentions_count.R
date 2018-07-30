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

#' Count mentions
#'
#' Counts mentions, aggregating them in to various data sets.
#'
#' @param .account   An account to read from
#' @param ...        Further arguments for other methods
#'
#' @return A tibble of data.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Count all mentions in your account in the last year
#' account("TEST01AA") %>%
#'   count_mentions("published inthelast year and brandisorchildof 1")
#'
#' # Count all mentions in your account, grouping by publication date.
#' account("TEST01AA") %>%
#'   count_mentions("published inthelast year and brandisorchildof 1", groupBy = published)
#'
#' }
count_mentions <- function(.account, ...) {
  UseMethod("count_mentions")
}

#' @describeIn count_mentions
#'
#' Count mentions in a v4 account.
#'
#' @param filter  A filter to use
#' @param groupBy An optional list of names of things to group by.
#'                For example, `groupBy = published`, or,
#'                `groupBy = tag`
#' @param select  An optional list of names of things to select and
#'                aggregate by, in addition to the number of mentions.
#'                For example, `select = totalSentiment`, or,
#'                `select = c(totalSentiment, totalEngagement)`.
#' @param orderBy An optional list of names of the aggregate fields to
#'                order the results by.
#' @param tagNamespace An optional string. When grouping by `tag`, `tagNamespace` can
#'                     be supplied to limit the tags being grouped by to only those
#'                     in the given namespace. For example, to only see topics,
#'                     have `tagNamespace = 'topic'`
#' @param .envir  An optional environment in which to evaluate variables in
#'                your `groupBy`, `select`, and `orderBy` arguments.
#'
#' @export
count_mentions.brandseyer2.account.v4 <- function(.account,
                                                  filter,
                                                  ...,
                                                  groupBy = NULL,
                                                  select = NULL,
                                                  orderBy = NULL,
                                                  tagNamespace = NULL,
                                                  .envir = parent.frame()) {

  groupBy <- get_name_list(deparse(substitute(groupBy)), env = .envir)
  select <- get_name_list(deparse(substitute(select)), env = .envir)
  orderBy <- get_name_list(deparse(substitute(orderBy)), env = .envir)

  count_mentions(account_code(.account), filter,
                 timezone = account_timezone(.account),
                 groupBy = groupBy,
                 select = select,
                 orderBy = orderBy,
                 tagNamespace = tagNamespace)
}




count_mentions.character <- function(.account,
                                     filter,
                                     ...,
                                     timezone = "Africa/Johannesburg",
                                     groupBy = NULL,
                                     select = NULL,
                                     orderBy = NULL,
                                     tagNamespace = NULL) {

  assert_that(!missing(filter) && is.string(filter),
              msg = "A filter must be provided")
  assert_that(is.character(timezone))

  query <- list(filter = filter)

  if (!is.null(groupBy)) {
    assert_that(is.character(groupBy))
    query$groupBy = paste0(groupBy, collapse = ',')
  }

  if (!is.null(select)) {
    assert_that(is.character(select))
    query$select = paste0(select, collapse = ',')
  }

  if (!is.null(orderBy)) {
    assert_that(is.character(orderBy))
    query$orderBy = paste0(orderBy, collapse = ',')
  }

  if (!is.null(tagNamespace)) {
    assert_that(is.string(tagNamespace))
    query$tagNamespace = tagNamespace
  }

  data <- read_api(endpoint = paste0("v4/accounts/", .account, "/mentions/count"),
                   query = query)

  process <- function(row) {
    as.tibble(row %>% imap(function(data, index) {
      if (index %in% c("published", "pickedUp", "updated")) {
        data <- if (nchar(data) > 10) lubridate::ymd_hm(data, tz = timezone) else lubridate::ymd(data, tz = timezone)
      }
      d <- list()
      if (is.atomic(data)) d[[index]] = data
      else {
        # Ensure that we have no null items
        for (name in names(data)) {
          if (is.null(data[[name]])) data[[name]] <- NA
        }

        # Create specific new fields to return
        d[[paste0(index, ".id")]] <-  data$id
        if (!is.null(data$name)) d[[paste0(index, ".name")]] <- data$name
        if (!is.null(data$fullName)) d[[paste0(index, ".name")]] <- data$fullName

        # Clean up the object's data.
        if (!is.null(data$labels)) data$labels <- list(as.tibble(data$labels) %>% tidyr::gather("language", "translation"))
        if (!is.null(data$descriptions)) data$descriptions <- list(as.tibble(data$descriptions) %>% tidyr::gather("language", "translation"))

        # See if we want to add the data itself
        n <- names(data)
        if (length(n) != 2 || !"id" %in% n || !"name" %in% n) {
          d[[index]] = list(as.tibble(data))
        }
      }
      d
    }) %>% purrr::flatten())
  }

  if (is.null(groupBy)) {
    return(process(data))
  }

  data %>% map_df(process)

}







count_mentions.brandseyer2.query <- function(.account, ...,
                                             tagNamespace = NULL) {
  count_mentions(.account$accounts,
                 filter = .account$filter,
                 groupBy = .account$grouping,
                 orderBy = .account$ordering,
                 tagNamespace = tagNamespace)
}
