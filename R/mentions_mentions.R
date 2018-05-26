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


#' Fetch mentions from an account
#'
#' This will give you access to mentions in an account.
#'
#' @note It is possible to read mentions from older accounts still
#' using the V3 API. Using the older brandseyer library is the most
#' convenient way of doing this, but if the brandseyer library is installed,
#' the \code{mentions} function will automatically fall back to the appropriate
#' function calls in \code{brandseyer} to read mentions for you.
#'
#' @param x An account object
#' @param filter A query to match mentions against. Always required.
#'               See the filter vignette for details.
#' @param select A character vector of the mention fields to be returned.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A tibble of mentions.
#'
#' @seealso \code{\link{tags}}() to fetch tag information from mentions.
#' @seealso \code{\link{topics}}() to fetch topic information from mentions.
#' @seealso \code{\link{brands}}() to fetch brand information from mentions.
#' @seealso \code{\link{phrases}}() to fetch phrase information from mentions.
#'
#' @export
mentions <- function(x, filter, select, ...) {
  UseMethod("mentions")
}

#' @describeIn mentions
#'
#' Reads V4 accounts. Returns a tibble of mentions.
#'
#' @param orderBy Fields to order the returned data by. Defaults to published
#' @param fetchGraph Fetch other mentions that are part of the same conversation as this one.
#'
#' @export
#'
#' @examples
#'
#' # Reading data from a single account
#' account("TEST01AA") %>%
#'   mentions(filter = "published inthelast week and brand isorchildof 1") # Must always have a filter
mentions.brandseyer2.account.v4 <- function(x, filter, select = NULL,
                                            ...,
                                            orderBy = NULL, fetchGraph = FALSE) {
  assert_that(assertthat::is.string(filter))
  assert_that(nchar(filter) > 0,
              msg = "filter cannot be an empty character vector")
  assert_that(is.logical(fetchGraph))

  restricted.filter <- add_pickedup(filter, account_timezone(x))
  result <- NULL
  limit <- if (fetchGraph) 100 else 100000

  repeat {
    # The sprintf is to avoid scientific notation for large numbers without
    # globally setting scipen in options.
    query <- list(filter = restricted.filter,
                  limit= sprintf("%d", limit),
                  offset = sprintf("%d", nrow(result) %||% 0))

    if (!is.null(select)) {
      assert_that(is.character(select))
      query$select = paste0(select, collapse = ',')
    }

    if (!is.null(orderBy)) {
      assert_that(is.character(orderBy))
      query$orderBy = paste0(orderBy, collapse = ',')
    }

    if (fetchGraph) {
      query$fetchGraph = TRUE
    }

    data <- read_data(x, query)

    if (length(data) == 0) break
    m <- list_to_v4_mentions(data)
    result <- dplyr::bind_rows(result, m)

    if (nrow(m) < limit) break
  }

  # Set up things for devtools::check
  uri <- NULL;                link <- NULL;                   published <- NULL
  sentiment <- NULL;          brands <- NULL;                 authorBio <- NULL
  authorHandle <- NULL;       authorId <- NULL;               authorName <- NULL
  authorPictureLink <- NULL;  authorProfileLink <- NULL;      authorTimezone <- NULL
  crowdVerified <- NULL;      extract <- NULL;                pickedUp <- NULL
  postExtract <- NULL;        relevancy <- NULL;              relevancyVerified <- NULL
  replyToId <- NULL;          replyToUri <- NULL;             reshareOfId <- NULL
  reshareOfUri <- NULL;       sentimentVerified <- NULL;      toHandle <- NULL
  toHandleId <- NULL;         toId <- NULL;  toName <- NULL;  updated <- NULL
  phrases <- NULL;

  # Clean up the final tibble.
  if (result %has_name% "published") result <- result %>% mutate(published = lubridate::ymd_hms(published))
  if (result %has_name% "pickedUp") result <- result %>% mutate(published = lubridate::ymd_hms(published))
  if (result %has_name% "updated") result <- result %>% mutate(published = lubridate::ymd_hms(published))

  result <- result %>%
    select(id, uri, link,
           published, pickedUp, updated,
           extract,
           postExtract,
           replyToUri, replyToId,
           reshareOfUri, reshareOfId,
           brands,
           phrases,
           sentiment, relevancy,
           crowdVerified,
           relevancyVerified,
           sentimentVerified,
           authorId, authorName, authorHandle, authorPictureLink, authorProfileLink, authorBio, authorTimezone,
           toId,
           toName, toHandle, toHandleId,
           everything())

  attr(result, "account") <- x
  result
}

#' @describeIn mentions
#'
#' Reads older, V3 accounts. To use this function, please ensure that
#' the older \code{brandseyer} library is installed as well. Returns
#' old \code{brandseyer} data structures, as though the \code{brandseyer::account_mentions}
#' function had been called.
#'
#' @param use.brandseyer Set to TRUE if the old brandseyer library is installed,
#'        and you would like to read from an account still using the old V3 API.
#' @param limit The maximum number of mentions to be returned
#' @param offset Mentions are returned in an order. Offset says how many of the
#'   first mentions should be skipped.
#' @param include A character vector of extra information to include in the mentions
#' @param all Set to true if you would like to return all mentions from the account.
#'            This overides the \code{limit} parameter.

#'
#' @export
mentions.brandseyer2.account.v3 <- function(x, filter, select, ...,
                                            use.brandseyer = FALSE,
                                            limit = 30, offset = 0,
                                            include,
                                            all = FALSE) {
  if (!is_installed("brandseyer") || !use.brandseyer) {
    abort("brandseyer2 only supports V4 accounts.")
  }
  brandseyer::account_mentions(account_code(x), filter = filter,
                               limit = 30, offset = 0,
                               include = include, select = select, all = all)
}

#' Format list data as a tibble
#'
#' This takes list data returned from the API and extracts
#' information for the mention table.
#'
#' @param data List data, as possibly returned from \code{link{read_data}}().
#'
#' @return A tibble of mention data.
list_to_v4_mentions <- function(data) {
  # We want to figure out a list of fields.
  list.fields <- c("brands", "tags", "mediaLinks", "phrases")
  fields = new.env(hash = TRUE)

  for (mention in data) {
    keys <- names(mention)
    for (key in keys) {
      if (!exists(key, envir = fields, inherits = FALSE)) {
        assign(key, if(key %in% list.fields) list() else c(), envir = fields)
      }
    }
  }

  columns <- new.env(hash = TRUE)

  # Now we want to collect the fields from the mentions.
  for (field in ls(fields)) {
    assign(field, value = map(data, function(mention) {
      value <- pluck(mention, field)

      # There are some special fields that are lists we need to handle
      # If they're not present, lists are NULL. Everything else is NA.
      if (field == "mediaLinks") {
        value = map_df(value, ~tibble(url = .x$url, mimeType = .x$mimeType))
        if (rlang::is_empty(value)) value <- tibble(url = NA, mimeType = NA)
      } else if (field %in% c("tags", "brands", "phrases")) {
        value <- map_int(value, "id")
        if (rlang::is_empty(value)) value <- NULL
      } else if (is.list(value)) {
        value = pluck(value, "id") %||% NA
        if (is.na(value)) rlang::warn(paste("Unable to find ID for compositive field", key))
      } else value <- value %||% NA

      value
    }), envir = columns)
  }

  # Now we want to convert to a tibble
  final <- list()
  for (field in ls(fields)) {
    data <- get(field, envir = columns)
    if (!(field %in% list.fields)) data <- c(data, recursive = TRUE)
    final[[field]] <- data
  }

  as_tibble(final)
}

#' Read mention data from the API.
#'
#' Given an account and a query, this reads data from grouse,
#' or, if test data exists, returns the test data.
#'
#' Test data should be in the format `test01aa_mentions`.
#'
#' @param x An account object.
#' @param query A list of query parameters
#'
#' @return A list of data from the API.
read_data <- function(x, query) {
  code <- account_code(x)

  # See if we have any internal data that we can load
  if (startsWith(code, "TEST") && nchar(code) == 8) {
    ac_data <- paste0(code, "_mentions")
    if (exists(tolower(ac_data))) {
      result <- get(tolower(ac_data))
      if (!is.null(result)) return(result)
    }
  }

  # Read from a live account.

  read_api(endpoint = paste0("v4/accounts/", code, "/mentions"),
           query = query)
}
