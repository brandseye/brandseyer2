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
#' @param filter A query to match mentions against. See the filter vignette for details.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A tibble of mentions.
#' @examples
#'
#' \dontrun{
#'
#' account("TEST01AA") %>%
#'   mentions(filter = "published inthelast week and brand isorchldof 1")
#'
#' }
#' @export
mentions <- function(x, filter, ...) {
  UseMethod("mentions")
}

#' @describeIn mentions
#'
#' Reads V4 accounts. Returns a tibble of mentions.
#'
#' @export
mentions.brandseyer2.account.v4 <- function(x, filter, ...) {
  assertthat::assert_that(assertthat::is.string(filter))
  assertthat::assert_that(nchar(filter) > 0,
                          msg = "filter cannot be an empty character vector")

  query <- list(filter = filter, limit=100)
  data <- read_api(endpoint = paste0("v4/accounts/", account_code(x), "/mentions"),
                   query = query)

  # We want to figure out a list of fields.
  fields = new.env(hash = TRUE)

  for (mention in data) {
    keys <- names(mention)
    for (key in keys) {
      if (!exists(key, envir = fields, inherits = FALSE)) {
        assign(key, list(), envir = fields)
      }
    }
  }

  list.fields <- c("brands", "tags", "mediaLinks")

  for (mention in data) {
    seen <- new.env(hash = TRUE)
    is.list.field <- FALSE
    imap(mention, function(value, key) {
      if (key == "mediaLinks") {
        value = map_df(value, ~tibble(url = .x$url, mimeType = .x$mimeType))
      } else if (key %in% c("brands", "tags")) {
        value = map(value, "id")
      } else if (rlang::is_list(value)) {
        value <- pluck(value, "id") %||% NA
        if (is.na(value)) rlang::warn(paste("Unable to find ID for compositive field", key))
      }
      l <- get(key, envir = fields)
      l[[length(l) + 1]] <- value
      assign(key, l, envir = fields)
      assign(key, TRUE, envir = seen)
    })

    # Now we add NAs for the fields that we have not seen.
    for (field in ls(fields)) {
      if (!exists(field, envir = seen, inherits = FALSE)) {
        l <- get(field, envir = fields)
        if (field %in% list.fields) length(l) <- length(l) + 1
        else l[[length(l) + 1]] <- NA
        assign(field, l, envir = fields)
      }
    }
  }

  final <- list()
  for (field in ls(fields)) {
    data <- get(field, envir = fields)
    if (!(field %in% list.fields)) data <- c(data, recursive = TRUE)
    final[[field]] <- data
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


  # Sort in to a slightly better order
  as_tibble(final) %>%
    mutate(published = lubridate::ymd_hms(published),
           pickedUp = lubridate::ymd_hms(pickedUp),
           updated = lubridate::ymd_hms(updated)) %>%
    select(id, uri, link,
           published, pickedUp, updated,
           extract,
           postExtract,
           replyToUri, replyToId,
           reshareOfUri, reshareOfId,
           brands,
           sentiment, relevancy,
           crowdVerified,
           relevancyVerified,
           sentimentVerified,
           authorId, authorName, authorHandle, authorPictureLink, authorProfileLink, authorBio, authorTimezone,
           toId,
           toName, toHandle, toHandleId,
           everything())
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
#' @param select A character vector of the mention fields to be returned.
#' @param all Set to true if you would like to return all mentions from the account.
#'            This overides the \code{limit} parameter.

#'
#' @export
mentions.brandseyer2.account.v3 <- function(x, filter, ...,
                                            use.brandseyer = FALSE,
                                            limit = 30, offset = 0,
                                            include, select,
                                            all = FALSE) {
  if (!is_installed("brandseyer") || !use.brandseyer) {
    abort("brandseyer2 only supports V4 accounts.")
  }
  brandseyer::account_mentions(account_code(x), filter = filter,
                               limit = 30, offset = 0,
                               include = include, select = select, all = all)
}
