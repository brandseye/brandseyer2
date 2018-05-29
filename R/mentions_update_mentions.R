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

#' Update data stored on a mention
#'
#' This function lets you update data that is stored on a mention.
#' For instance, you can add and rmeove tags from mentions.
#'
#' @param x The account object.
#' @param filter Defines the mentions to update.
#' @param auto.confirm Some updates, such as updating sentiment, can badly effect an account.
#'                     This function will usually ask if you want to proceed. Setting this to
#'                     TRUE will turn this behaviour off.
#' @param ... Arguments for other methods.
#'
#' @return Nothing
#' @export
#' @author Constance Neeser
update_mentions <- function(x, filter, auto.confirm = FALSE, ...) {
  UseMethod("update_mentions")
}

#' Updates mentions for an account object
#'
#' @describeIn update_mentions
#'
#' @param relevancy A string indicating whether mentions are relevant or not.
#' @param relevancyVerified A boolean indicating whether to consider the relevancy as verifed or not.
#' @param addTags A list of tag IDs to add to the mention.
#' @param removeTags A list of tag IDs to remove.
#' @param sentiment An integer indicating sentiment.
#' @param location A string giving location information.
#' @param sentimentVerified A boolean indicating whether to consider sentiment as verified or not.
#' @param media A string indicating the category of the mentions, such as CONSUMER or PRESS
#' @param gender A string indicating the author's gender, such as MALE, FEMALE, OTHER, UNKNOWN.
#' @param language A two letter, lowercase string, giving the iso 639-1 language code for the mention.
#' @param race A string giving the race of the author
#' @param addPhrases A list of integer IDs for phrases to add to the mention
#' @param removePhrases A list of integer IDs for the phrases to remove from the mention
#' @param addCrowdJobs A list of integer IDs representing crowd jobs related to this mention, to be
#'                     added to the mention.
#' @param removeCrowdJobs A list of integer IDs representing crowd jobs to be removed from this mention.
#' @param updateAuthor Whether the stored author information related to this mention should be
#'                     updated with any of the information related to the author on this mention, such as race,
#'                     gender, language or media.
#'
#' @export
update_mentions.brandseyer2.account.v4 <- function(x, filter,
                                                   auto.confirm,
                                                   ...,
                                                   relevancy,
                                                   relevancyVerified,
                                                   addTags,
                                                   removeTags,
                                                   sentiment,
                                                   sentimentVerified,
                                                   media,
                                                   gender,
                                                   language,
                                                   race,
                                                   addPhrases,
                                                   removePhrases,
                                                   addCrowdJobs,
                                                   removeCrowdJobs,
                                                   updateAuthor,
                                                   location) {

  assert_that(is.string(filter))

  json = list(filter = jsonlite::unbox(filter))

  if (!missing(addTags)) {
    assert_that(is.numeric(addTags), msg = "Tag IDs must be integer")
    json <- c(json, list(addTags = tibble::tibble(id = addTags)))
  }

  if (!missing(removeTags)) {
    assert_that(is.numeric(removeTags), msg = "Tag IDs must be integer")
    json <- c(json, list(removeTags = tibble::tibble(id = removeTags)))
  }

  if (!missing(sentiment)) {
    assert_that(is.numeric(sentiment), msg = "Sentiment must be an integer")
    if (!auto.confirm && !confirm("This could damage a lot of data in an account.")) {
      rlang::abort("Action cancelled by the user")
    }
    json <- c(json, list(sentiment = jsonlite::unbox(sentiment)))
  }

  if (!missing(sentimentVerified)) {
    assert_that(is.logical(sentimentVerified))
    json <- c(json, list(sentimentVerified = jsonlite::unbox(sentimentVerified)))
  }

  if (!missing(media)) {
    assert_that(is.string(media))
    json <- c(json, list(category = jsonlite::unbox(media)))
  }

  if (!missing(gender)) {
    assert_that(is.string(gender))
    json <- c(json, list(gender = jsonlite::unbox(gender)))
  }

  if (!missing(language)) {
    assert_that(is.string(language))
    json <- c(json, list(language = jsonlite::unbox(language)))
  }

  if (!missing(race)) {
    assert_that(is.string(race))
    json <- c(json, list(race = jsonlite::unbox(race)))
  }

  if (!missing(relevancy)) {
    assert_that(is.string(relevancy))
    json <- c(json, list(relevancy = jsonlite::unbox(relevancy)))
  }

  if (!missing(relevancyVerified)) {
    assert_that(is.logical(relevancyVerified))
    json <- c(json, list(relevancyVerified = jsonlite::unbox(relevancyVerified)))
  }

  if (!missing(addPhrases)) {
    assert_that(is.numeric(addPhrases))
    json <- c(json, list(addPhrases = addPhrases))
  }

  if (!missing(removePhrases)) {
    assert_that(is.numeric(removePhrases))
    json <- c(json, list(removePhrases = removePhrases))
  }

  if (!missing(addCrowdJobs)) {
    assert_that(is.numeric(addCrowdJobs))
    json <- c(json, list(addCrowdJobs = addCrowdJobs))
  }

  if (!missing(removeCrowdJobs)) {
    assert_that(is.numeric(removeCrowdJobs))
    json <- c(json, list(removeCrowdJobs = removeCrowdJobs))
  }

  if (!missing(updateAuthor)) {
    assert_that(is.numeric(updateAuthor))
    json <- c(json, list(updateAuthor = jsonlite::unbox(updateAuthor)))
  }

  if (!missing(location)) {
    assert_that(is.string(location))
    json <- c(json, list(location = jsonlite::unbox(location)))
  }

  if (length(json) == 1) {
    rlang::abort("No update parameters have been supplied, such as tag or sentiment")
  }

  endpoint <- paste0("v4/accounts/", account_code(x), "/mentions")
  write_api(endpoint, "PUT", json)
  invisible()
}
