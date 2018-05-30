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


#' Add tags to an account
#'
#' This will add a new tag to an account. This tag will be added
#' in the tag namespace. If a tag with the same name already exists,
#' this will do nothing. If a tag with the same name exists, and is deleted,
#' it will create a new tag with a new ID.
#'
#' @param x An account to add the tag to.
#' @param name A vector of tag names.
#' @param description An optional description for each tag. If given, must be
#'                    the same length as \code{name}.
#'
#' @return A tibble containing the new tag's details, including the tag's ID.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Create one tag
#' account("TEST01AA") %>%
#'   create_tags("tag1")
#'
#' # Create a tag with a description
#' account("TEST01AA") %>%
#'   create_tags("tag1", "A description")
#'
#' # Create multiple tags
#' account("TEST01AA") %>%
#'   create_tags(c("tag1", "tag2"))
#'
#' # Create multiple tags, with descriptions
#' account("TEST01AA") %>%
#'   create_tags(c("tag1", "tag2"),
#'               c("description 1", "description2"))
#'
#' }
create_tags <- function(x, name, description) {
  UseMethod("create_tags")
}


#' @export
create_tags.brandseyer2.account <- function(x, name, description) {
  assert_that(is.character(name))
  assert_that(missing(description) || is.character(description))

  json <- NULL
  if (missing(description)){
    json <- map(name, ~ list(name = jsonlite::unbox(.x)))
  } else {
    assert_that(length(name) == length(description),
                msg = "`description` is not the same length as `name` ")
    json <- map2(name, description, ~list(name = jsonlite::unbox(.x),
                                          description = jsonlite::unbox(.y)))
  }

  data <- write_mash(paste0("rest/accounts/", account_code(x), "/tags"),
                     method = "POST",
                     json = json)

  data %>%
    map_df(~tibble(
      id = .x$id,
      name = .x$name,
      description = .x$description %||% NA,
      namespace = .x$namespace
    ))

}
