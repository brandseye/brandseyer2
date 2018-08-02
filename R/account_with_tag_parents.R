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

#' Determine the parent of tags given a particular tree
#'
#' Adds a column called `parent` to a table. This is the ID
#' of a tag's parent tag. Since tags can have multiple parents,
#' we need to know what the ultimate root parent to determine
#' "parenthood" is. This is done using the `parent_id` parameter.
#'
#' @param tags A tibble of tags to find parents for
#' @param parent_id A root parent that all children are determined from
#'
#' @return The same tags tibble, but annotated with a `parent` column
#' @export
#'
#' @examples
#' # Add parents to tags
#' account("TEST01AA") %>%
#'   tags() %>%
#'   with_tag_parents(10)
#'
#' # See what root brands we have
#' account("TEST01AA") %>%
#'   topic_trees()
#'
#' # Tag 1001 is a topic tree. Let's find
#' # topics, with parents, that are part of this tree.
#' account("TEST01AA") %>%
#'   tags() %>%
#'   with_tag_parents(1001) %>%
#'   dplyr::filter(namespace == 'topic') %>%
#'   dplyr::arrange(desc(is_parent), name)
with_tag_parents <- function(tags, parent_id) {
  assert_that(is.data.frame(tags))
  assert_that(tags %has_name% 'id')
  assert_that(tags %has_name% 'children')
  assert_that(assertthat::is.count(parent_id))

  parent_children <- tags %>% filter(id == parent_id) %>% pluck("children", 1)

  if (rlang::is_empty(parent_children))
    rlang::abort(glue::glue("parent tag with id {parent_id} is not present in `tags` data.frame"))

  child_hash <-  new.env(hash = TRUE)

  walkChildren <- function(parent_id, parent_children) {
    parent_children %>%
      walk(function(child) {
        assign(make.names(child), parent_id, envir = child_hash)
        child_children <- tags %>% filter(id == child) %>% pluck("children", 1)
        if (!rlang::is_empty(child_children))
          walkChildren(child, child_children)
      })
  }

  walkChildren(parent_id, parent_children)

  tags %>%
    rowwise() %>%
    mutate(parent = mget(make.names(id), envir = child_hash, ifnotfound = NA)[[1]])
}
