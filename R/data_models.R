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


#' Explore the API data models
#' @name data_models
#'
#' @description Provides a table of values and the IDs that represent
#' them in the filter language. For instance, country names and the
#' codes used to represent them.
NULL

#' @rdname data_models
#' @export
#'
#' @details [data_model_countries()] supplies a tibble
#' of country names and their ISO 3166-1 Alpha 2 country
#' codes, as used in the filter language.
data_model_countries <- function() {
  read_api("/v4/data-model/countries") %>%
    map_df(~ tibble(id = .x$id, name = .x$name))
}

#' @rdname data_models
#' @export
#'
#' @details Our media types are listed in [data_model_categories()].
#' `ENTERPRISE` represents mentions coming from the brand itself
#' (i.e., the brand that the mention is matched against), while
#' `CONSUMER` are most other people. `PRESS` represents media sources.
data_model_categories <- function() {
  read_api("/v4/data-model/categories") %>%
    map_df(~ tibble(id = .x$id, name = .x$label))
}

#' @rdname data_models
#' @export
#'
#' @details Our various currencies are listed using [data_model_currencies()].
#' These are the 3-letter ISO 4217 codes.
data_model_currencies <- function() {
  read_api("/v4/data-model/currencies") %>%
    map_df(~ tibble(id = .x$id, name = .x$label))
}

#' @rdname data_models
#' @export
#'
#' @details We use 2-letter ISO 639-1 codes to represent
#' our languages, which are listed using [data_model_languages()].
data_model_languages <- function() {
  read_api("/v4/data-model/languages") %>%
    map_df(~ tibble(id = .x$id, name = .x$name))
}

#' @rdname data_models
#' @export
#'
#' @details The `socialNetwork` field in the
#' filter language takes IDs provided by [data_model_networks()].
data_model_networks <- function() {
  read_api("/v4/data-model/social-networks") %>%
    map_df(~ tibble(id = .x$id, name = .x$label))
}
