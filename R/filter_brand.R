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

filter_brand <- function(id, code = NULL, name = NULL) {
  assert_that(assertthat::is.count(id))

  structure(
    list(id = id,
         code = code,
         name = name),
    class = "brandseyer2.brand"
  )
}

filter_brand_from_df <- function(account, df) {
  # For devtools::check
  . <- NULL; name <- NULL;

  if (rlang::is_empty(df) || nrow(df) == 0) return(list())
  df %>%
    rowwise() %>%
    mutate(brand = list(filter_brand(id = id, code = account, name = name))) %>%
    .$brand
}


format.brandseyer2.brand <- function(x, colour = TRUE, ...) {
  code <- if (is.null(x$code)) "" else glue::glue("{x$code}:")
  name <- if (is.null(x$name)) "" else glue::glue("[{x$name}]")
  if (colour) {
    code = crayon::silver(code)
    name = crayon::silver(name)
  }
  glue::glue("{code}{x$id}{name}")
}

print.brandseyer2.brand <- function(x, ...) {
  cat(format(x), "\n")
}
