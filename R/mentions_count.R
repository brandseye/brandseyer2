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

# count_mentions <- function(x, filter, ...) {
#   UseMethod("count_mentions")
# }
#
# count_mentions.brandseyer2.account.v4 <- function(x, filter, ...,
#                                                   groupBy = NULL) {
#
#   assert_that(!missing(filter) && is.string(filter),
#               msg = "A filter must be provided")
#
#   query <- list(filter = filter, groupBy = groupBy)
#
#   data <- read_api(endpoint = paste0("v4/accounts/",account_code(x), "/mentions/count"),
#                    query = query)
#
#
#
#   data %>% map_df(function(row) {
#     as.tibble(row %>% imap(function(data, index) {
#       if (index %in% c("published")) {
#         data <- if (nchar(data) > 10) lubridate::ymd_hm(data) else lubridate::ymd(data)
#       }
#       d <- list()
#       if (is.atomic(data)) d[[index]] = data
#       else {
#         d[[paste0(index, ".id")]] = data$id
#         d[[index]] = list(as.tibble(data))
#       }
#       d
#     }) %>% purrr::flatten())
#   })
#
# }
