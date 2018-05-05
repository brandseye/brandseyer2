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

account_brands <- function(account) {
  UseMethod("account_brands")
}

account_brands.brandseyer2.account <- function(account) {
  
  recurse <- function(brands, parent = NA) {
    parents <- brands %>%
      map_df(function(brand) {
        
        tibble(
          id = brand$id,
          parent = parent,
          name = brand$name,
          tier = brand$tier %||% NA,
          schema = brand$schema %||% NA,
          filter = brand$mentionFilter %||% NA,
          sentimentRate = brand$crowdSamplePercentage %||% NA,
          topicRate = brand$crowdTopicPercentage %||% NA
        )
      })
    
    bind_rows(parents)
  }
  
  recurse(account$brands)
}