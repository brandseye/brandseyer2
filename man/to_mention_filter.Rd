% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_mention_filter.R
\name{to_mention_filter}
\alias{to_mention_filter}
\title{Returns a vector of filters for fetching mentions}
\usage{
to_mention_filter(query, code)
}
\arguments{
\item{query}{A \code{\link[=query]{query()}} object.}

\item{code}{The account we want filters for.}
}
\value{
A vector of characters. Returns NULL if there are no brands.
}
\description{
\code{\link[=to_mention_filter]{to_mention_filter()}} and \code{\link[=to_count_filter]{to_count_filter()}} converts a \code{\link[=query]{query()}} to a vector
of filters appropriate for either counting or fetching mentions. An account code
is specified to choose a filter for a specific account.
Each filter is required to perform all of the count operations or
to fetch all of the queried data.
}
\examples{

account("TEST01AA") \%>\%
  filter_mentions("published inthelast week") \%>\%
  to_mention_filter("TEST01AA")
}
\seealso{
Other query conversion functions: 
\code{\link{to_count_filter}()}
}
\concept{query conversion functions}
