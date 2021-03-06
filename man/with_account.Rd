% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_language.R
\name{with_account}
\alias{with_account}
\title{Add accounts to a query.}
\usage{
with_account(.query, account)
}
\arguments{
\item{.query}{A \code{\link[=query]{query()}} or \code{\link[=account]{account()}} to which to add this account.}

\item{account}{The \code{\link[=account]{account()}} to add.}
}
\value{
A \code{\link[=query]{query()}} object
}
\description{
\code{\link[=with_account]{with_account()}} lets you add additional accounts to a \code{\link[=query]{query()}}. It's
root brands and timezone are added, as appropriate. This is
part of the \code{\link[=query]{query()}} language.
}
\examples{

\dontrun{
account("TEST01AA") \%>\%
  filter_mentions("published inthelast week") \%>\%
  with_account("TEST03AA")

account("TEST01AA") \%>\%
  filter_mentions("published inthelast week") \%>\%
  with_account(account("TEST03AA"))
}
}
\seealso{
Other verbs for the query language: \code{\link[=group_mentions_by]{group_mentions_by()}},
\code{\link[=compare_mentions]{compare_mentions()}}, \code{\link[=with_mention_fields]{with_mention_fields()}}, \code{\link[=with_mention_order]{with_mention_order()}}

\code{\link[=query]{query()}} is a way to manually create queries.
}
