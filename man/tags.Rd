% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/account_tags.R, R/mention_tags.R, R/tags.R
\name{tags.brandseyer2.account}
\alias{tags.brandseyer2.account}
\alias{tags.list}
\alias{tags.data.frame}
\alias{tags}
\title{Fetch tags for accounts and mention data}
\usage{
\method{tags}{brandseyer2.account}(x, ...)

\method{tags}{list}(x, ...)

\method{tags}{data.frame}(x, ..., ac = attr(x, "account"), na.rm = TRUE)

tags(x, ...)
}
\arguments{
\item{x}{An object to find tag information for, such as an \code{\link[=account]{account()}}
or a tibble of mention data.}

\item{...}{Extra arguments passed to functions.}

\item{ac}{An optional account object from which to take tag information.}

\item{na.rm}{Whether to keep mentions that have no tags.}
}
\value{
A tibble of tag information. Includes the name, namespace, and description of the
tags. Note that topics are stored in the 'topics' namespace.
}
\description{
Fetches tag information, returned as a tibble, for the given
account. This can be done for both account information, as well as
mentions.
}
\section{Methods (by class)}{
\itemize{
\item \code{brandseyer2.account}: Get tags from an account object.

\item \code{list}: Create a table of tags for the list of given accounts.

\item \code{data.frame}: Fetch tag information from a tibble of mention data.
}}

\examples{

# See what namespaces are in your account
account("TEST01AA") \%>\%
  tags() \%>\%
  dplyr::select(namespace) \%>\%
  table()

# Find topics
account("TEST01AA") \%>\%
  tags() \%>\%
  dplyr::filter(namespace == "topic")


# See the tags for multiple accounts at a time.
accounts(c("TEST01AA", "TEST02AA")) \%>\%
  tags()

# Finding the tags that mentions matched.
account("TEST01AA") \%>\%
  mentions("published inthelast week") \%>\%
  tags()
}
\seealso{
\url{account()} to fetch account data.

\url{topics()} to see just a list of topics.

\url{mentions()} to query mention data.
}
\author{
Constance Neeser
}
