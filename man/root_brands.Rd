% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/account_root_brands.R
\name{root_brands}
\alias{root_brands}
\alias{root_brands.data.frame}
\alias{root_brands.brandseyer2.account}
\alias{root_brands.list}
\title{Find root brands in an account}
\usage{
root_brands(x, includeDeleted, includeArchived, short)

\method{root_brands}{data.frame}(x, includeDeleted = FALSE, includeArchived = FALSE, short = TRUE)

\method{root_brands}{brandseyer2.account}(x, includeDeleted = FALSE, includeArchived = FALSE, short = TRUE)

\method{root_brands}{list}(x, includeDeleted = FALSE, includeArchived = FALSE, short = TRUE)
}
\arguments{
\item{x}{The object to find root brands for.}

\item{includeDeleted}{Whether to include deleted root brands. By default, only
shows non-deleted brands.}

\item{includeArchived}{Whether to include archived root brands. By default, only
shows non-archived brands.}

\item{short}{Include only the most import bits of brand information, such as the name and ID.}
}
\value{
A tibble containing rows only for the root brands of an account. Otherwise,
like the table returned in \code{\link[=brands]{brands()}}.
}
\description{
Root brands are the brands around which we collect mentions, and
determine sentiment. Sentiment is always towards the brand.
}
\section{Methods (by class)}{
\itemize{
\item \code{data.frame}: Find root brands from a \code{tibble} or \code{data.frame}. The data.frame
should have  \code{id}, \code{parent}, and \code{deleted} columns, just as
the tibble returned from \code{\link[=brands]{brands()}} does.

\item \code{brandseyer2.account}: Returns root brand information from an account object.

\item \code{list}: Returns root brand information from a list of account objects.
}}

\examples{

# Get root brands for an account
account("TEST01AA") \%>\%
  root_brands()

# Get root brands from a tibble or data.frame
account(c("TEST01AA", "TEST02AA")) \%>\%
  brands(short = FALSE) \%>\%
  root_brands()

# Show fuller root brand information
account("TEST01AA") \%>\%
  root_brands(short = FALSE)
}
\seealso{
\code{\link[=brands]{brands()}} for returning all brands in an account.
}
\author{
Constance Neeser
}
