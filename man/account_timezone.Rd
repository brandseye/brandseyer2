% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/account_account_timezone.R
\name{account_timezone}
\alias{account_timezone}
\title{Get an account's timezone}
\usage{
account_timezone(account)
}
\arguments{
\item{account}{An account object, or list of account objects.}
}
\value{
A character vector of timezone that account data is reported in.
}
\description{
Returns a vector of timezones for the given accounts.
An account's data is reported in a particular timezone, set for that account.
All dates provided in filters are assumed to be given in that timezone.
}
\examples{

account("TEST01AA") \%>\%
  account_timezone()

account("TEST01AA", "TEST02AA") \%>\%
  account_timezone()
}
\author{
Constance Neeser
}
