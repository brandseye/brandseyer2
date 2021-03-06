% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/account_account_code.R
\name{account_code}
\alias{account_code}
\title{Get an account's code}
\usage{
account_code(account)
}
\arguments{
\item{account}{The account object, or list of account objects.}
}
\value{
A string holding the account's code
}
\description{
Returns a vector of account codes.
}
\examples{

account("TEST01AA") \%>\%
  account_code()

account("TEST01AA", "TEST02AA") \%>\%
  account_code()
}
