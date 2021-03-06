% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/account_list.R
\name{account_list}
\alias{account_list}
\title{List all accounts available to you}
\usage{
account_list(includeInactive = FALSE, client = NULL)
}
\arguments{
\item{includeInactive}{Set to \code{TRUE} if you would like inactive accounts to also be returned.}

\item{client}{Optional. A character vector of client codes to find the accounts for those clients only.}
}
\value{
A tibble of accounts available to you.
}
\description{
Provides a tibble of all BrandsEye accounts that are available to you. By default,
this will show only active accounts.
}
\details{
By default this returns only active accounts. You can use the
\code{includeInactive} parameter to change this behaviour

The returned tibble can be passed to some other functions, such as \code{\link[=account]{account()}},
to return information from across all accounts available to you.
}
