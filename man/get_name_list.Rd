% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{get_name_list}
\alias{get_name_list}
\title{Turn a string creating a vector in to a string of names}
\usage{
get_name_list(names, env = parent.frame())
}
\arguments{
\item{names}{A string generated using \code{deparse} and \code{substitute}.}

\item{env}{The environment in which to perform substitutions.}
}
\value{
A vector of characters.
}
\description{
Takes a string generated by deparse and returns a list of names
contained within.
}
\examples{

\dontrun{
get_name_list(deparse(substitute(one)))
get_name_list(deparse(substitute(c(one, two))))
get_name_list(deparse(substitute(NULL)))
}

}
