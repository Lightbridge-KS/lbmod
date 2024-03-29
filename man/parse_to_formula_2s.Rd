% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula.R
\name{parse_to_formula_2s}
\alias{parse_to_formula_2s}
\title{Parse Strings to LHS and RHS of formula}
\usage{
parse_to_formula_2s(fun = "lm", lhs, rhs, args = list(), pkg = "stats")
}
\arguments{
\item{fun}{Function to call}

\item{lhs}{(Symbol) left-hand sided of the formula}

\item{rhs}{(Symbol) right-hand sided of the formula}

\item{args}{A list of arguments to \code{fun}}

\item{pkg}{Package to call \code{fun}}
}
\value{
object as return by \code{fun}
}
\description{
Most generalized approach to parse formula
}
\examples{
parse_to_formula_2s(fun = "lm", "Sepal.Length", "Species", pkg = "stats", args = list(data = iris))
}
