% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpret.r
\name{interpret}
\alias{interpret}
\title{Interpret lox statements}
\usage{
interpret(statements, locals, env = NULL)
}
\arguments{
\item{statements}{list from \code{parse_tokens}}

\item{locals}{hashmap from \code{resolver}}

\item{env}{global environment}
}
\description{
Interpret lox statements
}
