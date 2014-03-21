\docType{methods}
\name{summary,BMAdata-method}
\alias{summary,BMAdata-method}
\title{Summarize BMAdata Objects}
\usage{
\S4method{summary}{BMAdata}(object, ...)
}
\arguments{
  \item{BMAdata}{An object of class BMAdata}

  \item{object}{an object for which a summary is desired.}

  \item{...}{additional arguments affecting the summary
  produced.}
}
\description{
Modifies the "summary" function to take objects of class
\code{BMAdata} (or any of its subclasses). It summarizes
the posterior expected values and probabilities (or being
non-zero) of the coefficients.
}
\author{
Thomas Carroll: \email{thomasscarroll89
}

