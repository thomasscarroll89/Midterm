\name{fitBMA}
\alias{fitBMA}
\title{Bayesian Model Averaging}
\usage{
fitBMA(object = "BMAdata", g = 3)
}
\arguments{
  \item{BMAdata}{An object of class BMAdata, which must
  first be created using the "new" function.}
}
\value{
A matrix of coefficient estimates for each model that was
run, along with the R^2 value for each of these models.
}
\description{
Takes objects of class \code{BMAdata} (or any of its
subclasses) and runs an OLS regression for every possible
combination of variables, and returns a matrix of
coefficient estimates for each model, as well as the R^2
value for each model.
}
\author{
Thomas Carroll: \email{thomasscarroll89
}

