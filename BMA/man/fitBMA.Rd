\name{fitBMA}
\alias{fitBMA}
\title{Bayesian Model Averaging}
\usage{
fitBMA(object = "BMAdata", g = 3)
}
\arguments{
  \item{BMAdata}{An object of class BMAdata, which must
  first be created using the "new" function.}

  \item{g}{A user-supplied single numerical value.}
}
\value{
A list containing 4 elements. The first element is a matrix
of coefficient estimates for each model that was run, along
with the R^2 value for each of these models. The second
element is a vector (really a matrix, but only one row)
containing the posterior model odds for each of the OLS
models that were run. The third element is also a one-row
matrix containing the posterior expected values (e.g.
coefficient estimates) that were derived using Bayesian
Model Averaging. Finally the fourth element is a one-row
matrix containing the posterior probability that each of
the coefficient estimates is non-zero.
}
\description{
Takes objects of class \code{BMAdata} (or any of its
subclasses) and runs an OLS regression for every possible
combination of variables, and returns a matrix of
coefficient estimates for each model, as well as the R^2
value for each model. It then runs Bayesian Model Averaging
on this dataset and returns the posterior model odds, along
with the posterior expected values and probability of being
non-zero for each of the predictor variables supplied by
the user.
}
\examples{
X1 <- rnorm(100, 100, 50)
X2 <- rnorm(100, 50, 10)
X3 <- rnorm(100, 75, 30)
Y <- 12 + 3*X1 - 6*X2 + 2*X3  + rnorm(100, 10, 5)
test <- new("BMAdata", depvar=Y, covariates=cbind(X1, X2, X3))
fitBMA(test)
}
\author{
Thomas Carroll: \email{thomasscarroll89
}

