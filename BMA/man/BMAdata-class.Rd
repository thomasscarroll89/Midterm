\docType{class}
\name{BMAdata-class}
\alias{BMAdata-class}
\title{Data-like object to be used by the fitBMA() function}
\description{
Objects of class \code{BMAdata} contain 2 slots, the first
of which contains the values for the dependent variable and
the second of which contains a matrix of values for the
predictor variables.
}
\details{
An object of the class `BMAdata' has the following slots:
\itemize{ \item \code{depvar} The added or subtracted
squared values \item \code{covariates} The added or
subtracted squared values }
}
\examples{
X1 <- rnorm(100, 100, 50)
X2 <- rnorm(100, 50, 10)
X3 <- rnorm(100, 75, 30)
Y <- 12 + 3*X1 - 6*X2 + 2*X3  + rnorm(100, 10, 5)
test <- new("BMAdata", depvar=Y, covariates=cbind(X1, X2, X3))
}
\author{
Thomas Carroll: \email{thomasscarroll89
}

