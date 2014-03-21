\name{BMA-package}
\alias{BMA-package}
\alias{BMA}
\docType{package}
\title{
Bayesian Model Averaging
}
\description{
Conduct Bayesian Model Averaging (BMA) on a user-supplied dataset. Runs multiple linear (OLS) regressions for every possible combination of user-supplied predictor variables, then returns the estimated coefficients and R^2 values from each of these models, along with the posterior model    odds, posterior expected values, and posterior probabilities after performing Bayesian Model Averaging. Also includes modifications of the summary and plot functions so they can plot BMA-type objects.
}
\details{
\tabular{ll}{
Package: \tab BMA\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-03-20\cr
License: \tab GPL (>= 2)\cr
}
To use the package, begin by creating an object of class BMAdata. You can do this by using the new() function and writing something like the following: object.name <- new(Class="BMAdata", depvar=[VECTOR OF DEPENDENT VARIABLE DATA], covariates=[MATRIX OF INDEPENDENT VARIABLES]). Obviously you'll have to assign this to a new object in R. Then use the function fitBMA(object.name) to get the results. You can also just write plot(object.name) or summary(object.name) to get default plots or summaries of the BMA results. 
}
\author{
Thomas Carroll
Maintainer: Thomas Carroll <thomasscarroll89@gmail.com>
}
\references{
Hoeting, Jennifer et al. 1999. "Bayesian Model Averaging: A Tutorial"
Montgomery, Jacob and Brendan Nyhan. 2010. "Bayesian Model Averaging: Theoretical Developments and Practical Applications"
}
\keyword{ package }
\examples{
X1 <- rnorm(100, 100, 50)
X2 <- rnorm(100, 50, 10)
X3 <- rnorm(100, 75, 30)
Y <- 12 + 3*X1 - 6*X2 + 2*X3  + rnorm(100, 10, 5)
test <- new("BMAdata", depvar=Y, covariates=cbind(X1, X2, X3))
fitBMA(test)}