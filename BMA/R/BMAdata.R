#' Data-like object to be used by the fitBMA() function
#' 
#' Objects of class \code{BMAdata} contain 2 slots, the first of which contains the values for the dependent variable and the second of which contains a matrix of values for the predictor variables.
#'
#' An object of the class `BMAdata' has the following slots:
#' \itemize{
#' \item \code{depvar} The added or subtracted squared values
#' \item \code{covariates} The added or subtracted squared values
#' }
#'
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname BMAdata-class
#' @export
setClass(Class="BMAdata",
         representation = representation(
           depvar = "numeric", 
           covariates = "matrix"
         ),    
         prototype = prototype(
           depvar = c(), 
           covariates = matrix()
         )
)

#' @export
setValidity("BMAdata", function(object){
  observations <- length(object@depvar)
  if(length(object@depvar)!=nrow(object@covariates)){
    return("Number of observations in dependent variable does not equal number of observations in independent variable(s)")
  }
})

#' @export
setMethod("initialize", "BMAdata", function(.Object, ...){
  value = callNextMethod()
  validObject(value)
  return(value)
})