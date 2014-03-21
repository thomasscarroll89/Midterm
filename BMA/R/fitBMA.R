#' Bayesian Model Averaging
#' 
#' Takes objects of class \code{BMAdata} (or any of its subclasses) and runs an OLS regression for every possible combination of variables, and returns a matrix of coefficient estimates for each model, as well as the R^2 value for each model. 
#' 
#' @param BMAdata An object of class BMAdata, which must first be created using the "new" function. 
#' 
#' @return A matrix of coefficient estimates for each model that was run, along with the R^2 value for each of these models. 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname fitBMA
#' @export
setGeneric("fitBMA", function(object="BMAdata"){
  standardGeneric("fitBMA")
})

#' @export
setMethod("fitBMA", "BMAdata", function(object="BMAdata"){
  k <- ncol(object@covariates) #Let k be the number of predictor variables (not including intercept)
  colnames(object@covariates) <- paste(rep("X", length=k), 1:k, sep="")
  model.combinations <- vector(mode="list") #Create object "model.combinations", which is a list containing all of the different combinations of variables 
  number.of.models <- c(1) #do 1, for the model with just an intercept
  for(i in 1:k){
    model.combinations[[i]] <- combn(c(1:k), m=i)
    number.of.models <- append(number.of.models, values=ncol(model.combinations[[i]]), after=length(number.of.models))
  }
  #CREATE basic structure of the output matrix
  output2 <- matrix(NA, nrow=k+2, ncol=sum(number.of.models))
  rownames(output2) <- c("Intercept", colnames(object@covariates), "R^2")
  colnames(output2) <- paste(rep("Model", length=sum(number.of.models)), 1:sum(number.of.models), sep=" ")
  #BEGIN running linear models; start with model using only an intercept
  model.base <- lm(object@depvar ~ 1)
  model.base.coef <- model.base$coef
  output2[1,1] <- model.base.coef
  r.squared <- summary(model.base)$r.squared  
  count <- 1
  for(i in 1:length(model.combinations)){
    for(j in 1:ncol(model.combinations[[i]])){
      count <- count + 1
      variable.numbers <- model.combinations[[i]][,j]
      model <- lm(object@depvar ~ object@covariates[,c(variable.numbers)])
      coefficients <- model$coef
      output2[c(1, variable.numbers + 1),count] <- coefficients
      r.squared <- append(r.squared, values=summary(model)$r.squared, after=length(r.squared))
    }
  }
  output2[k+2,] <- r.squared  
  output <- as.data.frame(output2)
  output <- round(output, 4)
  output[is.na(output)] <- ""
  return(output)
})