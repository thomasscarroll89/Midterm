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
  
  #FIRST we need to standardize the variables.
  y.stand <- (object@depvar - mean(object@depvar))/sd(object@depvar)
  x.stand <- matrix(NA, nrow=nrow(object@covariates), ncol=k)
  for(i in 1:k){
    x.stand[,i] <- (object@covariates[,i] - mean(object@covariates[,i]))/sd(object@covariates[,i])
  }
  colnames(x.stand) <- paste(rep("X", length=k), 1:k, sep="") #add variable names to keep track of the variables more easily
  
  #SECOND we run every possible combination of linear models (none including an intercept, since we standardized the variables)
    #2.1, we will calculate just how many linear models we need to run. The following for loop does this. 
    model.combinations <- vector(mode="list") #Create object "model.combinations", which is a list containing all of the different combinations of variables 
    number.of.models <- c(0) #Use 0 since we don't have any models where there's just an intercept
    for(i in 1:k){
      model.combinations[[i]] <- combn(c(1:k), m=i)
      number.of.models <- append(number.of.models, values=ncol(model.combinations[[i]]), after=length(number.of.models))
    }
    q <- sum(number.of.models) #q is just the total number of models we will need to run
    #2.2, we'll make a quick test to ensure that the number of models we are going to run is equal to how many models we should, theoretically, have
    q.test <- (2^k)-1
    if(q.test!=q){
      warning("Number of models being run does not equal how many models there should, theoretically, be")
    }

  #THIRD, we run the actual models.
    #3.1, we create the basic structure of the output matrix
    output2 <- matrix(NA, nrow=k+1, ncol=q)
    rownames(output2) <- c(colnames(x.stand), "R^2")
    colnames(output2) <- paste(rep("Model", length=q), 1:q, sep=" ")
    #3.2, we run linear models. The for loops run 2^k - 1 models, one at a time. 
    count <- 0  #The "count" variable keeps track of which model number we are currently on; it helps when we're plugging in coefficient
                # values and R squared values by basically telling us which column of the output matrix
                #to plug these values into
    for(i in 1:length(model.combinations)){
      for(j in 1:ncol(model.combinations[[i]])){
        count <- count + 1
        variable.numbers <- model.combinations[[i]][,j]
        model <- lm(y.stand ~ x.stand[,c(variable.numbers)] - 1)
        coefficients <- model$coef
        output2[c(variable.numbers),count] <- coefficients
        output2[c(k+1),count] <- summary(model)$r.squared
      }
    }
    #3.3 Next we clean up the output a little bit by doing some rounding and replacing NAs with empty character strings. 
    output <- as.data.frame(output2)
    output <- round(output, 4)
    output[is.na(output)] <- ""
    return(output)
  })