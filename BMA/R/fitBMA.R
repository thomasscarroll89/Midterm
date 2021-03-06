#' Bayesian Model Averaging
#' 
#' Takes objects of class \code{BMAdata} (or any of its subclasses) and runs an OLS regression for every possible combination of variables, and returns a matrix of coefficient estimates for each model, as well as the R^2 value for each model. It then runs Bayesian Model Averaging on this dataset and returns the posterior model odds, along with the posterior expected values and probability of being non-zero for each of the predictor variables supplied by the user. 
#' 
#' @param BMAdata An object of class BMAdata, which must first be created using the "new" function. 
#' @param g A user-supplied single numerical value. 
#' @param parallel Dummy telling R whether to run the code in parallel or not; the section of the code that runs all of the linear models can be run in parallel. By default this is set to FALSE. User must still load relevant libraries (e.g. doMC, multicore, foreach, etc.) and specify the number of cores before running the function.
#' 
#' @return A list containing 4 elements. The first element is a matrix of coefficient estimates for each model that was run, along with the R^2 value for each of these models. The second element is a vector (really a matrix, but only one row) containing the posterior model odds for each of the OLS models that were run. The third element is also a one-row matrix containing the posterior expected values (e.g. coefficient estimates) that were derived using Bayesian Model Averaging. Finally the fourth element is a one-row matrix containing the posterior probability that each of the coefficient estimates is non-zero. 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname fitBMA
#' @examples X1 <- rnorm(100, 100, 50) 
#' X2 <- rnorm(100, 50, 10) 
#' X3 <- rnorm(100, 75, 30) 
#' Y <- 12 + 3*X1 - 6*X2 + 2*X3  + rnorm(100, 10, 5) 
#' test <- new("BMAdata", depvar=Y, covariates=cbind(X1, X2, X3)) 
#' fitBMA(test)
#' @export
setGeneric("fitBMA", function(object="BMAdata", g=3, parallel=FALSE){
  standardGeneric("fitBMA")
})

#' @export
setMethod("fitBMA", "BMAdata", function(object="BMAdata", g=3, parallel=FALSE){
  library(plyr)
  k <- ncol(object@covariates) #Let k be the number of predictor variables (not including intercept)
  #FIRST we need to standardize the variables. I first create a generic function that will standardize a vector that is plugged into it. 
  standardize <- function(x){ #Let x be a vector of length greater than 1
    (x - mean(x))/sd(x)
  }
  y.stand <- standardize(object@depvar)
  x.stand <- apply(object@covariates, 2, standardize)
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
    #3.1 First I create a function that will calculate a bunch of linear regressions. This makes use of the list of all possible model combinations created in step 2 above
    my.test <- function(x){
      #The x argument is just a matrix; in this case the function was designed to work on the list-like object model.combinations created above, so x is just one element of the list model.combinations
      output <- matrix(NA, nrow=k+1, ncol=ncol(x))
      count <- 0
      for(i in 1:ncol(x)){
        count <- count + 1
        variable.numbers <- x[,i]
        model <- lm(y.stand ~ x.stand[,c(variable.numbers)] - 1)
        output[c(variable.numbers),count] <- model$coef
        output[c(k+1),count] <- summary(model)$r.squared
      }
      return(output)
    }
    try.this <- llply(model.combinations, .fun=my.test, .parallel=parallel) #returns a list containing the coefficient estimates and R^2 values for all the models
    #Next we have to convert the above list into a single matrix
    try.this <- matrix(unlist(try.this), nrow=k+1, ncol=q)
    rownames(try.this) <- c(colnames(x.stand), "R^2")
    colnames(try.this) <- paste(rep("Model", length=q), 1:q, sep=" ")
    #3.2 Next we clean up the output a little bit by doing some rounding and replacing NAs with empty character strings. 
    output.1 <- as.data.frame(try.this)
    output.1 <- round(output.1, 4)
    output.1[is.na(output.1)] <- "" # So output.1 is a matrix of coefficient estimates and R^2 values for each model
    final.output <- list(output.1)
    names(final.output) <- c("Coefficients.and.R(^2)")
  
  #FOURTH, we need to calculate the posterior model odds for each model. Here we make use of the formula
    #provided in the slides.
  B.Mk.M0 <- numeric(length=q) #this will be the vector that we store the values of B[M_{k}:M_{0}] into for each model. See slide 25
  n <- nrow(x.stand) #let n be the number of observations
  for(i in 1:q){
    p.sub.k <- sum(which(output.1[,i]!="")) - 1 #this represents number of independent variables in a given model. We subtract one because the R^2 value is recorded in output.1 and we don't want to count that as an independent variable
    r.squared.k <- as.numeric(output.1[k+1,i]) #let this represent the R^2 value in a given model
    B.Mk.M0[i] <- ((1 + g)^((n - p.sub.k - 1)/2))*((1 + (g*(1 - r.squared.k)))^(-((n - 1)/2)))
  }
  posterior.model.odds <- matrix(B.Mk.M0/sum(B.Mk.M0), nrow=1) #posterior.model.odds is p(M_{k}|Y) on slide 10
  colnames(posterior.model.odds) <-  paste(rep("Model", length=q), 1:q, sep=" ") 
  final.output[["posterior.model.odds"]] <- posterior.model.odds
  
  #FIFTH, we need to calculate the posterior expected values for each coefficient. We use the formula on slide 26
  expected.betas <- (g/(g + 1))*as.matrix(sapply(output.1[-c(k+1),], as.numeric)) #this returns a matrix containing the (weighted) coefficient estimates for each model
  posterior.expected.value <- numeric(length=k)
  for(i in 1:k){
    model.numbers <- which(output.1[i,]!="") #calculates which models a variable appears in
    posterior.expected.value[i] <- sum(posterior.model.odds[model.numbers] * expected.betas[i,model.numbers])
  }
  posterior.expected.value <- matrix(posterior.expected.value, nrow=1)
  colnames(posterior.expected.value) <- paste(rep("X", k), 1:k, sep="")
  final.output[["posterior.expected.values"]] <- posterior.expected.value
  
  #SIXTH, we need to calculate the posterior probability that each coefficient is non-zero, which we get by adding up
  #all weight assigned to each model that includes that coefficient (i.e. the posterior.model.odds). 
  posterior.probability <- numeric(length=k)
  for(i in 1:k){
    model.numbers <- which(output.1[i,]!="")
    posterior.probability[i] <- sum(posterior.model.odds[model.numbers])
  }
  posterior.probability <- matrix(posterior.probability, nrow=1)
  colnames(posterior.probability) <- paste(rep("X", k), 1:k, sep="")
  final.output[["posterior.probability"]] <- posterior.probability
  return(final.output)
})