#' Summarize BMAdata Objects
#' 
#' Modifies the "summary" function to take objects of class \code{BMAdata} (or any of its subclasses). It summarizes the posterior expected values and probabilities (or being non-zero) of the coefficients. 
#' 
#' @param BMAdata An object of class BMAdata
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname summary
#' @export
setMethod("summary", 
          signature(object="BMAdata"), #IMPORTANT NOTE: the arguments in signature must match the arguments in the original function
          function(object, ...){
            temp <- fitBMA(object)
            temp2 <- character(ncol(temp$posterior.probability))
            temp2.1 <- character(ncol(temp$posterior.probability))
            for(i in 1:ncol(temp$posterior.probability)){
              pp <- 1 - temp$posterior.probability[i]
              temp2[i] <- temp$posterior.expected.values[i]
              if(pp > 0.05){
                temp2.1[i] <- ""
              }
              if(pp <= 0.05 & pp > 0.01){
                temp2.1[i] <- "*"
              }
              if(pp <= 0.01 & pp > 0.001){
                temp2.1[i] <- "**"
              }
              if(pp <= 0.001){
                temp2.1[i] <- "***"
              }
            }
            temp2 <- substr(temp2, start=1, stop=8)
            out <- cbind(temp2, temp2.1)
           # row.names(out) <- paste(rep("X", ncol(temp$posterior.probability)), 1:ncol(temp$posterior.probability))
            colnames(out) <- c("Coefficient", "Significance")
            return(out)
          })