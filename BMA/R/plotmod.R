#' Plot BMAdata Objects
#' 
#' Modifies the "plot" function to take objects of class \code{BMAdata} (or any of its subclasses). It plots a barplot of the expected coefficient values and a barplot of the posterior probability that the coefficients are non-zero.
#' 
#' @param BMAdata An object of class BMAdata
#' 
#' @author Thomas Carroll: \email{thomasscarroll89@gmail.com}
#' @rdname plot
#' @export
setMethod("plot", 
          signature(x="BMAdata", y="missing"), #IMPORTANT NOTE: the arguments in signature must match the arguments in the original function
          function(x, y, ...){
            temp <- fitBMA(x)
            par(mfrow=c(2, 1))
            barplot(height=temp$posterior.expected.values, main="Coefficient Posterior Expected Values", 
                    col=c("darkblue"), ylab="Expected Coefficient Value", 
                    ylim=c(min(temp$posterior.expected.values) - .1*min(temp$posterior.expected.values), max(temp$posterior.expected.values) + .1*max(temp$posterior.expected.values)))
            abline(h=0)
            barplot(height=temp$posterior.probability, main="Coefficient Posterior Probability", 
                    col=c("darkblue"), ylab="Posterior Probability Coefficient is Non-Zero", 
                    ylim=c(min(temp$posterior.probability) - .1*min(temp$posterior.probability), max(temp$posterior.probability) + .1*max(temp$posterior.probability)))
            abline(h=0)
            abline(h=0.05, col="grey", lty=2)
          }
)
getMethod("plot", signature=c("BMAdata", "missing"))