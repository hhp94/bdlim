

#' Model comparison for bdlim objects
#'
#' @param object An object of class bdlim4 obtained from the bdlim4 function.
#'
#' @return A vector of model probabilities.
#' @export

modelcompare <- function(object){

  #  iterations to keep.
  iter_keep <- seq(object$nburn+1, object$nits, by=object$nthin)

  # make model probabilities
  modelselect <- table(apply(object$loglik[iter_keep,],1,which.max))
  names(modelselect) <- colnames(object$loglik)[as.numeric(names(modelselect))]

  # add in any groups missing due to 0 probability
  temp <- object$loglik[1,]
  temp[] <- 0
  temp[names(modelselect)] <- c(modelselect)
  temp <- unlist(temp/sum(temp))

  return(temp)
}
