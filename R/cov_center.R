#' Function to compute the shape (covarince) and center of an ellipsoid model
#' @description Function to compute covariance and the center of an
#' ellipsoid model using the values of the niche variables of the ocurrences points
#' @param data A data.frame or a matrix with the numeric values of the variables
#' that will be used to model the niche.
#' @param mve A logical value. If TRUE a minimum volume ellipsoid will be computed using
#' the function \code{\link[MASS]{cov.mve}} of the \pkg{MASS} package. If False the covariance matrix of the input data will be used.
#' @param level A numerical value specifying the proportion of the data to be
#' used to compute the ellipsoid.
#' @param vars A numeric vector specifying the columns indexes of the variables of the input data
#' which will be used to fit the ellipsoid model. If NULL the user will be asked to enter the indexes
#' interactively
#' @return Returns a list containing the centroid of the ellipsoid, the covariance matrix based on
#' the input data and the input data.
#' @export
#' @examples
#' ## Load niche data
#' # d_cardon <-  read.csv(system.file("extdata", "cardon_virtual.csv", package = "nichetoolbox"))
#' ## Look the data
#' # head(d_cardon)
#' ## Compute the centroid and shape (covariance matrix) of the ellipsoid model.
#' ## The user will be asked to enter the variables indexes
#' #covar_centroid <- cov_center(d_cardon,mve=TRUE,level=0.99,vars=NULL)
#' ## Alternatively you can specify the columns indexes of the variables
#' # covar_centroid <- cov_center(d_cardon,mve=TRUE,level=0.99,vars=c(3,4,5))
#'
#' #+++++++++++++++++++++++++++++ Check the output +++++++++++++++++++++++++++++#
#' ## ellipsoid center
#' # covar_centroid$centroid
#' ## ellipsoid shape
#' # covar_centroid$covariance
#' ## Data input
#' # head(covar_centroid$data)
cov_center <- function(data,mve=TRUE,level,vars=NULL){

  if(is.null(vars)){

    nvars <- readline(prompt="Number of variables to fit the ellipsoid model:\n\n")
    data <- data.frame(data)

    allvars <- names(data)
    print(nvars)
    vars <- numeric(nvars)
    cat('Select a variable form the list:\n\n')
    for (i in 1:dim(data)[2]){
      cat(i,'.-',allvars[i],'\n')

    }
    cont <- 1
    while(0 %in% vars){
      n <- readline(prompt="Enter an option from the above list: ")

      if(n %in% 1:dim(data)[2]){
        vars[cont] <- as.numeric(n)
        cont <- cont+1
      }
      else{
        cat('Option not in the list:\n')
      }

    }
  }
  data <- data[,vars]

  if(mve){

    # Compute the number of points of our data that represent the proportion of the data
    NDquntil <- function(nD,level){
      n <- round(nD*level)
      if(n > nD)
        n <- nD
      return(n)
    }

    n <-NDquntil(dim(data)[1],level)
    # Centroid and covarianve for the Minimum volume Ellipsoid
    cent_var <- cov.mve(data,quantile.used =n)
    centroid <- cent_var$center
    vari <- cent_var$cov
  }
  else{
    centroid <- colMeans(data)
    vari <- cov(data)
  }
  return(list(centroid=centroid,covariance=vari,data=data))

}
