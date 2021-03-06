% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov_center.R
\name{cov_center}
\alias{cov_center}
\title{Function to compute the shape (covarince) and center of an ellipsoid model}
\usage{
cov_center(data, mve = TRUE, level, vars = NULL)
}
\arguments{
\item{data}{A data.frame or a matrix with the numeric values of the variables
that will be used to model the niche.}

\item{mve}{A logical value. If TRUE a minimum volume ellipsoid will be computed using
the function \code{\link[MASS]{cov.mve}} of the \pkg{MASS} package. If False the covariance matrix of the input data will be used.}

\item{level}{A numerical value specifying the proportion of the data to be
used to compute the ellipsoid.}

\item{vars}{A numeric vector specifying the columns indexes of the variables of the input data
which will be used to fit the ellipsoid model. If NULL the user will be asked to enter the indexes
interactively}
}
\value{
Returns a list containing the centroid of the ellipsoid, the covariance matrix based on
the input data and the input data.
}
\description{
Function to compute covariance and the center of an
ellipsoid model using the values of the niche variables of the ocurrences points
}
\examples{
## Load niche data
# d_cardon <-  read.csv(system.file("extdata", "cardon_virtual.csv", package = "nichetoolbox"))
## Look the data
# head(d_cardon)
## Compute the centroid and shape (covariance matrix) of the ellipsoid model.
## The user will be asked to enter the variables indexes
#covar_centroid <- cov_center(d_cardon,mve=TRUE,level=0.99,vars=NULL)
## Alternatively you can specify the columns indexes of the variables
# covar_centroid <- cov_center(d_cardon,mve=TRUE,level=0.99,vars=c(3,4,5))

#+++++++++++++++++++++++++++++ Check the output +++++++++++++++++++++++++++++#
## ellipsoid center
# covar_centroid$centroid
## ellipsoid shape
# covar_centroid$covariance
## Data input
# head(covar_centroid$data)
}

