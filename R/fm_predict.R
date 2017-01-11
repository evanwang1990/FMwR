#' @title Predict Method for FM Fits
#'
#' @description
#' Obtains predictions from a fitted factorization machines model object.
#'
#' @usage
#' predict(object, newdata = NULL, max_threads = 1)
#'
#' @param object a fitted object of class inheriting from "fm".
#' @param newdata a data frame in which to look for variables with which to predict.
#' @param max_threads an integer, number of threads to speed up computing, \strong{openmp} should be supported.
predict.FM <- function (object, newdata = NULL, normalize = TRUE, max_threads = 1)
{
  if (is.null(newdata)) {
    stop("newdata is null")
  }
  if (class(newdata) != "fm.matrix") {
    stop("newdata must be a fm.matrix object")
  }
  if (any(is.na(newdata$features$value))) {
    stop("there are NAs in newdata")
  }

  if (normalize && is.null(object$Scales$mean)) {
    stop("can not normalize newdata because all the variables have not been normalized in FM model")
  }
  if (!normalize && !is.null(object$Scales$mean)) {
    warning("some variables in FM model are normalized, but those in newdata will not")
  }

  pred <- FMPredict(newdata, normalize, object, max_threads)

  pred
}
