#' @title Predict Method for FM Fits
#'
#' @description
#' Obtains predictions from a fitted factorization machines model object.
#'
#' @usage
#' predict(object, newdata = NULL)
#'
#' @param object a fitted object of class inheriting from "fm".
#' @param newdata a data frame in which to look for variables with which to predict.
#'
predict.FM <- function (object, newdata = NULL, normalize = TRUE)
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

  pred <- FMPredict(newdata, normalize, object, fm.get_threads())

  pred
}
