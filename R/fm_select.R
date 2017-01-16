#' @title Select Best Model
#'
#' @aliases fm.select.FM
#'
#' @description select the best model according to the performance on test data
#'
#' @param object a FM object created by `fm.train`, which must contain \strong{Trace}
#' element, that's to say, \strong{step_size} should be greater than 0 in \code{track.control}
#'
#' @param trace a FMTrace object
#'
#' @param best.iter an integer, iteration number of trace.if \strong{best.iter} is missing
#' it will select the best model automatically
#'
#' @param  drop.trace whether to delete \strong{Trace} from model
#'
#' @usage fm.select(object, trace = NULL, best.iter = NULL, drop.trace = FALSE)
fm.select <- function(...) {
  UseMethod("fm.select")
}

fm.select.FM <- function(object, trace = NULL, best.iter = NULL, drop.trace = FALSE) {
  if (is.null(trace) || missing(trace)) {
    stop("trace is missing")
  }
  if (class(trace) != "FMTrace") {
    stop("trace is not a FMTrace object")
  }
  if (is.null(object$Trace) || length(object$Trace) <= 1) {
    stop("the Trace part in object have been dropped")
  }
  compare <- cmp(attr(object$Model, "track.control")$evaluate.metric)
  compares <- ifelse(identical(compare, `<`), `min`, `max`)
  iterations <- object$Trace$trace[[1]]
  iter_num <- length(iterations)
  if (!missing(best.iter) && !is.null(best.iter)) {
    if (best.iter < iterations[1] || best.iter > iterations[iter_num]) {
      stop("best.iter is out of range")
    }
    if (!best.iter %in% iterations) {
      iter_1 <- which.min(best.iter >= iterations)
      if (iter_1 < iter_num)
        best.iter <- ifelse(compare(trace$trace.test[iter_1], trace$trace.test[iter_1+1]), iter_1, iter_1+1)
      else
        best.iter <- iter_1
    } else {
      best.iter <- which(iterations == best.iter)
    }
  } else {
    best.trace.test <- compares(trace$trace.test)
    best.iter <- which(trace$trace.test == best.trace.test)
    if (length(best.iter) > 1) {
      best.trace.train <- compares(trace$trace.train[best.iter])
      best.iter <- which(trace$trace.train == best.trace.train)[1]
    }
  }

  params <- object$Trace$trace[[best.iter+1]]
  if (!is.null(object$Model$w0)) object$Model$w0 <- params$w0
  if (!is.null(object$Model$w)) object$Model$w <- params$w
  if (!is.null(object$Model$v)) object$Model$v <- params$v

  if (drop.trace) {
    object$Trace <- NULL
  }
  object
}

cmp <- function(evaludate.metric)
{
  if (evaludate.metric %in% c("LL", "ACC", "AUC")) {
    cmp <- `>`
  } else {
    cmp <- `<`
  }
  cmp
}
